#[macro_use]
mod emit;
mod intermediate;
#[cfg(test)]
mod tests;

use self::emit::*;
use self::intermediate::*;
use Error;
use Level;
use common::{self, Lang, Outputs};
use inflector::Inflector;
use output::IndentedOutput;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Write;
use std::mem;
use std::path::PathBuf;
use syntax::ast;
use syntax::print::pprust;

const INDENT_WIDTH: usize = 4;

pub struct LangCSharp {
    using_decls: BTreeSet<String>,
    opaque_types: HashSet<String>,
    custom_decls: Vec<String>,
    ignored_functions: HashSet<String>,
    extract_comments: bool,

    context: Context,

    consts: Vec<Snippet<Const>>,
    enums: Vec<Snippet<Enum>>,
    structs: Vec<Snippet<Struct>>,
    functions: Vec<Snippet<Function>>,

    aliases: HashMap<String, Type>,
}

impl LangCSharp {
    pub fn new() -> Self {
        LangCSharp {
            using_decls: default_using_decls(),
            opaque_types: Default::default(),
            custom_decls: Vec::new(),
            ignored_functions: Default::default(),
            extract_comments: false,
            context: Context {
                lib_name: "backend".to_string(),
                class_name: "Backend".to_string(),
            },
            consts: Vec::new(),
            enums: Vec::new(),
            structs: Vec::new(),
            functions: Vec::new(),
            aliases: Default::default(),
        }
    }

    /// Set the name of the native library. This also sets the class name.
    pub fn set_lib_name<T: Into<String>>(&mut self, name: T) {
        let name = name.into();
        self.context.class_name = name.to_pascal_case();
        self.context.lib_name = name;
    }

    /// Set the name of the static class containing all transformed functions and
    /// constants. By default this is derived from the linked library name.
    pub fn set_class_name<T: Into<String>>(&mut self, name: T) {
        self.context.class_name = name.into();
    }

    /// Add additional `using` declaration.
    pub fn add_using_decl<T: Into<String>>(&mut self, decl: T) {
        let _ = self.using_decls.insert(decl.into());
    }

    /// Add definition of opaque type (type represented by an opaque pointer).
    pub fn add_opaque_type<T: Into<String>>(&mut self, name: T) {
        let _ = self.opaque_types.insert(name.into());
    }

    /// Add additional declaration.
    pub fn add_custom_decl<T: Into<String>>(&mut self, decl: T) {
        self.custom_decls.push(decl.into());
    }

    /// Ignore the function with the given name when transforming to the target language.
    pub fn ignore_function<T: Into<String>>(&mut self, name: T) {
        let _ = self.ignored_functions.insert(name.into());
    }

    fn resolve_aliases(&mut self) {
        for snippet in &mut self.consts {
            resolve_alias(&self.aliases, &mut snippet.item.ty);
        }

        for snippet in &mut self.structs {
            for field in &mut snippet.item.fields {
                resolve_alias(&self.aliases, &mut field.ty);
            }
        }

        for snippet in &mut self.functions {
            resolve_alias(&self.aliases, &mut snippet.item.output);

            for &mut (_, ref mut ty) in &mut snippet.item.inputs {
                resolve_alias(&self.aliases, ty)
            }
        }
    }
}

impl Lang for LangCSharp {
    fn parse_ty(&mut self, item: &ast::Item, _outputs: &mut Outputs) -> Result<(), Error> {
        if let ast::ItemKind::Ty(ref ty, ref generics) = item.node {
            if generics.is_parameterized() {
                println!("parameterized type aliases not supported. Skipping.");
                return Ok(());
            }

            let ty = transform_type(ty).ok_or_else(|| {
                Error {
                    level: Level::Error,
                    span: Some(ty.span),
                    message: format!(
                        "bindgen can not handle the type `{}`",
                        pprust::ty_to_string(ty)
                    ),
                }
            })?;
            let name = item.ident.name.as_str().to_string();

            self.aliases.insert(name, ty);
        }

        Ok(())
    }

    fn parse_const(&mut self, item: &ast::Item, _outputs: &mut Outputs) -> Result<(), Error> {
        let docs = if self.extract_comments {
            common::parse_attr(&item.attrs, |_| true, retrieve_docstring).1
        } else {
            String::new()
        };

        if let ast::ItemKind::Const(ref ty, ref expr) = item.node {
            let name = item.ident.name.as_str().to_string();
            let item = transform_const(ty, expr).ok_or_else(|| {
                Error {
                    level: Level::Error,
                    span: Some(expr.span),
                    message: format!(
                        "bindgen can not handle constant {}",
                        pprust::item_to_string(item)
                    ),
                }
            })?;

            self.consts.push(Snippet { docs, name, item });
        }

        Ok(())
    }

    fn parse_enum(&mut self, item: &ast::Item, _outputs: &mut Outputs) -> Result<(), Error> {
        let (repr_c, docs) = if self.extract_comments {
            common::parse_attr(&item.attrs, common::check_repr_c, retrieve_docstring)
        } else {
            common::parse_attr(&item.attrs, common::check_repr_c, |_| None)
        };

        // If it's not #[repr(C)] ignore it.
        if !repr_c {
            return Ok(());
        }


        if let ast::ItemKind::Enum(ast::EnumDef { ref variants }, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err(unsupported_generics_error(item, "enums"));
            }

            let name = item.ident.name.as_str().to_string();
            let item = transform_enum(variants).ok_or_else(|| {
                Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: format!(
                        "bindgen can not handle enum {}",
                        pprust::item_to_string(item)
                    ),
                }
            })?;

            self.enums.push(Snippet { docs, name, item });
        }

        Ok(())
    }

    fn parse_struct(&mut self, item: &ast::Item, _outputs: &mut Outputs) -> Result<(), Error> {
        let (repr_c, docs) = if self.extract_comments {
            common::parse_attr(&item.attrs, common::check_repr_c, retrieve_docstring)
        } else {
            common::parse_attr(&item.attrs, common::check_repr_c, |_| None)
        };

        // If it's not #[repr(C)] ignore it.
        if !repr_c {
            return Ok(());
        }

        let name = item.ident.name.as_str();

        if let ast::ItemKind::Struct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err(unsupported_generics_error(item, "structs"));
            }

            if !variants.is_struct() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: format!("bindgen can not handle unit or tuple structs ({})", name),
                });
            }

            let item = transform_struct(variants.fields()).ok_or_else(|| {
                Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: format!(
                        "bindgen can not handle struct {}",
                        pprust::item_to_string(item)
                    ),
                }
            })?;
            let name = name.to_string();

            self.structs.push(Snippet { docs, name, item });
        }

        Ok(())
    }

    fn parse_fn(&mut self, item: &ast::Item, _outputs: &mut Outputs) -> Result<(), Error> {
        let name = item.ident.name.as_str();

        if self.ignored_functions.contains(&*name) {
            return Ok(());
        }

        let (no_mangle, docs) = if self.extract_comments {
            common::parse_attr(&item.attrs, common::check_no_mangle, retrieve_docstring)
        } else {
            common::parse_attr(&item.attrs, common::check_no_mangle, |_| None)
        };

        // Ignore function without #[no_mangle].
        if !no_mangle {
            return Ok(());
        }

        if let ast::ItemKind::Fn(ref fn_decl, unsafety, ref constness, abi, ref generics, _) =
            item.node
        {
            if !common::is_extern(abi) {
                return Ok(());
            }

            if generics.is_parameterized() {
                return Err(unsupported_generics_error(item, "extern functions"));
            }

            let item = transform_function(&fn_decl).ok_or_else(|| {
                let string =
                    pprust::fun_to_string(fn_decl, unsafety, constness.node, item.ident, generics);

                Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: format!("bindgen can not handle function {}", string),
                }
            })?;
            let name = name.to_string();

            self.functions.push(Snippet { docs, name, item });
        }

        Ok(())
    }

    fn finalise_output(&mut self, outputs: &mut Outputs) -> Result<(), Error> {
        self.resolve_aliases();
        self.ignored_functions.clear();

        let mut output = String::new();

        {
            let mut output = IndentedOutput::new(&mut output, INDENT_WIDTH);

            // Using declarations.
            for decl in &self.using_decls {
                emit!(output, "using {};\n", decl);
            }
            self.using_decls = default_using_decls();

            emit!(output, "\n");

            // Enums
            for snippet in self.enums.drain(..) {
                emit!(output, "{}", snippet.docs);
                emit_enum(&mut output, &snippet.name, &snippet.item);
            }

            // Structs
            for snippet in self.structs.drain(..) {
                emit!(output, "{}", snippet.docs);
                emit_struct(&mut output, &self.context, &snippet.name, &snippet.item);
            }

            // Opaque types.
            for name in &self.opaque_types {
                emit_opaque_type(&mut output, name);
            }
            self.opaque_types.clear();

            if !self.functions.is_empty() || !self.consts.is_empty() {
                emit!(
                    output,
                    "public static class {} {{\n",
                    self.context.class_name
                );
                output.indent();

                // Define constant with the native library name, to be used in
                // the [DllImport] attributes.
                emit!(output, "#if __IOS__\n");
                emit!(output, "private const String DLL_NAME = \"__Internal\";\n");
                emit!(output, "#else\n");
                emit!(
                    output,
                    "private const String DLL_NAME = \"{}\";\n",
                    self.context.lib_name
                );
                emit!(output, "#endif\n\n");

                // Custom declarations.
                if !self.custom_decls.is_empty() {
                    emit!(output, "#region custom declarations\n");
                    for decl in self.custom_decls.drain(..) {
                        emit!(output, "{}\n", decl);
                    }
                    emit!(output, "#endregion\n\n");
                }

                // Consts
                for snippet in &self.consts {
                    emit!(output, "{}", snippet.docs);
                    emit_const(&mut output, &snippet.name, &snippet.item);
                }

                if !self.consts.is_empty() {
                    emit!(output, "\n");
                    self.consts.clear();
                }

                // Functions
                for snippet in &self.functions {
                    emit!(output, "{}", snippet.docs);
                    emit_function(&mut output, &self.context, &snippet.name, &snippet.item);
                }

                {
                    for callbacks in collect_callbacks(&self.functions) {
                        emit_callback_wrappers(&mut output, &callbacks);
                    }
                }

                self.functions.clear();

                output.unindent();
                emit!(output, "}}\n");
            }
        }

        outputs.insert(
            PathBuf::from(format!("{}.cs", self.context.class_name)),
            output,
        );

        Ok(())
    }
}

pub struct Context {
    lib_name: String,
    class_name: String,
}

fn resolve_alias(aliases: &HashMap<String, Type>, new_ty: &mut Type) {
    let mut orig_new_ty = mem::replace(new_ty, Type::Unit);

    match orig_new_ty {
        Type::User(ref name) => {
            if let Some(old_ty) = lookup_alias(aliases, name) {
                *new_ty = old_ty.clone();
                return;
            }
        }
        Type::Pointer(ref mut ty) => {
            resolve_alias(aliases, ty);
        }
        Type::Array(ref mut ty, _) => {
            resolve_alias(aliases, ty);
        }
        Type::Function(ref mut fun) => {
            resolve_alias(aliases, &mut fun.output);
            for &mut (_, ref mut input) in &mut fun.inputs {
                resolve_alias(aliases, input);
            }
        }
        _ => (),
    }

    mem::replace(new_ty, orig_new_ty);
}

fn lookup_alias<'a>(aliases: &'a HashMap<String, Type>, name: &str) -> Option<&'a Type> {
    if let Some(ty) = aliases.get(name) {
        if let Type::User(ref name) = *ty {
            Some(lookup_alias(aliases, name).unwrap_or(ty))
        } else {
            Some(ty)
        }
    } else {
        None
    }
}

fn default_using_decls() -> BTreeSet<String> {
    let mut result = BTreeSet::default();
    let _ = result.insert("System".to_string());
    let _ = result.insert("System.Runtime.InteropServices".to_string());
    result
}

fn collect_callbacks(functions: &[Snippet<Function>]) -> Vec<Vec<(&str, &Function)>> {
    let mut stash = BTreeMap::new();

    for snippet in functions {
        let callbacks = extract_callbacks(&snippet.item.inputs);
        let name = callback_wrapper_name(&callbacks);

        let _ = stash.entry(name).or_insert(callbacks);
    }

    stash.into_iter().map(|(_, callbacks)| callbacks).collect()
}

fn callback_wrapper_name(callbacks: &[(&str, &Function)]) -> String {
    let mut output = String::new();
    {
        let mut output = IndentedOutput::new(&mut output, INDENT_WIDTH);
        emit_callback_wrapper_name(&mut output, callbacks, 0);
    }

    output
}

fn unsupported_generics_error(item: &ast::Item, name: &str) -> Error {
    Error {
        level: Level::Error,
        span: Some(item.span),
        message: format!("bindgen can not handle parameterized {}", name),
    }
}
