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
use std::collections::{BTreeSet, HashSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use syntax::ast;
use syntax::print::pprust;

const INDENT_WIDTH: usize = 4;
const TYPES: &'static str = "_types";
const FUNCTIONS: &'static str = "_functions";
const CONSTANTS: &'static str = "_constants";

pub struct LangCSharp {
    lib_name: String,
    using_decls: BTreeSet<String>,
    opaque_types: HashSet<String>,
    custom_decls: Vec<String>,
    ignored_functions: HashSet<String>,
    callback_arities: BTreeSet<Vec<usize>>,
    context: Context,
}

impl LangCSharp {
    pub fn new<T: Into<String>>(lib_name: T) -> Self {
        let mut using_decls = BTreeSet::default();
        let _ = using_decls.insert("System".to_string());
        let _ = using_decls.insert("System.Runtime.InteropServices".to_string());

        let lib_name = lib_name.into();
        let class_name = lib_name.to_pascal_case();

        LangCSharp {
            lib_name,
            using_decls,
            opaque_types: Default::default(),
            custom_decls: Vec::new(),
            ignored_functions: Default::default(),
            callback_arities: Default::default(),
            context: Context { class_name },
        }
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
}

impl Lang for LangCSharp {
    fn parse_const(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
        let (_, docs) = common::parse_attr(&item.attrs, |_| true, retrieve_docstring);

        if let ast::ItemKind::Const(ref ty, ref expr) = item.node {
            let name = &*item.ident.name.as_str();
            let ty = transform_type(ty).ok_or_else(|| unknown_type_error(ty))?;
            let value = transform_const_value(expr).ok_or_else(|| {
                let value = pprust::expr_to_string(expr);

                Error {
                    level: Level::Error,
                    span: Some(expr.span),
                    message: format!("bindgen can not handle constant value {}", value),
                }
            })?;

            let mut output = output(outputs, CONSTANTS);

            emit!(output, "{}", docs);
            emit_const(&mut output, name, &ty, &value)
        }

        Ok(())
    }

    fn parse_ty(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
        let (_, docs) = common::parse_attr(&item.attrs, |_| true, retrieve_docstring);
        let name = &*item.ident.name.as_str();

        if let ast::ItemKind::Ty(ref ty, ref generics) = item.node {
            if generics.is_parameterized() {
                println!("parameterized type aliases not supported. Skipping.");
                return Ok(());
                // return Err(unsupported_generics_error(item, "type aliases"));
            }

            let ty = transform_type(ty).ok_or_else(|| unknown_type_error(ty))?;

            let mut output = output(outputs, TYPES);

            emit!(output, "{}", docs);
            emit_type_alias(&mut output, &self.context, &ty, &name);
        }

        Ok(())
    }

    fn parse_enum(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
        let (repr_c, docs) =
            common::parse_attr(&item.attrs, common::check_repr_c, retrieve_docstring);

        // If it's not #[repr(C)] ignore it.
        if !repr_c {
            return Ok(());
        }

        let name = &*item.ident.name.as_str();

        if let ast::ItemKind::Enum(ast::EnumDef { ref variants }, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err(unsupported_generics_error(item, "enums"));
            }

            let mut output = output(outputs, TYPES);

            emit!(output, "{}", docs);
            emit!(output, "public enum {} {{\n", name);
            output.indent();

            for variant in variants {
                if !variant.node.data.is_unit() {
                    return Err(Error {
                        level: Level::Error,
                        span: Some(variant.span),
                        message: "bindgen can not handle enum variants with data".into(),
                    });
                }

                let (_, doc) =
                    common::parse_attr(&variant.node.attrs, |_| true, retrieve_docstring);
                let name = &*variant.node.name.name.as_str();
                let value = extract_enum_variant_value(variant);

                emit!(output, "{}", doc);

                if let Some(value) = value {
                    emit!(output, "{} = {},\n", name, value);
                } else {
                    emit!(output, "{},\n", name);
                }
            }

            output.unindent();
            emit!(output, "}}\n\n");
        }

        Ok(())
    }

    fn parse_struct(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
        let (repr_c, docs) =
            common::parse_attr(&item.attrs, common::check_repr_c, retrieve_docstring);

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
                    message: "bindgen can not handle unit or tuple structs".into(),
                });
            }

            let mut output = output(outputs, TYPES);

            emit!(output, "{}", docs);
            emit!(output, "[StructLayout(LayoutKind.Sequential)]\n");
            emit!(output, "public class {} {{\n", name);
            output.indent();

            let fields = variants.fields();
            let mut fields = fields.iter();
            while let Some(field) = fields.next() {
                let (_, doc) = common::parse_attr(&field.attrs, |_| true, retrieve_docstring);

                let name = field.ident.unwrap().name.as_str();
                let name = name.to_camel_case();

                let ty = transform_type(&field.ty).ok_or_else(
                    || unknown_type_error(&field.ty),
                )?;

                emit!(output, "{}", doc);
                emit_struct_field(&mut output, &self.context, "public", &ty, &name);
            }

            output.unindent();
            emit!(output, "}}\n\n");
        }

        Ok(())
    }

    fn parse_fn(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
        let name = item.ident.name.as_str();

        if self.ignored_functions.contains(&*name) {
            return Ok(());
        }

        let (no_mangle, docs) =
            common::parse_attr(&item.attrs, common::check_no_mangle, retrieve_docstring);

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

            let mut output = output(outputs, FUNCTIONS);

            let function = transform_function(&fn_decl).ok_or_else(|| {
                let decl =
                    pprust::fun_to_string(fn_decl, unsafety, constness.node, item.ident, generics);

                Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: format!("bindgen can not handle function {}", decl),
                }
            })?;

            let callbacks = extract_callbacks(&function.inputs);
            let callback_arities: Vec<_> = callbacks
                .into_iter()
                .map(|(_, ref fun)| callback_arity(fun))
                .collect();
            if !callback_arities.is_empty() {
                let _ = self.callback_arities.insert(callback_arities);
            }

            emit!(output, "{}", docs);
            emit_function_wrapper(&mut output, &self.context, &name, &function);
            emit_function_extern_decl(&mut output, &self.context, &name, &function, &self.lib_name);
        }

        Ok(())
    }

    fn finalise_output(&mut self, outputs: &mut Outputs) -> Result<(), Error> {
        let mut output = String::new();
        let class_name = self.lib_name.to_pascal_case();

        {
            let mut output = IndentedOutput::new(&mut output, INDENT_WIDTH);

            let types = outputs.remove(Path::new(TYPES)).unwrap_or(String::new());
            let functions = outputs.remove(Path::new(FUNCTIONS)).unwrap_or(
                String::new(),
            );
            let constants = outputs.remove(Path::new(CONSTANTS)).unwrap_or(
                String::new(),
            );

            if !types.is_empty() || !functions.is_empty() || !constants.is_empty() {
                for decl in &self.using_decls {
                    emit!(output, "using {};\n", decl);
                }
                emit!(output, "\n");
            }

            emit!(output, "{}", types);

            // Inject opaque types.
            let opaque_ty = Type::Pointer(Box::new(Type::Unit));
            for name in &self.opaque_types {
                emit_type_alias(&mut output, &self.context, &opaque_ty, name);
            }

            if !functions.is_empty() || !constants.is_empty() {
                emit!(output, "public static class {} {{\n", class_name);
                output.indent();

                // Inject custom declarations.
                if !self.custom_decls.is_empty() {
                    emit!(output, "#region custom declarations\n");
                    for decl in &self.custom_decls {
                        emit!(output, "{}\n", decl);
                    }
                    emit!(output, "#endregion\n\n");
                }

                emit!(output, "{}", constants);
                emit!(output, "{}", functions);
                emit_callback_wrappers(&mut output, &self.callback_arities);

                output.unindent();
                emit!(output, "}}\n");
            }
        }

        outputs.insert(PathBuf::from(format!("{}.cs", class_name)), output);

        Ok(())
    }
}

pub struct Context {
    class_name: String,
}

impl Context {}

fn output<'a>(outputs: &'a mut Outputs, name: &str) -> IndentedOutput<'a> {
    IndentedOutput::new(
        outputs.entry(PathBuf::from(name)).or_insert(String::new()),
        INDENT_WIDTH,
    )
}


fn unknown_type_error(ty: &ast::Ty) -> Error {
    Error {
        level: Level::Error,
        span: Some(ty.span),
        message: format!(
            "bindgen can not handle the type `{}`",
            pprust::ty_to_string(ty)
        ),
    }
}

fn unsupported_generics_error(item: &ast::Item, name: &str) -> Error {
    Error {
        level: Level::Error,
        span: Some(item.span),
        message: format!("bindgen can not handle parameterized {}", name),
    }
}
