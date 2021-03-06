//! Functions for generating Java glue code.

mod jni;
mod types;

use common::{
    self, append_output, check_no_mangle, is_array_arg, is_user_data_arg, parse_attr,
    retrieve_docstring, Outputs,
};
use inflector::Inflector;
use java::types::{callback_name, java_type_to_str, rust_to_java, struct_to_java_classname};
use jni::signature::JavaType;
use rustfmt;
use std::collections::{BTreeSet, HashMap};
use struct_field::{transform_struct_fields, StructField};
use syntax::abi::Abi;
use syntax::print::pprust;
use syntax::{ast, codemap};
use Error;
use Level;

pub struct LangJava {
    context: Context,
}

pub struct Context {
    /// Native library name
    lib_name: String,
    /// Namespace
    namespace: String,
    /// Model namespace (structures go into this one)
    namespace_model: String,
    /// Maps types from Rust to Java
    type_map: HashMap<&'static str, JavaType>,
    /// Keeps track of which JNI callback functions has been generated already
    generated_jni_cbs: BTreeSet<String>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            lib_name: "safe".to_string(),
            namespace: "net.maidsafe.dummy".to_string(),
            namespace_model: "net.maidsafe.dummy".to_string(),
            type_map: Default::default(),
            generated_jni_cbs: Default::default(),
        }
    }
}

impl LangJava {
    pub fn new(type_map: HashMap<&'static str, JavaType>) -> Self {
        LangJava {
            context: Context {
                type_map,
                lib_name: "backend".to_owned(),
                namespace: "net.maidsafe.bindings".to_owned(),
                namespace_model: "net.maidsafe.model".to_owned(),
                generated_jni_cbs: BTreeSet::new(),
            },
        }
    }

    /// Set the name of the native library. This also sets the class name.
    pub fn set_lib_name<T: Into<String>>(&mut self, name: T) {
        self.context.lib_name = name.into();
    }

    /// Set the namespace to put the NativeBindings class in.
    pub fn set_namespace<T: Into<String>>(&mut self, namespace: T) {
        self.context.namespace = namespace.into();
    }

    /// Set the namespace to put all structures/classes in.
    pub fn set_model_namespace<T: Into<String>>(&mut self, namespace: T) {
        self.context.namespace_model = namespace.into();
    }

    /// Applies rustfmt to JNI code to improve debuggability
    fn format_jni_output(&self, input: &mut String) {
        let mut output: Vec<u8> = Vec::with_capacity(input.len() * 2);

        let mut cfg = rustfmt::config::Config::default();
        cfg.set().write_mode(rustfmt::config::WriteMode::Plain);

        unwrap!(rustfmt::format_input(
            rustfmt::Input::Text(input.clone()),
            &cfg,
            Some(&mut output),
        ));

        *input = String::from_utf8(output).expect("Invalid Rustfmt output found");
    }

    /// Adds package info to the NativeBindings Java module and indents lines
    fn format_native_functions(&self, funcs: &mut String) {
        // Indent lines
        let lines = funcs.lines().fold(String::new(), |mut output, line| {
            output.push_str(&format!("\t{}\n", line));
            output
        });
        *funcs = format!(
            "package {namespace};\n\n
                         public class NativeBindings {{\n
                         {lines}\n
                         }}",
            namespace = self.context.namespace,
            lines = lines
        );
    }
}
impl common::Lang for LangJava {
    /// Convert a Rust function declaration into Java.
    fn parse_fn(
        &mut self,
        item: &ast::Item,
        _module: &[String],
        outputs: &mut Outputs,
    ) -> Result<(), Error> {
        let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, |attr| {
            retrieve_docstring(attr, "")
        });
        // If it's not #[no_mangle] then it can't be called from C.
        if !no_mangle {
            return Ok(());
        }

        let name = item.ident.name.as_str();

        if let ast::ItemKind::Fn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
            if !common::is_extern(abi) {
                // If it doesn't have a C ABI it can't be called from C.
                return Ok(());
            }

            if generics.is_parameterized() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "cheddar can not handle parameterized extern functions".into(),
                });
            }

            transform_native_fn(
                &*fn_decl,
                &docs,
                &format!("{}", name),
                outputs,
                &mut self.context,
            )?;

            Ok(())
        } else {
            Err(Error {
                level: Level::Bug,
                span: Some(item.span),
                message: "`parse_fn` called on wrong `Item_`".into(),
            })
        }
    }

    /// Convert a Rust struct into a Java class.
    fn parse_struct(
        &mut self,
        item: &ast::Item,
        _module: &[String],
        outputs: &mut Outputs,
    ) -> Result<(), Error> {
        let (repr_c, docs) = parse_attr(&item.attrs, common::check_repr_c, |attr| {
            retrieve_docstring(attr, "")
        });
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c {
            return Ok(());
        }

        let mut buffer = String::new();
        buffer.push_str(&format!("package {};\n\n", self.context.namespace));
        buffer.push_str(&docs);

        let orig_name = item.ident.name.as_str();
        let name = struct_to_java_classname(&*orig_name);
        buffer.push_str(&format!("public class {}", name));

        if let ast::ItemKind::Struct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "cheddar can not handle parameterized `#[repr(C)]` structs".into(),
                });
            }

            if variants.is_struct() {
                let struct_fields = transform_struct_fields(variants.fields());
                let fields = transform_struct_into_class_fields(&struct_fields, &self.context)?;

                buffer.push_str(" {\n");

                // Class fields
                buffer.push_str(&generate_class_fields(&fields)?);
                buffer.push_str("\n");

                // Default constructor that should initialise object fields
                buffer.push_str(&generate_default_constructor(&name, &fields)?);

                // Parametrised constructor
                buffer.push_str(&generate_parametrised_constructor(&name, &fields)?);

                // Getters & setters
                buffer.push_str(&generate_getters_setters(&fields)?);
                buffer.push_str("}");

                let jni = jni::generate_struct(&struct_fields, &orig_name, &name, &self.context);
                append_output(jni, "jni.rs", outputs);
            } else if variants.is_tuple() && variants.fields().len() == 1 {
                // #[repr(C)] pub struct Foo(Bar);  =>  typedef struct Foo Foo;
            } else {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "cheddar can not handle unit or tuple `#[repr(C)]` \
                              structs with >1 members"
                        .into(),
                });
            }
        } else {
            return Err(Error {
                level: Level::Bug,
                span: Some(item.span),
                message: "`parse_struct` called on wrong `Item_`".into(),
            });
        }

        buffer.push_str("\n\n");

        outputs.insert(format!("{}.java", name), buffer);

        Ok(())
    }

    fn finalise_output(&mut self, outputs: &mut Outputs) -> Result<(), Error> {
        match outputs.get_mut("jni.rs") {
            Some(input) => {
                self.format_jni_output(input);
            }
            None => {
                return Err(Error {
                    level: Level::Error,
                    span: None,
                    message: "no jni bindings generated?".to_owned(),
                })
            }
        }

        match outputs.get_mut("NativeBindings.java") {
            Some(input) => {
                self.format_native_functions(input);
                Ok(())
            }
            None => Err(Error {
                level: Level::Error,
                span: None,
                message: "no native bindings generated?".to_owned(),
            }),
        }
    }
}

/// Contains all information necessary to construct a Java class
/// field, transformed from `StructField`.
struct JavaClassField {
    name: String,
    ty: JavaType,
    ty_str: String,
}

/// Transforms a list of struct fields into Java class fields
fn transform_struct_into_class_fields(
    fields: &[StructField],
    context: &Context,
) -> Result<Vec<JavaClassField>, Error> {
    let mut class_fields = Vec::new();

    for field in fields {
        let name = field.name().to_camel_case();
        let struct_field = field.struct_field().clone();
        let mut ty = rust_to_java(&*struct_field.ty, context)?;
        if let StructField::Array { .. } = *field {
            // Wrap a type into an array
            ty = JavaType::Array(Box::new(ty));
        }
        let ty_str = java_type_to_str(&ty)?;

        class_fields.push(JavaClassField { name, ty, ty_str });
    }

    Ok(class_fields)
}

/// Generates getters and setters for a struct transformed into a Java class
fn generate_getters_setters(fields: &[JavaClassField]) -> Result<String, Error> {
    let mut buffer = String::new();

    for field in fields {
        buffer.push_str(&format!(
            "\tpublic {ty} get{capitalized}() {{\n\t\treturn {name};\n\t}}\n\n",
            ty = field.ty_str,
            name = field.name,
            capitalized = field.name.to_class_case(),
        ));
        buffer.push_str(&format!(
            "\tpublic void set{capitalized}(final {ty} val) {{\n\t\tthis.{name} \
             = val;\n\t}}\n\n",
            ty = field.ty_str,
            name = field.name,
            capitalized = field.name.to_class_case(),
        ));
    }

    Ok(buffer)
}

/// Generates fields for a struct transformed into a Java class
fn generate_class_fields(fields: &[JavaClassField]) -> Result<String, Error> {
    let mut buffer = String::new();

    for field in fields {
        buffer.push_str(&format!("\tprivate {} {};\n", field.ty_str, field.name));
    }

    Ok(buffer)
}

/// Generates code for the default constructor initialising class fields with
/// default values
fn generate_default_constructor(
    class_name: &str,
    fields: &[JavaClassField],
) -> Result<String, Error> {
    let mut default_obj_fields = Vec::new();

    for field in fields {
        // Initialise object and array fields with default values to prevent them from being null
        match field.ty {
            JavaType::Array(..) => {
                default_obj_fields.push(format!(
                    "\t\tthis.{name} = new {ty} {{}};",
                    name = field.name,
                    ty = field.ty_str
                ));
            }
            JavaType::Object(ref obj) => {
                default_obj_fields.push(format!(
                    "\t\tthis.{name} = new {obj}();",
                    name = field.name,
                    obj = obj
                ));
            }
            _ => (),
        }
    }
    Ok(format!(
        "\tpublic {name}() {{\n{default_obj_fields}\n\t}}\n",
        name = class_name,
        default_obj_fields = default_obj_fields.join("\n")
    ))
}

/// Generates code for the parametrised constructor (which is taking arguments to
/// initialise default values)
fn generate_parametrised_constructor(
    class_name: &str,
    fields: &[JavaClassField],
) -> Result<String, Error> {
    let mut constructor_fields = Vec::new();
    let mut constructor_assignments = Vec::new();

    for field in fields {
        constructor_fields.push(format!("{} {}", field.ty_str, field.name));
        constructor_assignments.push(format!("\t\tthis.{name} = {name};", name = field.name));
    }

    Ok(format!(
        "\tpublic {name}({constructor_fields}) {{\n{constructor_assignments}\n\t}}\n",
        name = class_name,
        constructor_fields = constructor_fields.join(", "),
        constructor_assignments = constructor_assignments.join("\n")
    ))
}

/// Transform a Rust FFI function into a Java native function
pub fn transform_native_fn(
    fn_decl: &ast::FnDecl,
    docs: &str,
    name: &str,
    outputs: &mut Outputs,
    context: &mut Context,
) -> Result<(), Error> {
    let mut args_str = Vec::new();

    let mut fn_args = fn_decl
        .inputs
        .iter()
        .filter(|arg| !is_user_data_arg(arg))
        .peekable();

    while let Some(arg) = fn_args.next() {
        let arg_name = pprust::pat_to_string(&*arg.pat);

        // Generate function arguments
        let mut java_type = rust_to_java(&arg.ty, context)?;

        if is_array_arg(arg, fn_args.peek().cloned()) {
            // Skip the length args - e.g. for a case of `ptr: *const u8, ptr_len: usize`
            // we're going to skip the `len` part.
            java_type = JavaType::Array(Box::new(java_type));
            fn_args.next();
        }

        let java_type = java_type_to_str(&java_type)?;
        args_str.push(format!("{} {}", java_type, arg_name.to_camel_case()));

        // Generate a callback class - if it wasn't generated already
        if let ast::TyKind::BareFn(ref bare_fn) = arg.ty.node {
            let cb_class = callback_name(&*bare_fn.decl.inputs, context)?;
            let cb_file = format!("{}.java", cb_class);

            if outputs.get(&cb_file).is_none() {
                eprintln!("Generating CB {}", cb_class);

                let cb_output = transform_callback(&*arg.ty, &cb_class, context)?;
                let _ = outputs.insert(cb_file, cb_output);

                // Generate JNI callback fn
                let jni_cb_name = format!("call_{}", cb_class);
                if !context.generated_jni_cbs.contains(&jni_cb_name) {
                    let mut jni = jni::generate_jni_callback(bare_fn, &jni_cb_name, context);
                    jni.push_str("\n");

                    append_output(jni, "jni.rs", outputs);
                    context.generated_jni_cbs.insert(jni_cb_name);
                }
            }
        }
    }

    let output_type = &fn_decl.output;
    let return_type = match *output_type {
        ast::FunctionRetTy::Ty(ref ty) if ty.node == ast::TyKind::Never => {
            return Err(Error {
                level: Level::Error,
                span: Some(ty.span),
                message: "panics across a C boundary are naughty!".into(),
            });
        }
        ast::FunctionRetTy::Default(..) => String::from("public static native void"),
        ast::FunctionRetTy::Ty(ref ty) => java_type_to_str(&rust_to_java(&*ty, context)?)?,
    };

    let java_name = name.to_camel_case();
    let func_decl = format!(
        "{} {}({})",
        return_type,
        &java_name,
        args_str.as_slice().join(", ")
    );

    let mut buffer = String::new();
    buffer.push_str("/**\n");
    buffer.push_str(&docs.replace("///", " *"));
    buffer.push_str(" */\n");
    buffer.push_str(&func_decl);
    buffer.push_str(";\n\n");

    append_output(buffer, "NativeBindings.java", outputs);

    // Append the function declaration to import it as an "extern fn"
    let fn_decl_import = pprust::fun_to_string(
        fn_decl,
        ast::Unsafety::Normal,
        ast::Constness::NotConst,
        ast::Ident::from_str(name),
        &ast::Generics::default(),
    );
    let mut jni = format!(
        "\n#[link(name = \"safe_app\")]\nextern {{ {fndecl}; }}\n",
        fndecl = fn_decl_import,
    );

    // Generate the JNI part of the interface
    jni.push_str(&jni::generate_jni_function(
        fn_decl.inputs.clone(),
        name,
        &java_name,
        context,
        outputs,
    ));
    jni.push_str("\n");
    append_output(jni, "jni.rs", outputs);

    Ok(())
}

/// Turn a Rust callback function type into a Java interface.
pub fn transform_callback<S: AsRef<str>>(
    ty: &ast::Ty,
    class_name: S,
    context: &Context,
) -> Result<String, Error> {
    match ty.node {
        ast::TyKind::BareFn(ref bare_fn) => Ok(format!(
            "package {namespace};\n\n\
             public interface {name} {{\n\
             \tpublic void call({types});\n}}\n",
            namespace = context.namespace_model,
            name = class_name.as_ref(),
            types = callback_to_java(bare_fn, ty.span, context)?,
        )),
        // All other types just have a name associated with them.
        _ => Err(Error {
            level: Level::Error,
            span: Some(ty.span),
            message: "Invalid callback type".into(),
        }),
    }
}

/// Transform a Rust FFI callback into Java function signature
fn callback_to_java(
    fn_ty: &ast::BareFnTy,
    fn_span: codemap::Span,
    context: &Context,
) -> Result<String, Error> {
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {}
        _ => {
            return Err(Error {
                level: Level::Error,
                span: Some(fn_span),
                message: "callbacks that don't have C ABI are not supported".into(),
            })
        }
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err(Error {
            level: Level::Error,
            span: Some(fn_span),
            message: "can not handle lifetimes".into(),
        });
    }

    let fn_decl: &ast::FnDecl = &*fn_ty.decl;
    let mut args = Vec::new();

    let mut args_iter = fn_decl
        .inputs
        .iter()
        .filter(|arg| !is_user_data_arg(arg))
        .peekable();

    while let Some(arg) = args_iter.next() {
        let arg_name = pprust::pat_to_string(&*arg.pat);
        let mut java_type = rust_to_java(&*arg.ty, context)?;

        if is_array_arg(arg, args_iter.peek().cloned()) {
            // Detect array ptrs: skip the length args and add array to the type sig
            java_type = JavaType::Array(Box::new(java_type));
            args_iter.next();
        }

        let java_type = java_type_to_str(&java_type)?;
        args.push(format!("{} {}", java_type, arg_name.to_camel_case()));
    }

    Ok(args.join(", "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntax::ast::{Arg, ItemKind};
    use syntax::codemap::FilePathMapping;
    use syntax::parse::{self, ParseSess};

    #[test]
    fn cb_names() {
        fn get_inputs(source: &str) -> Vec<Arg> {
            let parse_sess = ParseSess::new(FilePathMapping::empty());

            let item = unwrap!(unwrap!(parse::parse_item_from_source_str(
                "dummy.rs".to_owned(),
                source.to_owned(),
                &parse_sess,
            )));

            match item.node {
                ItemKind::Fn(ref fn_decl, _, _, _, _, _) => fn_decl.inputs.clone(),
                _ => panic!("wrong item type"),
            }
        }

        let context = Context {
            type_map: HashMap::new(),
            lib_name: "backend".to_owned(),
            namespace: "net.maidsafe.bindings".to_owned(),
            namespace_model: "net.maidsafe.model".to_owned(),
            generated_jni_cbs: BTreeSet::new(),
        };

        let inputs = get_inputs("fn dummy() {}");
        assert_eq!("CallbackVoid", unwrap!(callback_name(&inputs, &context)));

        let inputs = get_inputs("fn dummy(user_data: *mut c_void, result: *const FfiResult) {}");
        assert_eq!("CallbackResult", unwrap!(callback_name(&inputs, &context)));

        let inputs = get_inputs(
            "fn dummy(user_data: *mut c_void, result: *const FfiResult, b: u64, c: u32) {}",
        );
        assert_eq!(
            "CallbackResultLongInt",
            unwrap!(callback_name(&inputs, &context))
        );

        let inputs = get_inputs(
            "fn dummy(user_data: *mut c_void, result: *const FfiResult, b: *const MyStruct) {}",
        );
        assert_eq!(
            "CallbackResultMyStruct",
            unwrap!(callback_name(&inputs, &context))
        );

        let inputs = get_inputs(
            "fn dummy(user_data: *mut c_void, result: *const FfiResult, b: *const c_char) {}",
        );
        assert_eq!(
            "CallbackResultString",
            unwrap!(callback_name(&inputs, &context))
        );
    }
}
