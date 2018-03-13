//! Functions for converting Rust types to Java types.


use Error;
use Level;
use common::{self, Outputs, append_output, check_no_mangle, is_array_arg, is_result_arg,
             is_user_data_arg, parse_attr, retrieve_docstring};
use inflector::Inflector;
use rustfmt;
use std::collections::{BTreeSet, HashMap};
use std::path::PathBuf;
use struct_field::{StructField, transform_struct_fields};
use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap;
use syntax::print::pprust;
mod jni;

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
    type_map: HashMap<&'static str, &'static str>,
    /// Keeps track of which JNI callback functions has been generated already
    generated_jni_cbs: BTreeSet<String>,
}

impl LangJava {
    pub fn new(type_map: HashMap<&'static str, &'static str>) -> Self {
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
        let lines = funcs.lines().fold(String::new(), |mut s, line| {
            s.push_str(&format!("\t{}\n", line));
            return s;
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
    fn parse_fn(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
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
    fn parse_struct(&mut self, item: &ast::Item, outputs: &mut Outputs) -> Result<(), Error> {
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
                let mut constructor_fields = Vec::new();
                let mut constructor_assignments = Vec::new();

                buffer.push_str(" {\n");

                // Default constructor
                buffer.push_str(&format!("\tpublic {name}() {{ }}\n", name = name));

                let fields = transform_struct_fields(variants.fields());

                for field in fields.iter() {
                    let name = field.name().to_camel_case();
                    let struct_field = field.struct_field();
                    let mut ty = rust_to_java(&*struct_field.ty, &self.context)?
                        .unwrap_or_default();

                    if let &StructField::Array { .. } = field {
                        // Detect array ptrs: skip the length args and add array to the type sig
                        ty.push_str("[]");
                    }

                    buffer.push_str(&format!("\tprivate {} {};\n\n", ty, name));

                    buffer.push_str(&format!(
                        "\tpublic {ty} get{capitalized}() {{\n\t\treturn {name};\n\t}}\n\n",
                        ty = ty,
                        name = name,
                        capitalized = name.to_class_case(),
                    ));
                    buffer.push_str(&format!(
                        "\tpublic void set{capitalized}(final {ty} val) {{\n\t\t{name} \
                         = val;\n\t}}\n\n",
                        ty = ty,
                        name = name,
                        capitalized = name.to_class_case(),
                    ));

                    constructor_fields.push(format!("{} {}", ty, name));
                    constructor_assignments.push(format!("\t\tthis.{name} = {name};", name = name));
                }

                // Parametrised constructor
                buffer.push_str(&format!(
                    "\tpublic {name}({constructor_fields}) {{\n{constructor_assignments}\n}}\n",
                    name = name,
                    constructor_fields = constructor_fields.join(", "),
                    constructor_assignments =
                        constructor_assignments.join("\n")
                ));

                buffer.push_str("}");

                let jni = jni::generate_struct(&fields, &orig_name, &name, &self.context);
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

        outputs.insert(From::from(format!("{}.java", name)), buffer);

        Ok(())
    }

    fn finalise_output(&mut self, outputs: &mut Outputs) -> Result<(), Error> {
        match outputs.get_mut(&PathBuf::from("jni.rs")) {
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

        match outputs.get_mut(&PathBuf::from("NativeBindings.java")) {
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

/// Transform a struct name into a Java class name
pub fn struct_to_java_classname<S: AsRef<str>>(s: S) -> String {
    // s.as_ref().to_class_case()
    let mut c = s.as_ref().chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// Get the Java interface name for the callback based on its types
pub fn callback_name(inputs: &[ast::Arg], context: &Context) -> Result<String, Error> {
    let mut components = Vec::new();
    let mut inputs = inputs.iter().peekable();

    while let Some(arg) = inputs.next() {
        if is_user_data_arg(arg) {
            // Skip user_data args
            continue;
        }
        if is_result_arg(arg) {
            // Make sure that a CB taking a single "result: *const FfiResult" param
            // won't end up being called "CallbackVoid" (but "CallbackResult" instead)
            components.push(From::from("Result"));
            continue;
        }

        let mut arg_type = rust_ty_to_java_class_name(&*arg.ty, context)?
            .map(struct_to_java_classname)
            .unwrap_or_default();

        if is_array_arg(&arg, inputs.peek().cloned()) {
            inputs.next();
            arg_type.push_str("ArrayLen");
        }

        components.push(arg_type);
    }

    if components.is_empty() {
        Ok(From::from("CallbackVoid"))
    } else {
        Ok(format!("Callback{}", components.join("")))
    }
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
        let mut java_type = rust_to_java(&arg.ty, context)?.unwrap_or_default();

        if is_array_arg(&arg, fn_args.peek().cloned()) {
            // This is an array, so add it to the type description
            java_type.push_str("[]");

            // Skip the length args - e.g. for a case of `ptr: *const u8, ptr_len: usize`
            // we're going to skip the `len` part.
            fn_args.next();
        }

        args_str.push(format!("{} {}", java_type, arg_name.to_camel_case()));

        // Generate a callback class - if it wasn't generated already
        if let ast::TyKind::BareFn(ref bare_fn) = arg.ty.node {
            let cb_class = callback_name(&*bare_fn.decl.inputs, context)?;
            let cb_file = PathBuf::from(format!("{}.java", cb_class));

            if let None = outputs.get(&cb_file) {
                eprintln!("Generating CB {}", cb_class);

                let cb_output = transform_callback(&*arg.ty, &cb_class, context)?
                    .unwrap_or_default();
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
        ast::FunctionRetTy::Ty(ref ty) => rust_to_java(&*ty, context)?.unwrap_or_default(),
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

    let mut jni =
        jni::generate_jni_function(fn_decl.inputs.clone(), name, &java_name, context, outputs);
    jni.push_str("\n");
    append_output(jni, "jni.rs", outputs);

    Ok(())
}

/// Turn a Rust callback function type into a Java interface.
pub fn transform_callback<S: AsRef<str>>(
    ty: &ast::Ty,
    class_name: S,
    context: &Context,
) -> Result<Option<String>, Error> {
    match ty.node {
        ast::TyKind::BareFn(ref bare_fn) => Ok(Some(format!(
            "package {namespace};\n\n\
             public interface {name} {{\n\
             \tpublic void call({types});\n}}\n",
            namespace = context.namespace_model,
            name = class_name.as_ref(),
            types = try_some!(callback_to_java(bare_fn, ty.span, context)),
        ))),
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
) -> Result<Option<String>, Error> {
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {}
        _ => return Ok(None),
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err(Error {
            level: Level::Error,
            span: Some(fn_span),
            message: "cheddar can not handle lifetimes".into(),
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
        let mut java_type = try_some!(rust_to_java(&*arg.ty, context));

        if is_array_arg(&arg, args_iter.peek().cloned()) {
            // Detect array ptrs: skip the length args and add array to the type sig
            java_type.push_str("[]");
            args_iter.next();
        }

        args.push(format!("{} {}", java_type, arg_name.to_camel_case()));
    }

    Ok(Some(args.join(", ")))
}

/// Converts a callback function argument into a Java interface name
fn callback_arg_to_java(
    fn_ty: &ast::BareFnTy,
    fn_span: codemap::Span,
    context: &Context,
) -> Result<Option<String>, Error> {
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {}
        _ => return Ok(None),
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err(Error {
            level: Level::Error,
            span: Some(fn_span),
            message: "can not handle lifetimes".into(),
        });
    }

    Ok(Some(callback_name(&*fn_ty.decl.inputs, context)?))
}

/// Turn a Rust type with an associated name or type into a C type.
pub fn rust_to_java(ty: &ast::Ty, context: &Context) -> Result<Option<String>, Error> {
    match ty.node {
        // This is a callback ref taken as a function argument
        ast::TyKind::BareFn(ref bare_fn) => callback_arg_to_java(bare_fn, ty.span, context),

        // All other types just have a name associated with them.
        _ => anon_rust_to_java(ty, context, true),
    }
}

/// Turn a Rust type into a part of the Java class name.
/// Handles the size types in a special way because Rust has to distinguish
/// between usize and u64, that's required for JNI bindings to work properly.
fn rust_ty_to_java_class_name(ty: &ast::Ty, context: &Context) -> Result<Option<String>, Error> {
    match ty.node {
        ast::TyKind::Path(None, ref path) => {
            let primitive_type: &str = &path.segments[0].identifier.name.as_str();
            if primitive_type == "usize" || primitive_type == "isize" {
                Ok(Some(From::from("size")))
            } else {
                path_to_java(path, context, false)
            }
        }
        _ => anon_rust_to_java(ty, context, false),
    }
}

/// Turn a Rust type into a Java type signature.
fn anon_rust_to_java(
    ty: &ast::Ty,
    context: &Context,
    use_type_map: bool,
) -> Result<Option<String>, Error> {
    match ty.node {
        // Function pointers should not be in this function.
        ast::TyKind::BareFn(..) => Err(Error {
            level: Level::Error,
            span: Some(ty.span),
            message: "C function pointers must have a name or function declaration \
                      associated with them"
                .into(),
        }),

        // Standard pointers.
        ast::TyKind::Ptr(ref ptr) => {
            // Detect strings, which are *const c_char or *mut c_char
            if pprust::ty_to_string(&ptr.ty) == "c_char" {
                return Ok(Some("String".into()));
            }
            anon_rust_to_java(&ptr.ty, context, use_type_map)
        }

        // Plain old types.
        ast::TyKind::Path(None, ref path) => path_to_java(path, context, use_type_map),

        // Possibly void, likely not.
        _ => {
            let new_type = pprust::ty_to_string(ty);
            if new_type == "()" {
                Ok(Some("void".into()))
            } else {
                Err(Error {
                    level: Level::Error,
                    span: Some(ty.span),
                    message: format!("cheddar can not handle the type `{}`", new_type),
                })
            }
        }
    }
}

/// Convert a Rust path type (my_mod::MyType) to a C type.
///
/// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
/// types in `libc` which we special case.
fn path_to_java(
    path: &ast::Path,
    context: &Context,
    use_type_map: bool,
) -> Result<Option<String>, Error> {
    // I don't think this is possible.
    if path.segments.is_empty() {
        Err(Error {
            level: Level::Bug,
            span: Some(path.span),
            message: "what the fuck have you done to this type?!".into(),
        })
    // Types in modules, `my_mod::MyType`.
    } else if path.segments.len() > 1 {
        let (ty, module) = path.segments.split_last().expect(
            "already checked that there were at least two elements",
        );
        let ty: &str = &ty.identifier.name.as_str();
        let mut segments = Vec::with_capacity(module.len());
        for segment in module {
            segments.push(String::from(&*segment.identifier.name.as_str()));
        }
        let module = segments.join("::");
        match &*module {
            "libc" => Ok(Some(libc_ty_to_java(ty).into())),
            "std::os::raw" => Ok(Some(osraw_ty_to_java(ty).into())),
            _ => Err(Error {
                level: Level::Error,
                span: Some(path.span),
                message: "cheddar can not handle types in other modules \
                          (except `libc` and `std::os::raw`)"
                    .into(),
            }),
        }
    } else {
        let ty: &str = &path.segments[0].identifier.name.as_str();
        let mapped = rust_ty_to_java(ty, context, use_type_map);

        Ok(Some(if mapped == ty {
            // Capitalise custom types, which are structs (most likely)
            struct_to_java_classname(ty)
        } else {
            mapped.into()
        }))
    }
}

/// Convert a Rust type from `libc` into a C type.
///
/// Most map straight over but some have to be converted.
fn libc_ty_to_java(ty: &str) -> &str {
    match ty {
        "c_bool" => "boolean",
        "c_void" => "void",
        "c_float" => "float",
        "c_double" => "double",
        "c_char" => "byte",
        "c_schar" => "byte",
        "c_uchar" => "byte",
        "c_short" => "short",
        "c_ushort" => "short",
        "c_int" => "int",
        "c_uint" => "int",
        "c_long" => "long",
        "c_ulong" => "long",
        // All other types should map over to C or explicitly defined mapping.
        ty => ty,
    }
}

/// Convert a Rust type from `std::os::raw` into a C type.
///
/// These mostly mirror the libc crate.
fn osraw_ty_to_java(ty: &str) -> &str {
    match ty {
        "c_void" => "void",
        "c_char" => "byte",
        "c_double" => "double",
        "c_float" => "float",
        "c_int" => "int",
        "c_long" => "long",
        "c_schar" => "byte",
        "c_short" => "short",
        "c_uchar" => "byte",
        "c_uint" => "int",
        "c_ulong" => "long",
        "c_ushort" => "short",
        // All other types should map as-is
        ty => ty,
    }
}

/// Convert any Rust type into C.
///
/// This includes user-defined types. We currently trust the user not to use types which we don't
/// know the structure of (like String).
fn rust_ty_to_java<'a>(ty: &'a str, context: &Context, use_type_map: bool) -> &'a str {
    match ty {
        "()" => "void",
        "bool" => "boolean",
        "f32" => "float",
        "f64" => "double",
        "u8" | "i8" => "byte",
        "u16" | "i16" => "short",
        "u32" | "i32" => "int",
        "u64" | "i64" => "long",
        "usize" | "isize" => "long",
        ty if use_type_map => {
            if let Some(mapping) = context.type_map.get(ty) {
                mapping
            } else {
                libc_ty_to_java(ty)
            }
        }
        ty => libc_ty_to_java(ty),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntax::ast::{Arg, ItemKind};
    use syntax::parse::{self, ParseSess};

    #[test]
    fn cb_names() {
        fn get_inputs(source: &str) -> Vec<Arg> {
            let parse_sess = ParseSess::new();

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

        let inputs = get_inputs(
            "fn dummy(user_data: *mut c_void, result: *const FfiResult) {}",
        );
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
