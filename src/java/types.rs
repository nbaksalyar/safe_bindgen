//! Functions for converting Rust types to Java types.

use {Error, Level};
use common::{is_array_arg, is_result_arg, is_user_data_arg};
use java::Context;
use jni::signature::{JavaType, Primitive};
use syntax::{ast, codemap};
use syntax::abi::Abi;
use syntax::print::pprust;

fn primitive_type_to_str(ty: &Primitive) -> &str {
    match *ty {
        Primitive::Boolean => "boolean",
        Primitive::Byte => "byte",
        Primitive::Char => "char",
        Primitive::Double => "double",
        Primitive::Float => "float",
        Primitive::Int => "int",
        Primitive::Long => "long",
        Primitive::Short => "short",
        Primitive::Void => "void",
    }
}

/// Converts `JavaType` into Java code
pub fn java_type_to_str(ty: &JavaType) -> Result<String, Error> {
    match *ty {
        JavaType::Primitive(ref primitive) => Ok(primitive_type_to_str(primitive).to_string()),
        JavaType::Object(ref obj) => {
            match obj.as_str() {
                "java/lang/String" => Ok("String".into()),
                _ => Ok(obj.to_string()),
            }
        }
        JavaType::Array(ref boxed) => Ok(format!("{}[]", java_type_to_str(&*boxed)?)),
        JavaType::Method(..) => Err(Error {
            level: Level::Error,
            span: None,
            message: "Java methods are not supported".into(),
        }),
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

        let arg_type = &rust_ty_to_java_class_name(&*arg.ty, context)?;
        let mut arg_type = struct_to_java_classname(arg_type);

        if is_array_arg(arg, inputs.peek().cloned()) {
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

/// Converts a callback function argument into a Java interface name
fn callback_arg_to_java(
    fn_ty: &ast::BareFnTy,
    fn_span: codemap::Span,
    context: &Context,
) -> Result<JavaType, Error> {
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

    Ok(JavaType::Object(
        callback_name(&*fn_ty.decl.inputs, context)?,
    ))
}

/// Turn a Rust type with an associated name or type into a C type.
pub fn rust_to_java(ty: &ast::Ty, context: &Context) -> Result<JavaType, Error> {
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
fn rust_ty_to_java_class_name(ty: &ast::Ty, context: &Context) -> Result<String, Error> {
    match ty.node {
        ast::TyKind::Path(None, ref path) => {
            let primitive_type: &str = &path.segments[0].identifier.name.as_str();
            if primitive_type == "usize" || primitive_type == "isize" {
                Ok(From::from("size"))
            } else {
                java_type_to_str(&path_to_java(path, context, false)?)
            }
        }
        _ => java_type_to_str(&anon_rust_to_java(ty, context, false)?),
    }
}

/// Turn a Rust type into a Java type signature.
fn anon_rust_to_java(
    ty: &ast::Ty,
    context: &Context,
    use_type_map: bool,
) -> Result<JavaType, Error> {
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
                return Ok(JavaType::Object("java/lang/String".into()));
            }
            anon_rust_to_java(&ptr.ty, context, use_type_map)
        }

        // Plain old types.
        ast::TyKind::Path(None, ref path) => path_to_java(path, context, use_type_map),

        // Possibly void, likely not.
        _ => {
            let new_type = pprust::ty_to_string(ty);
            if new_type == "()" {
                Ok(JavaType::Primitive(Primitive::Void))
            } else {
                Err(Error {
                    level: Level::Error,
                    span: Some(ty.span),
                    message: format!("unknown type `{}`", new_type),
                })
            }
        }
    }
}

/// Convert a Rust path type (`my_mod::MyType`) to a Java type.
///
/// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
/// types in `libc` which we special case.
fn path_to_java(
    path: &ast::Path,
    context: &Context,
    use_type_map: bool,
) -> Result<JavaType, Error> {
    if path.segments.is_empty() {
        return Err(Error {
            level: Level::Bug,
            span: Some(path.span),
            message: "invalid type".into(),
        });
    }

    // Types in modules, `my_mod::MyType`.
    if path.segments.len() > 1 {
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
            "libc" => Ok(libc_ty_to_java(ty).into()),
            "std::os::raw" => Ok(libc_ty_to_java(ty).into()),
            _ => Err(Error {
                level: Level::Error,
                span: Some(path.span),
                message: "can't convert type".into(),
            }),
        }
    } else {
        let ty: &str = &path.segments[0].identifier.name.as_str();
        let mapped = rust_ty_to_java(ty, context, use_type_map);
        /*
        Ok(Some(if mapped == ty {
            // Capitalise custom types, which are structs (most likely)
            struct_to_java_classname(ty)
        } else {
            mapped.into()
        }))
         */
        Ok(mapped)
    }
}

/// Convert a Rust type from `libc` and `std::os::raw` into a Java type.
/// Most map straight over but some have to be converted.
fn libc_ty_to_java(ty: &str) -> JavaType {
    match ty {
        "c_bool" => JavaType::Primitive(Primitive::Boolean),
        "c_void" => JavaType::Primitive(Primitive::Void),
        "c_float" => JavaType::Primitive(Primitive::Float),
        "c_double" => JavaType::Primitive(Primitive::Double),
        "c_char" | "c_schar" | "c_uchar" => JavaType::Primitive(Primitive::Byte),
        "c_short" | "c_ushort" => JavaType::Primitive(Primitive::Short),
        "c_int" | "c_uint" => JavaType::Primitive(Primitive::Int),
        "c_long" | "c_ulong" => JavaType::Primitive(Primitive::Long),
        // All other types should map over to Java classes or explicitly defined mapping.
        ty => JavaType::Object(ty.to_string()),
    }
}

/// Convert any Rust type into Java.
///
/// This includes user-defined types. We currently trust the user not to use types which we don't
/// know the structure of (like String).
fn rust_ty_to_java<'a>(ty: &'a str, context: &Context, use_type_map: bool) -> JavaType {
    match ty {
        "()" => JavaType::Primitive(Primitive::Void), // "void",
        "bool" => JavaType::Primitive(Primitive::Boolean), // "boolean",
        "f32" => JavaType::Primitive(Primitive::Float), // "float",
        "f64" => JavaType::Primitive(Primitive::Double), // "double",
        "u8" | "i8" => JavaType::Primitive(Primitive::Byte), // "byte",
        "u16" | "i16" => JavaType::Primitive(Primitive::Short), // "short",
        "u32" | "i32" => JavaType::Primitive(Primitive::Int), // "int",
        "u64" | "i64" | "usize" | "isize" => JavaType::Primitive(Primitive::Long), // "long",
        ty if use_type_map => {
            if let Some(mapping) = context.type_map.get(ty) {
                (*mapping).clone()
            } else {
                libc_ty_to_java(ty)
            }
        }
        ty => libc_ty_to_java(ty),
    }
}
