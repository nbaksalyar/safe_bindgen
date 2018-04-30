//! Functions for converting Rust types to C types.

use Error;
use Level;
use common::{Lang, Outputs, append_output, check_no_mangle, check_repr_c, parse_attr,
             retrieve_docstring};
use std::path::Path;
use syntax::{ast, codemap, print};
use syntax::abi::Abi;
use syntax::print::pprust;

pub struct LangC {
    lib_name: String,
}

/// Compile the header declarations then add the needed `#include`s.
///
/// Currently includes:
///
/// - `stdint.h`
/// - `stdbool.h`

impl LangC {
    pub fn new() -> Self {
        Self { lib_name: "backend".to_owned() }
    }

    /// Set the name of the native library.
    pub fn set_lib_name<T: Into<String>>(&mut self, name: T) {
        self.lib_name = name.into();
    }
}

impl Default for LangC {
    fn default() -> Self {
        Self::new()
    }
}

impl Lang for LangC {
    /// Convert `pub type A = B;` into `typedef B A;`.
    ///
    /// Aborts if A is generic.
    fn parse_ty(
        &mut self,
        item: &ast::Item,
        module: &str,
        outputs: &mut Outputs,
    ) -> Result<(), Error> {
        let (_, docs) = parse_attr(&item.attrs, |_| true, |attr| retrieve_docstring(attr, ""));

        let mut buffer = String::new();
        buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        let new_type = match item.node {
            ast::ItemKind::Ty(ref ty, ref generics) => {
                // Can not yet convert generics.
                if generics.is_parameterized() {
                    return Ok(());
                }

                rust_to_c(&*ty, &name)?
            }
            _ => {
                return Err(Error {
                    level: Level::Bug,
                    span: Some(item.span),
                    message: "`parse_ty` called on wrong `Item_`".into(),
                });
            }
        };

        buffer.push_str(&format!("typedef {};\n\n", new_type));

        append_output(buffer, &header_name(module)?, outputs);

        Ok(())
    }

    /// Convert a Rust enum into a C enum.
    ///
    /// The Rust enum must be marked with `#[repr(C)]` and must be public otherwise the function
    /// will abort.
    ///
    /// Bindgen will error if the enum if generic or if it contains non-unit variants.
    fn parse_enum(
        &mut self,
        item: &ast::Item,
        module: &str,
        outputs: &mut Outputs,
    ) -> Result<(), Error> {
        let (repr_c, docs) = parse_attr(
            &item.attrs,
            check_repr_c,
            |attr| retrieve_docstring(attr, ""),
        );
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c {
            return Ok(());
        }

        let mut buffer = String::new();
        buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let ast::ItemKind::Enum(ref definition, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "bindgen can not handle parameterized `#[repr(C)]` enums".into(),
                });
            }

            for var in &definition.variants {
                if !var.node.data.is_unit() {
                    return Err(Error {
                        level: Level::Error,
                        span: Some(var.span),
                        message: "bindgen can not handle `#[repr(C)]` enums with non-unit variants"
                            .into(),
                    });
                }

                let (_, docs) = parse_attr(
                    &var.node.attrs,
                    |_| true,
                    |attr| retrieve_docstring(attr, "\t"),
                );
                buffer.push_str(&docs);

                buffer.push_str(&format!("\t{}_{},\n", name, pprust::variant_to_string(var)));
            }
        } else {
            return Err(Error {
                level: Level::Bug,
                span: Some(item.span),
                message: "`parse_enum` called on wrong `Item_`".into(),
            });
        }

        buffer.push_str(&format!("}} {};\n\n", name));

        append_output(buffer, &header_name(module)?, outputs);

        Ok(())
    }

    /// Convert a Rust struct into a C struct.
    ///
    /// The rust struct must be marked `#[repr(C)]` and must be public otherwise the function will
    /// abort.
    ///
    /// Bindgen will error if the struct is generic or if the struct is a unit or tuple struct.
    fn parse_struct(
        &mut self,
        item: &ast::Item,
        module: &str,
        outputs: &mut Outputs,
    ) -> Result<(), Error> {
        let (repr_c, docs) = parse_attr(
            &item.attrs,
            check_repr_c,
            |attr| retrieve_docstring(attr, ""),
        );
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c {
            return Ok(());
        }

        let mut buffer = String::new();
        buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        buffer.push_str(&format!("typedef struct {}", name));

        if let ast::ItemKind::Struct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "bindgen can not handle parameterized `#[repr(C)]` structs".into(),
                });
            }

            if variants.is_struct() {
                buffer.push_str(" {\n");

                for field in variants.fields() {
                    let (_, docs) = parse_attr(
                        &field.attrs,
                        |_| true,
                        |attr| retrieve_docstring(attr, "\t"),
                    );
                    buffer.push_str(&docs);

                    let name = match field.ident {
                        Some(name) => name.name.as_str(),
                        None => unreachable!("a tuple struct snuck through"),
                    };
                    let ty = rust_to_c(&*field.ty, &name)?;
                    buffer.push_str(&format!("\t{};\n", ty));
                }

                buffer.push_str("}");
            } else if variants.is_tuple() && variants.fields().len() == 1 {
                // #[repr(C)] pub struct Foo(Bar);  =>  typedef struct Foo Foo;
            } else {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "bindgen can not handle unit or tuple `#[repr(C)]` structs with >1 members"
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

        buffer.push_str(&format!(" {};\n\n", name));

        append_output(buffer, &header_name(module)?, outputs);

        Ok(())
    }

    /// Convert a Rust function declaration into a C function declaration.
    ///
    /// The function declaration must be marked `#[no_mangle]` and have a C ABI otherwise the
    /// function will abort.
    ///
    /// If the declaration is generic or diverges then bindgen will error.
    fn parse_fn(
        &mut self,
        item: &ast::Item,
        module: &str,
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
            match abi {
                // If it doesn't have a C ABI it can't be called from C.
                Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {}
                _ => return Ok(()),
            }

            if generics.is_parameterized() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(item.span),
                    message: "bindgen can not handle parameterized extern functions".into(),
                });
            }

            transform_native_fn(&*fn_decl, &docs, &format!("{}", name), module, outputs)?;

            Ok(())
        } else {
            Err(Error {
                level: Level::Bug,
                span: Some(item.span),
                message: "`parse_fn` called on wrong `Item_`".into(),
            })
        }
    }

    fn finalise_output(&mut self, outputs: &mut Outputs) -> Result<(), Error> {
        let mut module_includes = String::new();

        for (module, value) in outputs.iter_mut() {
            let header_name = module.to_str().expect("invalid module path");
            let code = format!("#include <stdint.h>\n#include <stdbool.h>\n\n{}", value);

            *value = wrap_guard(&wrap_extern(&code), header_name);

            module_includes.push_str(&format!("#include \"{}\"\n", header_name));
        }

        outputs.insert(From::from(format!("{}.h", self.lib_name)), module_includes);

        Ok(())
    }
}

/// Transform a Rust FFI function into a C function decl
pub fn transform_native_fn(
    fn_decl: &ast::FnDecl,
    docs: &str,
    name: &str,
    module: &str,
    outputs: &mut Outputs,
) -> Result<(), Error> {
    // Handle the case when the return type is a function pointer (which requires that the
    // entire declaration is wrapped by the function pointer type) by first creating the name
    // and parameters, then passing that whole thing to `rust_to_c`.
    let fn_args = fn_decl.inputs.clone();
    let mut args = Vec::new();

    // Arguments
    for arg in &fn_args {
        let arg_name = pprust::pat_to_string(&*arg.pat);

        args.push(rust_to_c(&arg.ty, &arg_name)?);
    }

    let buf = format!(
        "{}({})",
        name,
        if args.is_empty() {
            String::from("void")
        } else {
            args.join(", ")
        }
    );

    // Generate return type
    let output_type = &fn_decl.output;
    let full_declaration = match *output_type {
        ast::FunctionRetTy::Ty(ref ty) if ty.node == ast::TyKind::Never => {
            return Err(Error {
                level: Level::Error,
                span: Some(ty.span),
                message: "panics across a C boundary are naughty!".into(),
            });
        }
        ast::FunctionRetTy::Default(..) => format!("void {}", buf),
        ast::FunctionRetTy::Ty(ref ty) => rust_to_c(&*ty, &buf)?,
    };

    let mut output = String::new();
    output.push_str(docs);
    output.push_str(&full_declaration);
    output.push_str(";\n\n");

    append_output(output, &header_name(module)?, outputs);

    Ok(())
}

/// Turn a Rust type with an associated name or type into a C type.
pub fn rust_to_c(ty: &ast::Ty, assoc: &str) -> Result<String, Error> {
    match ty.node {
        // Function pointers make life an absolute pain here.
        ast::TyKind::BareFn(ref bare_fn) => fn_ptr_to_c(bare_fn, ty.span, assoc),
        // Special case Options wrapping function pointers.
        ast::TyKind::Path(None, ref path) => {
            if path.segments.len() == 1 && path.segments[0].identifier.name == "Option" {
                if let Some(ref param) = path.segments[0].parameters {
                    if let ast::PathParameters::AngleBracketed(ref d) = **param {
                        assert!(d.lifetimes.is_empty() && d.bindings.is_empty());
                        if d.types.len() == 1 {
                            if let ast::TyKind::BareFn(ref bare_fn) = d.types[0].node {
                                return fn_ptr_to_c(bare_fn, ty.span, assoc);
                            }
                        }
                    }
                }
            }

            Ok(format!("{} {}", anon_rust_to_c(ty)?, assoc))
        }
        // All other types just have a name associated with them.
        _ => Ok(format!("{} {}", anon_rust_to_c(ty)?, assoc)),
    }
}

/// Turn a Rust type into a C type.
fn anon_rust_to_c(ty: &ast::Ty) -> Result<String, Error> {
    match ty.node {
        // Function pointers should not be in this function.
        ast::TyKind::BareFn(..) => Err(Error {
            level: Level::Error,
            span: Some(ty.span),
            message: "C function pointers must have a name or function declaration associated with them"
                .into(),
        }),
        // Fixed-length arrays, converted into pointers.
        ast::TyKind::Array(ref ty, _) => Ok(format!("*{}", anon_rust_to_c(ty)?)),
        // Standard pointers.
        ast::TyKind::Ptr(ref ptr) => ptr_to_c(ptr),
        // Plain old types.
        ast::TyKind::Path(None, ref path) => path_to_c(path),
        // Possibly void, likely not.
        _ => {
            let new_type = print::pprust::ty_to_string(ty);
            if new_type == "()" {
                Ok("void".into())
            } else {
                Err(Error {
                    level: Level::Error,
                    span: Some(ty.span),
                    message: format!("bindgen can not handle the type `{}`", new_type),
                })
            }
        }
    }
}

/// Turn a Rust pointer (*mut or *const) into the correct C form.
fn ptr_to_c(ty: &ast::MutTy) -> Result<String, Error> {
    let new_type = anon_rust_to_c(&ty.ty)?;
    let const_spec = match ty.mutbl {
        // *const T
        ast::Mutability::Immutable => " const",
        // *mut T
        ast::Mutability::Mutable => "",
    };

    Ok(format!("{}{}*", new_type, const_spec))
}

/// Turn a Rust function pointer into a C function pointer.
///
/// Rust function pointers are of the form
///
/// ```ignore
/// fn(arg1: Ty1, ...) -> RetTy
/// ```
///
/// C function pointers are of the form
///
/// ```C
/// RetTy (*inner)(Ty1 arg1, ...)
/// ```
///
/// where `inner` could either be a name or the rest of a function declaration.
fn fn_ptr_to_c(
    fn_ty: &ast::BareFnTy,
    fn_span: codemap::Span,
    inner: &str,
) -> Result<String, Error> {
    /*
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {}
        _ => return Ok(None),
    }
*/
    if !fn_ty.lifetimes.is_empty() {
        return Err(Error {
            level: Level::Error,
            span: Some(fn_span),
            message: "bindgen can not handle lifetimes".into(),
        });
    }

    let fn_decl: &ast::FnDecl = &*fn_ty.decl;

    let mut buf_without_return = format!("(*{})(", inner);

    let has_args = !fn_decl.inputs.is_empty();

    for arg in &fn_decl.inputs {
        let arg_name = print::pprust::pat_to_string(&*arg.pat);
        let arg_type = rust_to_c(&*arg.ty, &arg_name)?;
        buf_without_return.push_str(&arg_type);
        buf_without_return.push_str(", ");
    }

    if has_args {
        // Remove the trailing comma and space.
        buf_without_return.pop();
        buf_without_return.pop();
    } else {
        buf_without_return.push_str("void");
    }

    buf_without_return.push(')');

    let output_type = &fn_decl.output;
    let full_declaration = match *output_type {
        ast::FunctionRetTy::Ty(ref ty) if ty.node == ast::TyKind::Never => {
            return Err(Error {
                level: Level::Error,
                span: Some(ty.span),
                message: "panics across a C boundary are naughty!".into(),
            });
        }
        ast::FunctionRetTy::Default(..) => format!("void {}", buf_without_return),
        ast::FunctionRetTy::Ty(ref ty) => rust_to_c(&*ty, &buf_without_return)?,
    };


    Ok(full_declaration)
}

/// Convert a Rust path type (e.g. `my_mod::MyType`) to a C type.
///
/// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
/// types in `libc` which we special case.
fn path_to_c(path: &ast::Path) -> Result<String, Error> {
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
            "libc" => Ok(libc_ty_to_c(ty).into()),
            "std::os::raw" => Ok(osraw_ty_to_c(ty).into()),
            _ => Err(Error {
                level: Level::Error,
                span: Some(path.span),
                message: "bindgen can not handle types in other modules (except `libc` and `std::os::raw`)"
                    .into(),
            }),
        }
    } else {
        Ok(
            rust_ty_to_c(&path.segments[0].identifier.name.as_str()).into(),
        )
    }
}

/// Convert a Rust type from `libc` into a C type.
///
/// Most map straight over but some have to be converted.
fn libc_ty_to_c(ty: &str) -> &str {
    match ty {
        "c_void" => "void",
        "c_float" => "float",
        "c_double" => "double",
        "c_char" => "char",
        "c_schar" => "signed char",
        "c_uchar" => "unsigned char",
        "c_short" => "short",
        "c_ushort" => "unsigned short",
        "c_int" => "int",
        "c_uint" => "unsigned int",
        "c_long" => "long",
        "c_ulong" => "unsigned long",
        "c_longlong" => "long long",
        "c_ulonglong" => "unsigned long long",
        // All other types should map over to C.
        ty => ty,
    }
}

/// Convert a Rust type from `std::os::raw` into a C type.
///
/// These mostly mirror the libc crate.
fn osraw_ty_to_c(ty: &str) -> &str {
    match ty {
        "c_void" => "void",
        "c_char" => "char",
        "c_double" => "double",
        "c_float" => "float",
        "c_int" => "int",
        "c_long" => "long",
        "c_longlong" => "long long",
        "c_schar" => "signed char",
        "c_short" => "short",
        "c_uchar" => "unsigned char",
        "c_uint" => "unsigned int",
        "c_ulong" => "unsigned long",
        "c_ulonglong" => "unsigned long long",
        "c_ushort" => "unsigned short",
        // All other types should map over to C.
        ty => ty,
    }
}

/// Convert any Rust type into C.
///
/// This includes user-defined types. We currently trust the user not to use types which we don't
/// know the structure of (like String).
fn rust_ty_to_c(ty: &str) -> &str {
    match ty {
        "()" => "void",
        "f32" => "float",
        "f64" => "double",
        "i8" => "int8_t",
        "i16" => "int16_t",
        "i32" => "int32_t",
        "i64" => "int64_t",
        "isize" => "intptr_t",
        "u8" => "uint8_t",
        "u16" => "uint16_t",
        "u32" => "uint32_t",
        "u64" => "uint64_t",
        "usize" => "uintptr_t",
        // Fallback to libc because these types could be not fully-qualified ones:
        // https://github.com/mozilla/moz-cheddar/issues/7
        ty => libc_ty_to_c(ty),
    }
}


/// Wrap a block of code with an extern declaration.
fn wrap_extern(code: &str) -> String {
    format!(
        r#"
#ifdef __cplusplus
extern "C" {{
#endif

{}

#ifdef __cplusplus
}}
#endif
"#,
        code
    )
}

/// Wrap a block of code with an include-guard.
fn wrap_guard(code: &str, id: &str) -> String {
    format!(
        r"
#ifndef bindgen_{0}
#define bindgen_{0}

 {1}

#endif
",
        sanitise_id(id),
        code
    )
}

/// Transform a module name into a header name
fn header_name(module: &str) -> Result<String, Error> {
    let path = Path::new(module);
    let module_stem = unwrap!(unwrap!(path.file_stem()).to_str());

    let header_name = if module_stem == "lib" {
        // Unwrap lib name - <lib_name>/src/lib.rs
        unwrap!(unwrap!(unwrap!(unwrap!(path.parent()).parent()).file_name()).to_str())
    } else if module_stem == "mod" {
        // Unwrap module name - <mod_name>/mod.rs
        unwrap!(unwrap!(unwrap!(path.parent()).file_name()).to_str())
    } else {
        module_stem
    };

    Ok(format!("{}.h", header_name))
}

/// Remove illegal characters from the identifier.
///
/// This is because macros names must be valid C identifiers. Note that the identifier will always
/// be concatenated onto `cheddar_generated_` so can start with a digit.
pub fn sanitise_id(id: &str) -> String {
    // `char.is_digit(36)` ensures `char` is in `[A-Za-z0-9]`
    id.chars()
        .filter(|ch| ch.is_digit(36) || *ch == '_')
        .collect()
}


#[cfg(test)]
mod test {
    #[test]
    fn sanitise_id() {
        assert!(super::sanitise_id("") == "");
        assert!(super::sanitise_id("!@£$%^&*()_+") == "_");
        // https://github.com/Sean1708/rusty-cheddar/issues/29
        assert!(super::sanitise_id("filename.h") == "filenameh");
    }

    fn ty(source: &str) -> ::syntax::ast::Ty {
        let sess = ::syntax::parse::ParseSess::new();
        let result = {
            let mut parser =
                ::syntax::parse::new_parser_from_source_str(&sess, "".into(), source.into());
            parser.parse_ty()
        };

        match result {
            Ok(p) => (*p).clone(),
            _ => {
                panic!(
                    "internal testing error: could not parse type from {:?}",
                    source
                )
            }
        }
    }

    #[test]
    fn pure_rust_types() {
        let type_map = [
            ("()", "void"),
            ("f32", "float"),
            ("f64", "double"),
            ("i8", "int8_t"),
            ("i16", "int16_t"),
            ("i32", "int32_t"),
            ("i64", "int64_t"),
            ("isize", "intptr_t"),
            ("u8", "uint8_t"),
            ("u16", "uint16_t"),
            ("u32", "uint32_t"),
            ("u64", "uint64_t"),
            ("usize", "uintptr_t"),
        ];

        let name = "gabriel";

        for &(rust_type, correct_c_type) in &type_map {
            let parsed_c_type = super::anon_rust_to_c(&ty(rust_type)).expect(&format!(
                "error while parsing {:?} with no name",
                rust_type
            ));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = super::rust_to_c(&ty(rust_type), name).expect(&format!(
                "error while parsing {:?} with name {:?}",
                rust_type,
                name
            ));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn libc_types() {
        let type_map = [
            ("libc::c_void", "void"),
            ("libc::c_float", "float"),
            ("libc::c_double", "double"),
            ("libc::c_char", "char"),
            ("libc::c_schar", "signed char"),
            ("libc::c_uchar", "unsigned char"),
            ("libc::c_short", "short"),
            ("libc::c_ushort", "unsigned short"),
            ("libc::c_int", "int"),
            ("libc::c_uint", "unsigned int"),
            ("libc::c_long", "long"),
            ("libc::c_ulong", "unsigned long"),
            ("libc::c_longlong", "long long"),
            ("libc::c_ulonglong", "unsigned long long"),
            // Some other common ones.
            ("libc::size_t", "size_t"),
            ("libc::dirent", "dirent"),
            ("libc::FILE", "FILE"),
        ];

        let name = "lucifer";

        for &(rust_type, correct_c_type) in &type_map {
            let parsed_c_type = super::anon_rust_to_c(&ty(rust_type)).expect(&format!(
                "error while parsing {:?} with no name",
                rust_type
            ));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = super::rust_to_c(&ty(rust_type), name).expect(&format!(
                "error while parsing {:?} with name {:?}",
                rust_type,
                name
            ));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn const_pointers() {
        let name = "maalik";

        let source = "*const u8";
        let parsed_type = super::anon_rust_to_c(&ty(source)).expect(&format!(
            "error while parsing {:?} with no name",
            source
        ));
        assert_eq!(parsed_type, "uint8_t const*");

        let source = "*const ()";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("void const* {}", name));

        let source = "*const *const f64";
        let parsed_type = super::anon_rust_to_c(&ty(source)).expect(&format!(
            "error while parsing {:?} with no name",
            source
        ));
        assert_eq!(parsed_type, "double const* const*");

        let source = "*const *const i64";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("int64_t const* const* {}", name));
    }

    #[test]
    fn mut_pointers() {
        let name = "raphael";

        let source = "*mut u16";
        let parsed_type = super::anon_rust_to_c(&ty(source)).expect(&format!(
            "error while parsing {:?} with no name",
            source
        ));
        assert_eq!(parsed_type, "uint16_t*");

        let source = "*mut f32";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("float* {}", name));

        let source = "*mut *mut *mut i32";
        let parsed_type = super::anon_rust_to_c(&ty(source)).expect(&format!(
            "error while parsing {:?} with no name",
            source
        ));
        assert_eq!(parsed_type, "int32_t***");

        let source = "*mut *mut i8";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("int8_t** {}", name));
    }

    #[test]
    fn mixed_pointers() {
        let name = "samael";

        let source = "*const *mut *const bool";
        let parsed_type = super::anon_rust_to_c(&ty(source)).expect(&format!(
            "error while parsing {:?} with no name",
            source
        ));
        assert_eq!(parsed_type, "bool const** const*");

        let source = "*mut *mut *const libc::c_ulonglong";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("unsigned long long const*** {}", name));

        let source = "*const *mut *mut i8";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("int8_t** const* {}", name));
    }

    #[test]
    fn function_pointers() {
        let name = "sariel";

        let source = "fn(a: bool)";
        let parsed_type = super::anon_rust_to_c(&ty(source));
        assert!(
            parsed_type.is_err(),
            "C function pointers should have an inner or name associated"
        );

        // let source = "fn(a: i8) -> f64";
        // let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
        //     "error while parsing {:?} with name {:?}",
        //     source,
        //     name
        // ));
        // assert!(parsed_type.is_none(), "parsed a non-C function pointer");

        let source = "extern fn(hi: libc::c_int) -> libc::c_double";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("double (*{})(int hi)", name));

        let source = "Option<extern fn(hi: libc::c_int) -> libc::c_double>";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("double (*{})(int hi)", name));
    }

    #[test]
    fn paths() {
        let name = "zachariel";

        let source = "MyType";
        let parsed_type = super::anon_rust_to_c(&ty(source)).expect(&format!(
            "error while parsing {:?} with no name",
            source
        ));
        assert_eq!(parsed_type, "MyType");

        let source = "SomeType";
        let parsed_type = super::rust_to_c(&ty(source), name).expect(&format!(
            "error while parsing {:?} with name {:?}",
            source,
            name
        ));
        assert_eq!(parsed_type, format!("SomeType {}", name));

        let source = "my_mod::MyType";
        let parsed_type = super::anon_rust_to_c(&ty(source));
        assert!(
            parsed_type.is_err(),
            "can't use a multi-segment path which isn't `libc`"
        );

        let source = "some_mod::SomeType";
        let parsed_type = super::rust_to_c(&ty(source), name);
        assert!(
            parsed_type.is_err(),
            "can't use a multi-segment path which isn't `libc`"
        );
    }
}
