//! Functions to generate JNI bindings

use common::{is_array_arg, is_user_data_arg};
use syntax::ast;
use syntax::print::pprust;
use java::callback_name;
use jni::signature::{self, TypeSignature, JavaType};
use quote;

fn to_jni_arg(arg: &ast::Arg, ty_name: &str) -> quote::Tokens {
    let pat = quote::Ident::new(pprust::pat_to_string(&*arg.pat));
    let ty_name = quote::Ident::new(ty_name);
    quote! { #pat: #ty_name }
}

fn transform_jni_arg(arg: &ast::Arg) -> quote::Tokens {
    match arg.ty.node {
        // Callback
        ast::TyKind::BareFn(ref _bare_fn) => to_jni_arg(arg, "JObject"),

        // Plain old types.
        ast::TyKind::Path(None, ref path) => {
            let (ty, _module) = path.segments.split_last().expect(
                "already checked that there were at least two elements",
            );
            let ty: &str = &ty.identifier.name.as_str();

            let jni_type = match ty {
                "c_char" | "u8" | "i8" => "jbyte",
                "c_short" | "u16" | "i16" => "jshort",
                "c_int" | "u32" | "i32" => "jint",
                "c_long" | "u64" | "i64" => "jlong",
                "c_usize" | "usize" => "jlong",
                _ => ty,
            };

            to_jni_arg(arg, jni_type)
        }

        // Standard pointers.
        ast::TyKind::Ptr(ref ptr) => {
            // Detect strings, which are *const c_char or *mut c_char
            if pprust::ty_to_string(&ptr.ty) == "c_char" {
                to_jni_arg(arg, "JString")
            } else {
                to_jni_arg(arg, "JObject")
            }
        }

        _ => to_jni_arg(arg, &pprust::ty_to_string(&arg.ty)),

    }
}

fn rust_ty_to_signature(ty: &ast::Ty) -> Option<JavaType> {
    match ty.node {
        // Callback
        ast::TyKind::BareFn(ref _bare_fn) => Some(JavaType::Object(From::from("java/lang/Object"))),

        // Plain old types.
        ast::TyKind::Path(None, ref path) => {
            let (ty, _module) = path.segments.split_last().expect(
                "already checked that there were at least two elements",
            );
            let ty: &str = &ty.identifier.name.as_str();

            match ty {
                "c_byte" | "u8" => Some(JavaType::Primitive(signature::Primitive::Byte)),
                "c_short" | "u16" => Some(JavaType::Primitive(signature::Primitive::Short)),
                "c_int" | "u32" => Some(JavaType::Primitive(signature::Primitive::Int)),
                "c_long" | "u64" => Some(JavaType::Primitive(signature::Primitive::Long)),
                "c_usize" | "usize" => Some(JavaType::Primitive(signature::Primitive::Long)),
                "c_bool" | "bool" => Some(JavaType::Object(From::from("java/lang/Boolean"))),
                _ => Some(JavaType::Object(From::from(ty))),
            }
        }

        // Standard pointers.
        ast::TyKind::Ptr(ref ptr) => {
            // Detect strings, which are *const c_char or *mut c_char
            if pprust::ty_to_string(&ptr.ty) == "c_char" {
                Some(JavaType::Object(From::from("java/lang/String")))
            } else {
                rust_ty_to_signature(&ptr.ty)
            }
        }

        _ => None,

    }
}

struct JniArgResult {
    stmt: quote::Tokens,
    call_args: Vec<quote::Tokens>,
}

fn transform_string_arg(arg_name: &str) -> JniArgResult {
    // statements
    let arg_name = quote::Ident::new(arg_name);
    let stmt =
        quote! {
            let #arg_name = CString::from_java(&env, #arg_name);
        };

    // call arg value(s)
    let call_args = vec![quote! { #arg_name.as_ptr() }];

    JniArgResult { stmt, call_args }
}

fn transform_struct_arg(arg_name: &str, arg_ty: &ast::Ty) -> JniArgResult {
    // statements
    let arg_name = quote::Ident::new(arg_name);
    let struct_ty = quote::Ident::new(pprust::ty_to_string(arg_ty));
    let stmt =
        quote! {
            let #arg_name = #struct_ty::from_java(&env, #arg_name);
        };

    // call arg value(s)
    let call_args = vec![quote! { &#arg_name }];

    JniArgResult { stmt, call_args }
}

fn transform_array_arg(arg_name: &str) -> JniArgResult {
    // statements
    let arg_name = quote::Ident::new(arg_name);
    let stmt =
        quote! {
            let #arg_name = Vec::from_java(&env, #arg_name);
        };

    // call arg value(s)
    let call_args = vec![quote! { #arg_name.as_ptr() }, quote! { #arg_name.len() }];

    JniArgResult { stmt, call_args }
}

fn transform_callbacks_arg(cb_idents: Vec<(quote::Ident, quote::Ident)>) -> JniArgResult {
    // statements
    let cb_ids: Vec<quote::Ident> = cb_idents
        .iter()
        .map(|&(ref ident, _)| ident.clone())
        .collect();

    let stmt =
        quote! {
            let ctx = gen_ctx!(env, #(#cb_ids),*);
        };

    // call arg value(s)
    let call_args = cb_idents
        .iter()
        .map(|&(_, ref cb_fn)| quote! { #cb_fn })
        .collect();

    JniArgResult { stmt, call_args }
}

pub fn generate_jni_function(args: Vec<ast::Arg>, native_name: &str, func_name: &str) -> String {
    let func_name = quote::Ident::new(format!("Java_NativeBindings_{}", func_name));
    let native_name = quote::Ident::new(native_name);

    // Generate inputs
    let mut call_args = Vec::new();
    let mut stmts = Vec::new();
    let mut callbacks = Vec::new();
    let mut jni_fn_inputs = Vec::new();

    let mut args_iter = args.iter().filter(|arg| !is_user_data_arg(arg)).peekable();

    while let Some(arg) = args_iter.next() {
        let arg_name = pprust::pat_to_string(&*arg.pat);

        let res = if is_array_arg(&arg, args_iter.peek().cloned()) {
            args_iter.next();
            Some(transform_array_arg(&arg_name))
        } else {
            match arg.ty.node {
                // Callback
                ast::TyKind::BareFn(ref bare_fn) => {
                    let cb_class =
                        format!("call_{}", callback_name(&*bare_fn.decl.inputs).unwrap());

                    callbacks.push((quote::Ident::new(arg_name), quote::Ident::new(cb_class)));

                    None
                }

                // Standard pointers.
                ast::TyKind::Ptr(ref ptr) => {
                    // Detect strings, which are *const c_char or *mut c_char
                    if pprust::ty_to_string(&ptr.ty) == "c_char" {
                        Some(transform_string_arg(&arg_name))
                    } else {
                        Some(transform_struct_arg(&arg_name, &ptr.ty))
                    }
                }

                // Native types and others
                _ => {
                    let id = quote::Ident::new(arg_name);
                    Some(JniArgResult {
                        stmt: quote!{},
                        call_args: vec![quote! { #id }],
                    })
                }
            }
        };

        if let Some(jni_arg_res) = res {
            call_args.extend(jni_arg_res.call_args);
            stmts.push(jni_arg_res.stmt);
        }

        jni_fn_inputs.push(transform_jni_arg(&arg));
    }

    let cb_arg_res = transform_callbacks_arg(callbacks);
    call_args.push(quote! { ctx });
    call_args.extend(cb_arg_res.call_args);
    stmts.push(cb_arg_res.stmt);

    let tokens =
        quote! {
            #[no_mangle]
            pub unsafe extern "system" fn #func_name(env: JNIEnv, _class: JClass, #(#jni_fn_inputs),*) {
                #(#stmts)*;
                ffi::#native_name(#(#call_args),*);
            }
        };

    tokens.to_string()
}

pub fn generate_jni_callback(cb: &ast::BareFnTy, cb_class: &str) -> String {
    let cb_name = quote::Ident::new(format!("call_{}", cb_class));

    let mut args: Vec<quote::Tokens> = Vec::new();
    let mut stmts: Vec<quote::Tokens> = Vec::new();
    let mut jni_cb_inputs = Vec::new();
    let mut arg_java_ty = Vec::new();

    let mut args_iter = (&*cb.decl)
        .inputs
        .iter()
        .filter(|arg| !is_user_data_arg(arg))
        .peekable();

    while let Some(arg) = args_iter.next() {
        let arg_name = quote::Ident::new(pprust::pat_to_string(&*arg.pat));
        let arg_ty = quote::Ident::new(pprust::ty_to_string(&*arg.ty));

        jni_cb_inputs.push(quote! { #arg_name: #arg_ty });
        args.push(quote! { #arg_name.into() });

        if is_array_arg(&arg, args_iter.peek().cloned()) {
            let val_java_type = rust_ty_to_signature(&arg.ty).unwrap();
            arg_java_ty.push(JavaType::Array(Box::new(val_java_type)));

            if let Some(len_arg) = args_iter.next() {
                let len_arg_name = quote::Ident::new(pprust::pat_to_string(&*len_arg.pat));
                let len_arg_ty = quote::Ident::new(pprust::ty_to_string(&*len_arg.ty));
                jni_cb_inputs.push(quote! { #len_arg_name: #len_arg_ty });

                stmts.push(quote! {
                    let #arg_name = slice::from_raw_parts(#arg_name, #len_arg_name).to_java(&env);
                });
            } else {
                // error: no length arg?
            }
        } else {
            arg_java_ty.push(rust_ty_to_signature(&arg.ty).unwrap());

            match arg.ty.node {
                // Standard pointers.
                ast::TyKind::Ptr(ref ptr) => {
                    // Detect strings, which are *const c_char or *mut c_char
                    if pprust::ty_to_string(&ptr.ty) == "c_char" {
                        stmts.push(quote! {
                            let #arg_name: JObject = (*#arg_name).to_java(&env).into();
                        });
                    } else {
                        stmts.push(quote! {
                            let #arg_name = (*#arg_name).to_java(&env);
                        });
                    }
                }
                _ => {
                    stmts.push(quote! {
                        let #arg_name = #arg_name.to_java(&env);
                    });
                }
            }
        }
    }

    let arg_ty_str = format!(
        "{}",
        TypeSignature {
            args: arg_java_ty,
            ret: JavaType::Primitive(signature::Primitive::Void),
        }
    );

    let tokens =
        quote! {
        extern "C" fn #cb_name(ctx: *mut c_void, #(#jni_cb_inputs),*) {
            unsafe {
                let env = JVM.attach_current_thread_as_daemon().unwrap();
                let cb = GlobalRef::from_raw_ptr(&env, ctx);

                #(#stmts)*;

                env.call_method(
                    cb.as_obj(),
                    "call",
                    #arg_ty_str,
                    &[ #(#args),* ],
                ).unwrap();
            }
        }
    };

    tokens.to_string()
}
