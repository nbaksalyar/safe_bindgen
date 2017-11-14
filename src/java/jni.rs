//! Functions to generate JNI bindings

use common::is_array_arg;
use syntax::ast::{self, Ident, DUMMY_NODE_ID};
use syntax::symbol::Symbol;
use syntax::codemap::dummy_spanned;
use syntax::abi::Abi;
use syntax::parse::ParseSess;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::ext::quote::rt::{Span, DUMMY_SP};
use syntax::ext::base::{DummyResolver, ExtCtxt};
use syntax::ext::build::AstBuilder;
use syntax::ext::expand::ExpansionConfig;

trait JniAstBuilder: AstBuilder {
    fn no_mangle_attr(&self, span: Span) -> ast::Attribute {
        self.attribute(span, self.meta_word(DUMMY_SP, Symbol::intern("no_mangle")))
    }

    fn jni_fn(
        &self,
        span: Span,
        name: Ident,
        inputs: Vec<ast::Arg>,
        output: P<ast::Ty>,
        body: P<ast::Block>,
    ) -> P<ast::Item> {
        // Generate first 2 arguments
        let jni_env_arg = self.arg(
            DUMMY_SP,
            Ident::from_str("env"),
            self.ty_path(self.path_ident(DUMMY_SP, Ident::from_str("JNIEnv"))),
        );
        let class_arg = self.arg(
            DUMMY_SP,
            Ident::from_str("_class"),
            self.ty_path(self.path_ident(DUMMY_SP, Ident::from_str("JClass"))),
        );

        // Combine with the rest of args
        let mut all_inputs = vec![jni_env_arg, class_arg];
        all_inputs.extend_from_slice(&inputs);

        // Return template JNI function
        self.item(
            span,
            name,
            vec![self.no_mangle_attr(span)],
            ast::ItemKind::Fn(
                self.fn_decl(all_inputs, output),
                ast::Unsafety::Unsafe,
                dummy_spanned(ast::Constness::NotConst),
                Abi::System,
                ast::Generics::default(),
                body,
            ),
        )
    }

    fn to_jni_arg(&self, arg: &ast::Arg, ty_name: &str) -> ast::Arg {
        ast::Arg {
            id: DUMMY_NODE_ID,
            pat: arg.pat.clone(),
            ty: self.ty_path(self.path_ident(DUMMY_SP, Ident::from_str(ty_name))),
        }
    }

    /// Creates a `from_java` call wrapper
    fn jni_convert_type(&self, native_type: &str, arg_name: &str) -> P<ast::Expr> {
        self.expr_call_ident(
            DUMMY_SP,
            Ident::from_str(&format!("{}::from_java", native_type)),
            vec![
                self.expr_addr_of(DUMMY_SP, self.expr_ident(DUMMY_SP, Ident::from_str("env"))),
                self.expr_ident(DUMMY_SP, Ident::from_str(arg_name)),
            ],
        )
    }

    /// `env.<method>(<args>).unwrap()`
    fn env_method_call(&self, method: &str, args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        let exp = self.expr_method_call(
            DUMMY_SP,
            self.expr_ident(DUMMY_SP, Ident::from_str("env")),
            Ident::from_str(method),
            args,
        );

        // unwrap
        self.expr_method_call(DUMMY_SP, exp, Ident::from_str("unwrap"), vec![])
    }

    /// Generates `env.new_global_ref(<arg_name>).unwrap()` expr
    fn new_global_ref(&self, arg_name: &str) -> P<ast::Expr> {
        self.env_method_call(
            "new_global_ref",
            vec![self.expr_ident(DUMMY_SP, Ident::from_str(arg_name))],
        )
    }

    /// Generates `env.delete_global_ref(<arg_name>).unwrap()` expr
    fn delete_local_ref(&self, arg_name: &str) -> P<ast::Expr> {
        self.env_method_call(
            "delete_local_ref",
            vec![self.expr_ident(DUMMY_SP, Ident::from_str(arg_name))],
        )
    }
}

impl<'a> JniAstBuilder for ExtCtxt<'a> {}

fn transform_jni_arg<Ast: AstBuilder + JniAstBuilder>(ast: &Ast, arg: &ast::Arg) -> ast::Arg {
    match arg.ty.node {
        // Callback
        ast::TyKind::BareFn(ref _bare_fn) => ast.to_jni_arg(arg, "JObject"),

        // Plain old types.
        ast::TyKind::Path(None, ref path) => {
            let (ty, _module) = path.segments.split_last().expect(
                "already checked that there were at least two elements",
            );
            let ty: &str = &ty.identifier.name.as_str();

            let jni_type = match ty {
                "c_short" | "u16" => "jshort",
                "c_int" | "u32" => "jint",
                "c_long" | "u64" => "jlong",
                "c_usize" | "usize" => "jlong",
                _ => ty,
            };

            ast.to_jni_arg(arg, jni_type)
        }

        // Standard pointers.
        ast::TyKind::Ptr(ref ptr) => {
            // Detect strings, which are *const c_char or *mut c_char
            if pprust::ty_to_string(&ptr.ty) == "c_char" {
                return ast.to_jni_arg(arg, "JString");
            }
            ast.to_jni_arg(arg, "JObject")
        }

        _ => arg.clone(),

    }
}

struct JniArgResult {
    stmts: Vec<ast::Stmt>,
    call_args: Vec<P<ast::Expr>>,
}

fn transform_string_arg<Ast: AstBuilder + JniAstBuilder>(
    ast: &Ast,
    arg_name: &str,
) -> JniArgResult {
    // statements
    let cstr_from_java = ast.jni_convert_type("CString", arg_name);
    let stmts = vec![
        ast.stmt_let(DUMMY_SP, false, Ident::from_str(arg_name), cstr_from_java),
    ];

    // call arg value(s)
    let call_args = vec![
        ast.expr_method_call(
            DUMMY_SP,
            ast.expr_ident(DUMMY_SP, Ident::from_str(arg_name)),
            Ident::from_str("as_ptr"),
            vec![]
        ),
    ];

    JniArgResult { stmts, call_args }
}

fn transform_struct_arg<Ast: AstBuilder + JniAstBuilder>(
    ast: &Ast,
    arg_name: &str,
) -> JniArgResult {
    // statements
    let cstr_from_java = ast.jni_convert_type("NativeStruct", arg_name);
    let stmts = vec![
        ast.stmt_let(DUMMY_SP, false, Ident::from_str(arg_name), cstr_from_java),
    ];

    // call arg value(s)
    let call_args = vec![
        ast.expr_addr_of(
            DUMMY_SP,
            ast.expr_ident(DUMMY_SP, Ident::from_str(arg_name))
        ),
    ];

    JniArgResult { stmts, call_args }
}

fn transform_array_arg<Ast: AstBuilder + JniAstBuilder>(ast: &Ast, arg_name: &str) -> JniArgResult {
    // statements
    let cstr_from_java = ast.jni_convert_type("Vec", arg_name);
    let stmts = vec![
        ast.stmt_let(DUMMY_SP, false, Ident::from_str(arg_name), cstr_from_java),
    ];

    // call arg value(s)
    let call_args = vec![
        ast.expr_method_call(
            DUMMY_SP,
            ast.expr_ident(DUMMY_SP, Ident::from_str(arg_name)),
            Ident::from_str("as_ptr"),
            vec![]
        ),
        ast.expr_method_call(
            DUMMY_SP,
            ast.expr_ident(DUMMY_SP, Ident::from_str(arg_name)),
            Ident::from_str("as_len"),
            vec![]
        ),
    ];

    JniArgResult { stmts, call_args }
}

fn transform_callbacks_arg<Ast: AstBuilder + JniAstBuilder>(
    ast: &Ast,
    cb_args: Vec<P<ast::BareFnTy>>,
) -> JniArgResult {
    let mut stmts = Vec::new();
    let mut call_args = vec![ast.expr_ident(DUMMY_SP, Ident::from_str("ctx"))];

    if cb_args.len() > 1 {
        // Handle more than one cb

        // let cbs_slice = ast.expr_vec(DUMMY_SP, exprs);
        // ast.stmt_let(DUMMY_SP, false, Ident::from_str("ctx"), cbs_slice);
    } else {
        let cb_name = "o_cb";

        // let ctx = env.new_global_ref(...).unwrap().into_raw_pointer();
        stmts.push(ast.stmt_let(
            DUMMY_SP,
            false,
            Ident::from_str("ctx"),
            ast.expr_method_call(
                DUMMY_SP,
                ast.new_global_ref(cb_name),
                Ident::from_str("into_raw_pointer"),
                vec![],
            ),
        ));

        // env.delete_local_ref(...).unwrap()
        stmts.push(ast.stmt_expr(ast.delete_local_ref(cb_name)));

        // Some(callback_name)
        call_args.push(ast.expr_some(
            DUMMY_SP,
            ast.expr_ident(DUMMY_SP, Ident::from_str("callback_fn")),
        ));
    }

    JniArgResult { stmts, call_args }
}

pub fn generate_jni_function(args: Vec<ast::Arg>, native_name: &str, func_name: &str) -> String {
    let full_name = &format!("Java_NativeBindings_{}", func_name);

    let parse_sess = ParseSess::new();
    let mut resolver = DummyResolver {};
    let ast = ExtCtxt::new(
        &parse_sess,
        ExpansionConfig::default(String::from("jni")),
        &mut resolver,
    );

    let mut args_iter = args.iter().peekable();
    let mut call_args = Vec::new();
    let mut stmts = Vec::new();
    let mut callbacks = Vec::new();
    let mut jni_fn_args = Vec::new();

    while let Some(arg) = args_iter.next() {
        let arg_name = pprust::pat_to_string(&*arg.pat);

        let jni_arg_res = if is_array_arg(&arg, args_iter.peek().cloned()) {
            args_iter.next();
            transform_array_arg(&ast, &arg_name)
        } else {
            match arg.ty.node {
                // Callback
                ast::TyKind::BareFn(ref bare_fn) => {
                    callbacks.push(bare_fn.clone());
                    JniArgResult {
                        call_args: vec![],
                        stmts: vec![],
                    }
                }

                // Standard pointers.
                ast::TyKind::Ptr(ref ptr) => {
                    // Detect strings, which are *const c_char or *mut c_char
                    if pprust::ty_to_string(&ptr.ty) == "c_char" {
                        transform_string_arg(&ast, &arg_name)
                    } else {
                        transform_struct_arg(&ast, &arg_name)
                    }
                }
                _ => JniArgResult {
                    call_args: vec![],
                    stmts: vec![],
                },
            }
        };

        call_args.extend(jni_arg_res.call_args);
        stmts.extend(jni_arg_res.stmts);
        jni_fn_args.push(transform_jni_arg(&ast, &arg));
    }

    let cb_arg_res = transform_callbacks_arg(&ast, callbacks);
    call_args.extend(cb_arg_res.call_args);
    stmts.extend(cb_arg_res.stmts);

    // Call the native backend function
    let fn_call = ast.expr_call_ident(
        DUMMY_SP,
        Ident::from_str(&format!("ffi::{}", native_name)),
        call_args,
    );

    stmts.push(ast.stmt_expr(fn_call));

    let fn_block = ast.block(DUMMY_SP, stmts);

    let item = ast.jni_fn(
        DUMMY_SP,
        Ident::from_str(full_name),
        jni_fn_args,
        ast.ty(DUMMY_SP, ast::TyKind::Tup(vec![])),
        fn_block,
    );

    pprust::item_to_string(&item)
}

pub fn generate_jni_callback() {}
