//! Functions common for all target languages.

use syntax::ast;
use syntax::print::pprust;

use Error;
use Level;

pub fn is_user_data_arg(arg: &ast::Arg) -> bool {
    pprust::pat_to_string(&*arg.pat) == "user_data" &&
        pprust::ty_to_string(&*arg.ty) == "*mut c_void"
}

pub fn is_result_arg(arg: &ast::Arg) -> bool {
    pprust::pat_to_string(&*arg.pat) == "result" &&
        pprust::ty_to_string(&*arg.ty) == "*const FfiResult"
}

/// Transform function arguments into a (name, type) pair
pub fn fn_args(inputs: &Vec<ast::Arg>, name: &str) -> Result<Vec<(String, ast::Ty)>, Error> {
    inputs
        .iter()
        .map(|ref arg| {
            use syntax::ast::{PatKind, BindingMode};
            let arg_name = match arg.pat.node {
                PatKind::Ident(BindingMode::ByValue(_), ref ident, None) => {
                    ident.node.name.to_string()
                }
                _ => {
                    return Err(Error {
                        level: Level::Error,
                        span: None,
                        message: format!(
                            "cheddar only supports by-value arguments:
    incorrect argument `{}` in function definition `{}`",
                            pprust::pat_to_string(&*arg.pat),
                            name
                        ),
                    })
                }
            };
            let arg_ty: &ast::Ty = &*arg.ty.clone();
            Ok((arg_name, arg_ty.clone()))
        })
        .collect()
}
