//! Functions for actually parsing the source file.

use common::{Lang, Outputs};
use syntax::ast;
use Error;

/// Returns a list of FFI submodules imported in a top-level module
pub fn imported_mods(module: &ast::Mod) -> Vec<Vec<String>> {
    let mut imported = Vec::new();

    for item in &module.items {
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = item.vis {
            continue;
        }
        if let ast::ItemKind::Use(ref import) = item.node {
            if let ast::ViewPathGlob(ref path) = import.node {
                let mut segments = path.segments.iter();
                if path.is_global() {
                    segments.next();
                }
                let base_output_path: Vec<String> = segments
                    .map(|seg| seg.identifier.name.as_str().to_string())
                    .collect();

                if base_output_path[0] == "ffi" {
                    imported.push(base_output_path);
                }
            }
        }
    }

    imported
}

/// The manager of bindgen and entry point when the crate is the module.
///
/// Iterates through all items in the module and dispatches to correct methods, then pulls all
/// the results together into a header.
pub fn parse_mod<L: Lang>(
    lang: &mut L,
    module: &ast::Mod,
    module_path: &[String],
    outputs: &mut Outputs,
) -> Result<(), Vec<Error>> {
    let mut errors = vec![];

    for item in &module.items {
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = item.vis {
            continue;
        }

        // Dispatch to correct method.
        let res = match item.node {
            ast::ItemKind::Const(..) => lang.parse_const(item, module_path, outputs),
            ast::ItemKind::Ty(..) => lang.parse_ty(item, module_path, outputs),
            ast::ItemKind::Enum(..) => lang.parse_enum(item, module_path, outputs),
            ast::ItemKind::Struct(..) => lang.parse_struct(item, module_path, outputs),
            ast::ItemKind::Fn(..) => lang.parse_fn(item, module_path, outputs),
            _ => Ok(()),
        };

        // Display any non-fatal errors, fatal errors are handled at cause.
        if let Err(error) = res {
            errors.push(error)
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
