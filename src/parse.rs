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
        match item.node {
            ast::ItemKind::Use(ref import) => {
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
            _ => (),
        }
    }

    imported
}

/// The manager of moz-cheddar and entry point when the crate is the module.
///
/// Iterates through all items in the module and dispatches to correct methods, then pulls all
/// the results together into a header.
pub fn parse_mod<L: Lang>(module: &ast::Mod, outputs: &mut Outputs) -> Result<(), Vec<Error>> {
    let mut errors = vec![];

    for item in &module.items {
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = item.vis {
            continue;
        }

        // Dispatch to correct method.
        let res = match item.node {
            // TODO: Check for ItemStatic and ItemConst as well.
            //     - How would this work?
            //     - Is it even possible?
            ast::ItemKind::Ty(..) => L::parse_ty(item, outputs),
            ast::ItemKind::Enum(..) => L::parse_enum(item, outputs),
            ast::ItemKind::Struct(..) => L::parse_struct(item, outputs),
            ast::ItemKind::Fn(..) => L::parse_fn(item, outputs),
            _ => Ok(()),
        };

        match res {
            // Display any non-fatal errors, fatal errors are handled at cause.
            Err(error) => errors.push(error),
            Ok(_) => {}  // Item should not be written to header.
        };
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
