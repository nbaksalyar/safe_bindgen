//! Functions for actually parsing the source file.

use common::{Lang, Outputs};
use std::collections::HashMap;
use syntax::ast;
use Error;
use Level;

// TODO: we should use our own parse state which tracks what types are callable from C
//     - then we can give decent errors for ones that aren't
//     - will require multiple passes of the module
//         - each time a struct, enum, or typedef changes do another pass
//         - only store errors on the final pass
//             - maybe there will be some errors which will need to be stored before then
//     - search inside struct as well for whitelisted types
//     - possibly also search other crates when encountering a path

/// Check that an expected path has been `pub use`d.
fn check_pub_use(item: &ast::Item, expected: &ast::Path) -> bool {
    if let ast::ItemKind::Use(ref path) = item.node {
        // API has to be public to be used.
        if let ast::Visibility::Public = item.vis {
            // Easiest way to ensure all of API has been brought into scope.
            if let ast::ViewPath_::ViewPathGlob(ref path) = path.node {
                let mut segments = path.segments.iter();
                if path.is_global() {
                    segments.next();
                }
                return segments.eq(expected.segments.iter());
            }
        }
    }

    false
}


/// The main entry point when looking for a specific module.
///
/// Determines which module to parse, ensures it is `pub use`ed then hands off to
/// `cheddar::parse::parse_mod`.
pub fn parse_crate<L: Lang>(krate: &ast::Crate, path: &ast::Path) -> Result<Outputs, Vec<Error>> {
    // First look to see if the module has been `pub use`d.
    if !krate.module.items.iter().any(
        |item| check_pub_use(&item, &path),
    )
    {
        return Err(vec![
            Error {
                level: Level::Error,
                span: None,
                message: format!("module `{}` has not been brought into global scope", path),
            },
            Error {
                level: Level::Help,
                span: None,
                message: format!("try putting `pub use {}::*` in your root source file", path),
            },
        ]);
    }

    // For each module in the path, look for the corresponding module in the source.
    let mut current_module = &krate.module;
    for module in &path.segments {
        let mut found = false;
        for item in &current_module.items {
            if let ast::ItemKind::Mod(ref new_module) = item.node {
                if module.identifier == item.ident {
                    current_module = new_module;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            return Err(vec![
                Error {
                    level: Level::Fatal,
                    span: None,
                    message: format!("module `{}` could not be found", module.identifier),
                },
            ]);
        }
    }

    parse_mod::<L>(&current_module)
}

/// The manager of moz-cheddar and entry point when the crate is the module.
///
/// Iterates through all items in the module and dispatches to correct methods, then pulls all
/// the results together into a header.
pub fn parse_mod<L: Lang>(module: &ast::Mod) -> Result<Outputs, Vec<Error>> {
    let mut buffer = HashMap::new();
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
            ast::ItemKind::Ty(..) => L::parse_ty(item, &mut buffer),
            ast::ItemKind::Enum(..) => L::parse_enum(item, &mut buffer),
            ast::ItemKind::Struct(..) => L::parse_struct(item, &mut buffer),
            ast::ItemKind::Fn(..) => L::parse_fn(item, &mut buffer),
            _ => Ok(()),
        };

        match res {
            // Display any non-fatal errors, fatal errors are handled at cause.
            Err(error) => errors.push(error),
            Ok(_) => {}  // Item should not be written to header.
        };
    }

    if errors.is_empty() {
        Ok(buffer)
    } else {
        Err(errors)
    }
}
