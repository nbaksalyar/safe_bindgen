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
    module: &syn::ItemMod,
    mod_path: &[String],
    outputs: &mut Outputs,
) -> Result<(), Vec<Error>> {
    let mut errors = vec![];

    if module.to_owned().content.is_some() {
        for item in module.to_owned().content.unwrap().1 {
            // If it's not visible it can't be called from C.
            match item {
                syn::Item::Mod(ref item) => {
                    if let syn::Visibility::Inherited = item.vis {
                        continue;
                    }
                    if let syn::Visibility::Crate(_) = item.vis {
                        continue;
                    }
                }
                syn::Item::Const(ref item) => {
                    if let syn::Visibility::Inherited = item.vis {
                        continue;
                    }
                    if let syn::Visibility::Crate(_) = item.vis {
                        continue;
                    }
                }
                syn::Item::Type(ref item) => {
                    if let syn::Visibility::Inherited = item.vis {
                        continue;
                    }
                    if let syn::Visibility::Crate(_) = item.vis {
                        continue;
                    }
                }
                syn::Item::Enum(ref item) => {
                    if let syn::Visibility::Inherited = item.vis {
                        continue;
                    }
                    if let syn::Visibility::Crate(_) = item.vis {
                        continue;
                    }
                }
                syn::Item::Fn(ref item) => {
                    if let syn::Visibility::Inherited = item.vis {
                        continue;
                    }
                    if let syn::Visibility::Crate(_) = item.vis {
                        continue;
                    }
                }
                syn::Item::Struct(ref item) => {
                    if let syn::Visibility::Inherited = item.vis {
                        continue;
                    }
                    if let syn::Visibility::Crate(_) = item.vis {
                        continue;
                    }
                }
                _ => {}
            }

            // Dispatch to correct method.
            let res = match item {
                syn::Item::Mod(ref item) => {
                    println!("Mod found inside a Mod");
                    parse_mod(lang, item, mod_path, outputs);
                    Ok(())
                }
                syn::Item::Const(ref item) => {
                    println!("Const found inside mod");
                    lang.parse_const(item, mod_path, outputs);
                    Ok(())
                }
                syn::Item::Type(ref item) => {
                    println!("Type found inside mod");
                    lang.parse_ty(item, mod_path, outputs);
                    Ok(())
                }
                syn::Item::Enum(ref item) => {
                    println!("Enum found inside mod");
                    lang.parse_enum(item, mod_path, outputs);
                    Ok(())
                }
                syn::Item::Fn(ref item) => {
                    println!("Fn found inside mod");
                    lang.parse_fn(item, mod_path, outputs);
                    Ok(())
                }
                syn::Item::Struct(ref item) => {
                    println!("Struct found inside mod");
                    lang.parse_struct(&item, mod_path, outputs);
                    Ok(())
                }
                _ => Ok(()),
            };

            // Display any non-fatal errors, fatal errors are handled at cause.
            if let Err(error) = res {
                errors.push(error)
            }
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
