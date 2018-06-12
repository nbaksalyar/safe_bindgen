use std::collections::BTreeSet;
use syntax::print::pprust;
use syntax::{ast, symbol};

#[derive(Debug)]
pub enum StructField {
    Primitive(ast::StructField),
    Array {
        field: ast::StructField,
        len_field: String,
        cap_field: Option<String>,
    },
    String(ast::StructField),
    StructPtr {
        field: ast::StructField,
        ty: ast::MutTy,
    },
    LenField(ast::StructField),
}

impl StructField {
    pub fn struct_field(&self) -> &ast::StructField {
        match *self {
            StructField::Primitive(ref f)
            | StructField::Array { field: ref f, .. }
            | StructField::StructPtr { field: ref f, .. }
            | StructField::String(ref f)
            | StructField::LenField(ref f) => f,
        }
    }

    pub fn name(&self) -> symbol::InternedString {
        self.struct_field().ident.unwrap().name.as_str()
    }
}

pub fn transform_struct_fields(fields: &[ast::StructField]) -> Vec<StructField> {
    let mut results = Vec::new();
    let field_names: BTreeSet<_> = fields
        .iter()
        .map(|f| f.ident.unwrap().name.as_str().to_string())
        .collect();

    for f in fields {
        let mut field_name: String = f.ident.unwrap().name.as_str().to_string();

        match f.ty.node {
            // Pointers
            ast::TyKind::Ptr(ref ptr) => {
                if field_name.ends_with("_ptr") {
                    field_name = field_name.chars().take(field_name.len() - 4).collect();
                }

                let len_field = format!("{}_len", field_name);
                let cap_field = format!("{}_cap", field_name);

                if field_names.contains(&len_field) {
                    results.push(StructField::Array {
                        field: f.clone(),
                        len_field,
                        cap_field: if field_names.contains(&cap_field) {
                            Some(cap_field)
                        } else {
                            None
                        },
                    });
                } else {
                    match pprust::ty_to_string(&ptr.ty).as_str() {
                        // Strings
                        "c_char" => {
                            results.push(StructField::String(f.clone()));
                        }
                        // Other ptrs, most likely structs
                        _ => {
                            results.push(StructField::StructPtr {
                                field: f.clone(),
                                ty: ptr.clone(),
                            });
                        }
                    }
                }
            }

            ast::TyKind::Path(None, ref _path) => {
                results.push(if is_array_meta_field(f) {
                    StructField::LenField(f.clone())
                } else {
                    StructField::Primitive(f.clone())
                });
            }

            _ => results.push(StructField::Primitive(f.clone())),
        }
    }

    results
}

fn is_array_meta_field(field: &ast::StructField) -> bool {
    let str_name = field.ident.unwrap().name.as_str();

    if let ast::TyKind::Path(None, ref path) = field.ty.node {
        let (ty, _module) = path
            .segments
            .split_last()
            .expect("already checked that there were at least two elements");
        let ty: &str = &ty.identifier.name.as_str();

        ty == "usize" && (str_name.ends_with("_len") || str_name.ends_with("_cap"))
    } else {
        false
    }
}
