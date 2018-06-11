//! Intermediate enums representing C types converted from Rust types

use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq)]
pub struct CTypeNamed(pub String, pub CType);

impl Display for CTypeNamed {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.1 {
            // Function declarations don't need to be prefixed, so it's a
            // special case
            CType::FnDecl { .. } => write!(f, "{}", self.1),

            // For all other cases we add a type prefix
            _ => write!(f, "{} {}", self.1, self.0),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CPtrType {
    Const,
    Mutable,
}

impl Display for CPtrType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            CPtrType::Const => write!(f, " const"),
            CPtrType::Mutable => Ok(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CType {
    Void,
    Mapping(String),
    Native(&'static str),
    Ptr(Box<CType>, CPtrType),
    FnDecl {
        inner: String,
        args: Vec<CTypeNamed>,
        return_type: Box<CType>,
    },
}

impl CType {
    /// Returns a list of user-defined types this `CType` depends on
    pub fn dependencies(&self) -> Vec<String> {
        match *self {
            CType::FnDecl {
                ref args,
                ref return_type,
                ..
            } => return_type
                .dependencies()
                .iter()
                .cloned()
                .chain(
                    args.iter()
                        .flat_map(|&CTypeNamed(_, ref cty)| cty.dependencies()),
                )
                .collect(),
            CType::Ptr(ref cty, _) => cty.dependencies(),
            CType::Mapping(ref mapping) => vec![mapping.clone()],
            _ => Default::default(),
        }
    }
}

impl Display for CType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            CType::Void => write!(f, "void"),
            CType::Mapping(ref s) => write!(f, "{}", s),
            CType::Native(s) => write!(f, "{}", s),
            CType::Ptr(ref cty, ref ptrty) => write!(f, "{}{}*", cty, ptrty),
            CType::FnDecl {
                ref inner,
                ref args,
                ref return_type,
            } => write!(
                f,
                "{} (*{})({})",
                return_type,
                inner,
                if args.is_empty() {
                    "void".to_string()
                } else {
                    args.iter()
                        .map(|cty| format!("{}", cty))
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            ),
        }
    }
}
