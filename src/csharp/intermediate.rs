//! Types and utilities for the intermediate representation between the rust code
//! and the target language code.

use common;
use std::collections::BTreeMap;
use syntax::ast;
use syntax::print::pprust;
use syntax::ptr;

// TODO: replace whit macro with the ? operator one we upgrade to rust 1.22
macro_rules! try_opt {
    ($e:expr) => {
        match $e {
            Some(value) => value,
            None => return None,
        }
    };
}

#[derive(Clone, Debug)]
pub enum Type {
    Unit,
    Bool,
    Char,
    CChar,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    ISize,
    U8,
    U16,
    U32,
    U64,
    USize,
    String,
    Pointer(Box<Type>),
    Array(Box<Type>, ArraySize),
    Function(Box<Function>),
    User(String),
}

impl Type {
    pub fn is_dynamic_array(&self) -> bool {
        if let Type::Array(_, ArraySize::Dynamic) = *self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArraySize {
    /// Static size given as literal number.
    Lit(usize),
    /// Static size given as named constant.
    Const(String),
    // Dynamic size.
    Dynamic,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub inputs: Vec<(String, Type)>,
    pub output: Type,
}

pub struct Snippet<T> {
    pub docs: String,
    pub name: String,
    pub item: T,
}

pub struct Const {
    pub ty: Type,
    pub value: ConstValue,
}

pub enum ConstValue {
    Bool(bool),
    Char(char),
    Float(String),
    Int(i64),
    String(String),
    Array(Vec<ConstValue>),
    Struct(String, BTreeMap<String, ConstValue>),
}

pub struct Struct {
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub docs: String,
    pub name: String,
    pub ty: Type,
    pub has_cap: bool,
}

pub struct Enum {
    pub variants: Vec<EnumVariant>,
}

pub struct EnumVariant {
    pub docs: String,
    pub name: String,
    pub value: Option<i64>,
}

pub fn transform_type(input: &ast::Ty) -> Option<Type> {
    match input.node {
        ast::TyKind::Array(ref ty, ref size) => transform_array(ty, size),
        ast::TyKind::Path(None, _) => transform_path(input),
        ast::TyKind::Ptr(ref ptr) => transform_pointer(ptr),
        ast::TyKind::Rptr(ref lifetime, ast::MutTy { ref ty, .. }) => {
            transform_reference(lifetime, ty)
        }
        ast::TyKind::BareFn(ref bare_fn) => {
            transform_function(&*bare_fn.decl).map(|fun| Type::Function(Box::new(fun)))
        }
        _ => None,
    }
}

pub fn transform_function(decl: &ast::FnDecl) -> Option<Function> {
    let output = match decl.output {
        ast::FunctionRetTy::Default(..) => Type::Unit,
        ast::FunctionRetTy::Ty(ref ty) => match transform_type(ty) {
            Some(ty) => ty,
            None => return None,
        },
    };

    let mut inputs = Vec::with_capacity(decl.inputs.len());
    let mut iter = decl.inputs.iter();

    let mut carry = None;

    loop {
        let one = if let Some(param) = carry.take() {
            Some(param)
        } else if let Some(arg) = iter.next() {
            Some(try_opt!(transform_function_param(arg)))
        } else {
            None
        };

        let two = if let Some(arg) = iter.next() {
            Some(try_opt!(transform_function_param(arg)))
        } else {
            None
        };

        if let Some(one) = one {
            if let Some(two) = two {
                if let Some(new_one) =
                    transform_ptr_and_len_to_array(&one.0, &one.1, &two.0, &two.1)
                {
                    inputs.push(new_one);
                } else {
                    inputs.push(one);
                    carry = Some(two);
                }
            } else {
                inputs.push(one);
                break;
            }
        } else {
            break;
        }
    }

    Some(Function { inputs, output })
}

pub fn transform_function_param(arg: &ast::Arg) -> Option<(String, Type)> {
    if let Some(ty) = transform_type(&*arg.ty) {
        let name = pprust::pat_to_string(&*arg.pat);
        Some((name, ty))
    } else {
        None
    }
}

pub fn transform_const(ty: &ast::Ty, value: &ast::Expr) -> Option<Const> {
    let ty = try_opt!(transform_type(ty));
    let value = try_opt!(transform_const_value(value));

    Some(Const { ty, value })
}

pub fn transform_enum(variants: &[ast::Variant]) -> Option<Enum> {
    let variants: Option<Vec<_>> = variants
        .into_iter()
        .map(|variant| {
            if !variant.node.data.is_unit() {
                return None;
            }

            let (_, docs) = common::parse_attr(&variant.node.attrs, |_| true, retrieve_docstring);
            let name = variant.node.name.name.as_str().to_string();
            let value = extract_enum_variant_value(variant);

            Some(EnumVariant { docs, name, value })
        })
        .collect();

    variants.map(|variants| Enum { variants })
}

pub fn transform_struct(fields: &[ast::StructField]) -> Option<Struct> {
    let fields: Option<Vec<_>> = fields
        .into_iter()
        .map(|field| {
            let (_, docs) = common::parse_attr(&field.attrs, |_| true, retrieve_docstring);
            let name = field.ident.unwrap().name.as_str().to_string();
            let ty = try_opt!(transform_type(&field.ty));

            Some(StructField {
                docs,
                name,
                ty,
                has_cap: false,
            })
        })
        .collect();

    fields.map(|fields| Struct {
        fields: process_struct_fields(fields),
    })
}

/// Is the given parameter an `user_data` for a callback?
pub fn is_user_data(name: &str, ty: &Type) -> bool {
    if let Type::Pointer(ref ty) = *ty {
        if let Type::Unit = **ty {
            return name == "" || name == "user_data";
        }
    }

    false
}

pub fn extract_callbacks(inputs: &[(String, Type)]) -> Vec<&Function> {
    inputs
        .into_iter()
        .filter_map(|&(_, ref ty)| extract_callback(ty))
        .collect()
}

pub fn extract_first_callback(inputs: &[(String, Type)]) -> Option<&Function> {
    inputs
        .into_iter()
        .filter_map(|&(_, ref ty)| extract_callback(ty))
        .next()
}

pub fn num_callbacks(inputs: &[(String, Type)]) -> usize {
    inputs
        .into_iter()
        .filter_map(|&(_, ref ty)| extract_callback(ty))
        .count()
}

pub fn extract_callback(ty: &Type) -> Option<&Function> {
    if let Type::Function(ref fun) = *ty {
        let &(ref name, ref ty) = try_opt!(fun.inputs.get(0));

        if is_user_data(name, ty) {
            return Some(fun);
        }
    }

    None
}

pub fn retrieve_docstring(attr: &ast::Attribute) -> Option<String> {
    common::retrieve_docstring(attr, "")
}

fn transform_const_value(expr: &ast::Expr) -> Option<ConstValue> {
    match expr.node {
        ast::ExprKind::Lit(ref lit) => transform_const_literal(lit),
        ast::ExprKind::Array(ref elements) => transform_const_array(elements),
        ast::ExprKind::Struct(ref path, ref fields, None) => transform_const_struct(path, fields),
        ast::ExprKind::AddrOf(_, ref expr) => transform_const_value(expr),
        ast::ExprKind::Cast(ref expr, ref ty) => transform_const_cast(expr, ty),
        _ => None,
    }
}

fn transform_const_literal(lit: &ast::Lit) -> Option<ConstValue> {
    let result = match lit.node {
        ast::LitKind::Bool(value) => ConstValue::Bool(value),
        ast::LitKind::Byte(value) => ConstValue::Int(i64::from(value)),
        ast::LitKind::Char(value) => ConstValue::Char(value),
        ast::LitKind::Int(value, _) => ConstValue::Int(value as i64),
        ast::LitKind::Float(ref value, _) | ast::LitKind::FloatUnsuffixed(ref value) => {
            ConstValue::Float(value.as_str().to_string())
        }
        ast::LitKind::Str(ref value, ..) => ConstValue::String(value.as_str().to_string()),
        // TODO: LitKind::ByteStr
        _ => return None,
    };

    Some(result)
}

fn transform_const_array(array: &[ptr::P<ast::Expr>]) -> Option<ConstValue> {
    let elements: Option<Vec<_>> = array
        .into_iter()
        .map(|expr| transform_const_value(expr))
        .collect();

    elements.map(ConstValue::Array)
}

fn transform_const_struct(path: &ast::Path, fields: &[ast::Field]) -> Option<ConstValue> {
    let name = pprust::path_to_string(path);
    let fields: Option<BTreeMap<_, _>> = fields
        .into_iter()
        .map(|field| {
            if let Some(value) = transform_const_value(&*field.expr) {
                let name = field.ident.node.name.as_str().to_string();
                Some((name, value))
            } else {
                None
            }
        })
        .collect();

    fields.map(|fields| ConstValue::Struct(name, fields))
}

fn transform_const_cast(expr: &ast::Expr, ty: &ast::Ty) -> Option<ConstValue> {
    // Currently only supports null strings, e.g.: `0 as *const c_char`

    if let Some(Type::String) = transform_type(ty) {
        if &pprust::expr_to_string(expr) == "0" {
            return Some(ConstValue::String(String::new()));
        }
    }

    None
}

fn transform_array(ty: &ast::Ty, size: &ast::Expr) -> Option<Type> {
    let size = match extract_array_size(size) {
        None => return None,
        Some(size) => size,
    };

    let ty = match transform_type(ty) {
        None | Some(Type::Array { .. }) => return None, // multi-dimensional array not supported yet
        Some(ty) => ty,
    };

    Some(Type::Array(Box::new(ty), size))
}

fn extract_array_size(expr: &ast::Expr) -> Option<ArraySize> {
    match expr.node {
        ast::ExprKind::Lit(ref lit) => {
            extract_int_literal(lit).map(|value| ArraySize::Lit(value as usize))
        }
        ast::ExprKind::Path(None, ref path) => {
            // Currently supports only unqualified constants.
            if path.segments.len() > 1 || path.segments[0].parameters.is_some() {
                None
            } else {
                Some(ArraySize::Const(
                    path.segments[0].identifier.name.as_str().to_string(),
                ))
            }
        }
        _ => None,
    }
}

fn transform_ptr_and_len_to_array(
    ptr_name: &str,
    ptr_ty: &Type,
    len_name: &str,
    len_ty: &Type,
) -> Option<(String, Type)> {
    let elem_ty = if let Type::Pointer(ref ty) = *ptr_ty {
        &**ty
    } else {
        return None;
    };

    if let Type::USize = *len_ty {
    } else {
        return None;
    }

    // Matches "foo_ptr"/"foo_len" and "foo"/"foo_len"
    let ptr_index = if ptr_name.ends_with("_ptr") {
        ptr_name.len() - "_ptr".len()
    } else {
        ptr_name.len()
    };

    let len_index = if len_name.ends_with("_len") {
        len_name.len() - "_len".len()
    } else {
        return None;
    };

    if ptr_name[0..ptr_index] == len_name[0..len_index] {
        Some((
            ptr_name[0..ptr_index].to_string(),
            Type::Array(Box::new(elem_ty.clone()), ArraySize::Dynamic),
        ))
    } else {
        None
    }
}

fn transform_path(input: &ast::Ty) -> Option<Type> {
    let full = pprust::ty_to_string(input);
    let output = match full.as_str() {
        "bool" => Type::Bool,
        "char" => Type::Char,
        "c_char" | "libc::c_char" | "std::os::raw::c_char" => Type::CChar,
        "f32" => Type::F32,
        "f64" => Type::F64,
        "i8" => Type::I8,
        "i16" => Type::I16,
        "i32" => Type::I32,
        "i64" => Type::I64,
        "isize" => Type::ISize,
        "u8" => Type::U8,
        "u16" => Type::U16,
        "u32" => Type::U32,
        "u64" => Type::U64,
        "usize" => Type::USize,
        "c_void" | "libc::c_void" | "std::os::raw::c_void" => Type::Unit,
        "str" => Type::String,
        name => Type::User(name.to_string()),
    };

    Some(output)
}

fn transform_pointer(ptr: &ast::MutTy) -> Option<Type> {
    match transform_type(&ptr.ty) {
        Some(Type::CChar) => Some(Type::String),
        Some(ty) => Some(Type::Pointer(Box::new(ty))),
        _ => None,
    }
}

fn transform_reference(lifetime: &Option<ast::Lifetime>, ty: &ast::Ty) -> Option<Type> {
    if lifetime
        .map(|lifetime| &*lifetime.name.as_str() != "'static")
        .unwrap_or(false)
    {
        return None;
    }

    match transform_type(ty) {
        Some(Type::String) => Some(Type::String),
        Some(Type::User(name)) => Some(Type::User(name)),
        _ => None,
    }
}

fn extract_enum_variant_value(variant: &ast::Variant) -> Option<i64> {
    if let Some(ref expr) = variant.node.disr_expr {
        if let ast::ExprKind::Lit(ref lit) = expr.node {
            return extract_int_literal(lit);
        }
    }

    None
}

fn extract_int_literal(lit: &ast::Lit) -> Option<i64> {
    if let ast::LitKind::Int(val, ..) = lit.node {
        Some(val as i64)
    } else {
        None
    }
}

fn process_struct_fields(mut input: Vec<StructField>) -> Vec<StructField> {
    let mut output = Vec::with_capacity(input.len());

    let mut iter = input.drain(..);
    let mut field0 = if let Some(field) = iter.next() {
        field
    } else {
        return output;
    };

    loop {
        if let Some(field1) = iter.next() {
            if let Some((name, ty)) =
                transform_ptr_and_len_to_array(&field0.name, &field0.ty, &field1.name, &field1.ty)
            {
                output.push(StructField {
                    docs: String::new(),
                    name,
                    ty,
                    has_cap: false,
                });

                let last = output.last_mut().unwrap();

                if let Some(field) = iter.next() {
                    if is_capacity(&field, &last.name) {
                        last.has_cap = true;

                        if let Some(field) = iter.next() {
                            field0 = field;
                        } else {
                            break;
                        }
                    } else {
                        field0 = field;
                    }
                } else {
                    break;
                }
            } else {
                output.push(field0);
                field0 = field1;
            }
        } else {
            output.push(field0);
            break;
        }
    }

    output
}

fn is_capacity(field: &StructField, base_name: &str) -> bool {
    if let Type::USize = field.ty {
        &field.name[0..base_name.len()] == base_name && &field.name[base_name.len()..] == "_cap"
    } else {
        false
    }
}
