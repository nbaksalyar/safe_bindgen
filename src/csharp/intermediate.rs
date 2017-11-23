//! Types and utilities for the intermediate representation between the rust code
//! and the target language code.

use common;
use inflector::Inflector;
use std::cmp;
use std::collections::BTreeMap;
use syntax::ast;
use syntax::print::pprust;
use syntax::ptr;

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

#[derive(Clone, Debug)]
pub enum ArraySize {
    Lit(usize),
    Const(String),
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

pub struct StructField {
    pub docs: String,
    pub name: String,
    pub ty: Type,
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
        ast::FunctionRetTy::Ty(ref ty) => {
            match transform_type(ty) {
                Some(ty) => ty,
                None => return None,
            }
        }
    };

    let mut inputs = Vec::with_capacity(decl.inputs.len());

    let mut iter = decl.inputs.iter();
    let mut param = None;

    loop {
        if let Some(param) = param.take() {
            inputs.push(param);
        } else {
            if let Some(arg) = iter.next() {
                let (name, ty) = match transform_function_param(arg) {
                    Some(param) => param,
                    None => return None,
                };

                if let Some(next_arg) = iter.next() {
                    let (next_name, next_ty) = match transform_function_param(next_arg) {
                        Some(param) => param,
                        None => return None,
                    };

                    if let Some(new_param) = transform_ptr_and_len_to_array(
                        &name,
                        &ty,
                        &next_name,
                        &next_ty,
                    )
                    {
                        param = Some(new_param);
                    } else {
                        inputs.push((name, ty));
                        param = Some((next_name, next_ty));
                    }
                } else {
                    param = Some((name, ty));
                }
            } else {
                break;
            }
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
    let ty = match transform_type(ty) {
        Some(ty) => ty,
        None => return None,
    };

    let value = match transform_const_value(value) {
        Some(value) => value,
        None => return None,
    };

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
            let name = field.ident.unwrap().name.as_str().to_camel_case();
            let ty = match transform_type(&field.ty) {
                Some(ty) => ty,
                None => return None,
            };

            Some(StructField { docs, name, ty })
        })
        .collect();

    fields.map(|fields| Struct { fields })
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

pub fn extract_callback(ty: &Type) -> Option<&Function> {
    if let Type::Function(ref fun) = *ty {
        if let Some(first) = fun.inputs.first() {
            if is_user_data(&first.0, &first.1) {
                return Some(fun);
            }
        }
    }

    None
}

pub fn extract_callbacks(inputs: &[(String, Type)]) -> Vec<(&str, &Function)> {
    inputs
        .iter()
        .filter_map(|&(ref name, ref ty)| {
            extract_callback(ty).map(|fun| (name.as_str(), fun))
        })
        .collect()
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
        ast::LitKind::Byte(value) => ConstValue::Int(value as i64),
        ast::LitKind::Char(value) => ConstValue::Char(value),
        ast::LitKind::Int(value, _) => ConstValue::Int(value as i64),
        ast::LitKind::Float(ref value, _) |
        ast::LitKind::FloatUnsuffixed(ref value) => ConstValue::Float(value.as_str().to_string()),
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
        .map(|field| if let Some(value) = transform_const_value(
            &*field.expr,
        )
        {
            let name = field.ident.node.name.as_str().to_string();
            Some((name, value))
        } else {
            None
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
        None => return None,
        Some(Type::Array { .. }) => return None, // multi-dimensional array not supported yet
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

    // Matches "foo_ptr"/"foo_len" or "foo"/"foo_len"
    const PTR_SUFFIX: &'static str = "_ptr";
    const LEN_SUFFIX: &'static str = "_len";

    let index = if ptr_name.ends_with(PTR_SUFFIX) {
        ptr_name.len() - PTR_SUFFIX.len()
    } else {
        ptr_name.len()
    };

    let base_name = &ptr_name[0..index];
    let (len_name_0, len_name_1) = len_name.split_at(cmp::min(index, len_name.len()));

    if len_name_0 == base_name && len_name_1 == LEN_SUFFIX {
        Some((
            base_name.to_string(),
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
        "c_char" |
        "libc::c_char" |
        "std::os::raw::c_char" => Type::CChar,
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
        "c_void" |
        "libc::c_void" |
        "std::os::raw::c_void" => Type::Unit,
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
