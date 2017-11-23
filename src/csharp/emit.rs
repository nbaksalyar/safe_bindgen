//! Utilities for emiting fragments of the target language code.

use super::Context;
use super::intermediate::*;
use inflector::Inflector;
use output::IndentedOutput;
use std::collections::BTreeSet;
use std::fmt::Write;

macro_rules! emit {
    ($output:expr, $($arg:tt)*) => {
        write!($output, $($arg)*).unwrap()
    }
}

pub fn emit_function(output: &mut IndentedOutput, context: &Context, name: &str, item: &Function) {
    emit_function_wrapper(output, context, &name, item);
    emit_function_extern_decl(output, context, &name, item);
}

pub fn emit_callback_wrappers(output: &mut IndentedOutput, all_arities: &BTreeSet<Vec<usize>>) {
    for arities in all_arities {
        for index in 0..arities.len() {
            emit_callback_wrapper(output, index, &arities);
        }
    }
}

pub fn emit_const(output: &mut IndentedOutput, name: &str, item: &Const) {
    emit!(output, "public ");

    match item.value {
        ConstValue::Array(..) |
        ConstValue::Struct(..) => emit!(output, "static readonly "),
        _ => emit!(output, "const "),
    }

    emit_managed_type(output, &item.ty);
    emit!(output, " {} = ", name.to_screaming_snake_case());
    emit_const_value(output, Some(&item.ty), &item.value);
    emit!(output, ";\n\n");
}

pub fn emit_enum(output: &mut IndentedOutput, name: &str, item: &Enum) {

    emit!(output, "public enum {} {{\n", name);
    output.indent();

    for variant in &item.variants {
        emit!(output, "{}", variant.docs);

        if let Some(value) = variant.value {
            emit!(output, "{} = {},\n", variant.name, value);
        } else {
            emit!(output, "{},\n", variant.name);
        }
    }

    output.unindent();
    emit!(output, "}}\n\n");
}

pub fn emit_struct(output: &mut IndentedOutput, context: &Context, name: &str, item: &Struct) {
    emit!(output, "[StructLayout(LayoutKind.Sequential)]\n");
    emit!(output, "public class {} {{\n", name);
    output.indent();

    for field in &item.fields {
        emit!(output, "{}", field.docs);
        emit_struct_field(output, context, "public", &field.ty, &field.name);
    }

    output.unindent();
    emit!(output, "}}\n\n");
}

pub fn emit_opaque_type(output: &mut IndentedOutput, name: &str) {
    emit!(output, "[StructLayout(LayoutKind.Sequential)]\n");
    emit!(output, "public struct {} {{\n", name);
    output.indent();
    emit!(output, "private IntPtr value;\n");
    output.unindent();
    emit!(output, "}}\n\n");
}

fn emit_function_wrapper(
    output: &mut IndentedOutput,
    context: &Context,
    name: &str,
    fun: &Function,
) {
    emit_function_decl(
        output,
        context,
        "public static",
        &name.to_pascal_case(),
        fun,
        false,
        true,
    );
    emit!(output, " {{\n");
    output.indent();

    let callbacks = extract_callbacks(&fun.inputs);
    let callback_arities: Vec<_> = callbacks
        .iter()
        .map(|&(_, ref fun)| callback_arity(fun))
        .collect();
    let callback_params: Vec<_> = callbacks
        .iter()
        .flat_map(|&(_, ref fun)| {
            fun.inputs[1..].iter().map(|&(_, ref ty)| ty)
        })
        .collect();
    let mut callback_index = 0;

    if callbacks.len() > 0 {
        emit!(output, "var userData = ");
        emit_delegate_holder(output, &callbacks);
        emit!(output, ";\n");
    }

    match fun.output {
        Type::Unit => (),
        _ => emit!(output, "return "),
    }

    emit!(output, "{}(", name);

    let mut first = true;
    for &(ref name, ref ty) in &fun.inputs {
        if first {
            first = false;
        } else {
            emit!(output, ", ");
        }

        if let Some(fun) = extract_callback(ty) {
            emit!(output, "new ");
            emit_delegate(output, fun, false);
            emit!(output, "(");
            emit_callback_wrapper_name(output, callback_index, &callback_arities);
            emit_generic_args(output, callback_params.iter().cloned());
            emit!(output, ")");

            callback_index += 1;
        } else if let Type::Array(_, ArraySize::Dynamic) = *ty {
            let name = name.to_camel_case();
            emit!(output, "{}, (ulong) {}.Length", name, name);
        } else {
            emit!(output, "{}", name.to_camel_case());
        }
    }

    emit!(output, ");\n");

    output.unindent();
    emit!(output, "}}\n\n");
}

fn emit_function_extern_decl(
    output: &mut IndentedOutput,
    context: &Context,
    name: &str,
    fun: &Function,
) {
    emit!(output, "[DllImport(\"{}\")]\n", context.lib_name);
    emit_function_decl(output, context, "private static", name, fun, true, false);
    emit!(output, ";\n\n");
}

// Emit static method that can be passed to native functions as callback and
// which in turn calls the delegate stored in the `user_data` pointer.
fn emit_callback_wrapper(output: &mut IndentedOutput, index: usize, arities: &[usize]) {
    emit!(output, "private static void ");
    emit_callback_wrapper_name(output, index, arities);
    emit_generic_params(output, arities.iter().sum());
    emit!(output, "(IntPtr userData");

    let offset: usize = arities.iter().take(index).sum();

    for index in 0..arities[index] {
        emit!(output, ", T{} arg{}", offset + index, index);
    }

    emit!(output, ") {{\n");
    output.indent();

    emit!(output, "var handle = GCHandle.FromIntPtr(userData);\n");

    if arities.len() > 1 {
        emit_multiple_callback_invocation(output, index, arities);
    } else {
        emit_single_callback_invocation(output, arities[0], offset)
    }

    output.unindent();
    emit!(output, "}}\n\n");
}

fn emit_const_value(output: &mut IndentedOutput, ty: Option<&Type>, value: &ConstValue) {
    match *value {
        ConstValue::Bool(true) => emit!(output, "true"),
        ConstValue::Bool(false) => emit!(output, "false"),
        ConstValue::Char(value) => emit!(output, "{:?}", value),
        ConstValue::Int(value) => emit!(output, "{}", value),
        ConstValue::Float(ref value) => emit!(output, "{}", value),
        ConstValue::String(ref value) => emit!(output, "{:?}", value),
        ConstValue::Array(ref elements) => {
            if let Some(&Type::Array(ref ty, ..)) = ty {
                emit!(output, "new ");
                emit_managed_type(output, ty);
                emit!(output, "[] ");
            }

            emit!(output, "{{ ");

            for (index, element) in elements.iter().enumerate() {
                if index > 0 {
                    emit!(output, ", ");
                }

                emit_const_value(output, None, element);
            }

            emit!(output, " }}");
        }
        ConstValue::Struct(ref name, ref fields) => {
            emit!(output, "new {} {{ ", name);

            for (index, (name, value)) in fields.into_iter().enumerate() {
                if index > 0 {
                    emit!(output, ", ");
                }

                emit!(output, "{} = ", name.to_camel_case());
                emit_const_value(output, None, value);
            }

            emit!(output, " }}");
        }
    }
}

fn emit_struct_field(
    output: &mut IndentedOutput,
    context: &Context,
    access: &str,
    ty: &Type,
    name: &str,
) {
    emit_marshal_as(output, context, ty, None, "\n");
    emit!(output, "{} ", access);
    emit_managed_type(output, ty);
    emit!(output, " {};\n", name);
}

fn emit_function_decl(
    output: &mut IndentedOutput,
    context: &Context,
    modifiers: &str,
    name: &str,
    fun: &Function,
    native: bool,
    skip_user_data: bool,
) {
    emit!(output, "{} ", modifiers);

    if native {
        emit!(output, "extern ");
    }

    emit_managed_type(output, &fun.output);
    emit!(output, " {}(", name);

    let mut first = true;
    for (index, &(ref name, ref ty)) in fun.inputs.iter().enumerate() {
        // Skip the user data pointer.
        if skip_user_data && is_user_data(name, ty) {
            continue;
        }

        if first {
            first = false;
        } else {
            emit!(output, ", ");
        }

        if native {
            emit_marshal_as(output, context, ty, Some(index), " ");
        }

        if let Some(fun) = extract_callback(ty) {
            emit_delegate(output, fun, skip_user_data);
        } else {
            emit_managed_type(output, ty);
        }

        emit!(output, " {}", name.to_camel_case());

        if native {
            if let Type::Array(_, ArraySize::Dynamic) = *ty {
                emit!(output, ", ulong {}Len", name);
            }
        }
    }

    emit!(output, ")");

}

fn emit_marshal_as(
    output: &mut IndentedOutput,
    context: &Context,
    ty: &Type,
    index: Option<usize>,
    append: &str,
) {
    if let Some(unmanaged) = unmanaged_type(ty) {
        emit!(output, "[MarshalAs(UnmanagedType.{}", unmanaged);

        if let Type::Array(ref ty, ref size) = *ty {
            if let Some(unmanaged) = unmanaged_type(ty) {
                emit!(output, ", ArraySubType = UnmanagedType.{}", unmanaged);
            }

            match *size {
                ArraySize::Lit(value) => emit!(output, ", SizeConst = {}", value),
                ArraySize::Const(ref name) => {
                    emit!(output, ", SizeConst = {}.{}", context.class_name, name)
                }
                ArraySize::Dynamic => {
                    if let Some(index) = index {
                        emit!(output, ", SizeParamIndex = {}", index + 1)
                    }
                }
            }
        }

        emit!(output, ")]{}", append);
    }
}

fn unmanaged_type(ty: &Type) -> Option<&str> {
    match *ty {
        Type::Bool => Some("Bool"),
        // TODO: consider marshaling as "LPUTF8Str", is possible and useful.
        Type::String => Some("LPStr"),
        Type::Array(_, ArraySize::Dynamic) => Some("LPArray"),
        Type::Array(_, _) => Some("ByValArray"),
        _ => None,
    }
}

fn emit_managed_type(output: &mut IndentedOutput, ty: &Type) {
    match *ty {
        Type::Unit => emit!(output, "void"),
        Type::Bool => emit!(output, "bool"),
        Type::CChar => emit!(output, "sbyte"),
        Type::Char => emit!(output, "char"),
        Type::F32 => emit!(output, "float"),
        Type::F64 => emit!(output, "double"),
        Type::I8 => emit!(output, "sbyte"),
        Type::I16 => emit!(output, "short"),
        Type::I32 => emit!(output, "int"),
        Type::I64 => emit!(output, "long"),
        Type::ISize => emit!(output, "long"),
        Type::U8 => emit!(output, "byte"),
        Type::U16 => emit!(output, "ushort"),
        Type::U32 => emit!(output, "uint"),
        Type::U64 => emit!(output, "ulong"),
        Type::USize => emit!(output, "ulong"),
        Type::String => emit!(output, "String"),
        Type::Pointer(ref ty) => {
            match **ty {
                // Pointer to an user type => object reference
                Type::User(ref name) => emit!(output, "{}", name),
                _ => emit!(output, "IntPtr"),
            }
        }
        Type::Array(ref ty, ..) => {
            emit_managed_type(output, ty);
            emit!(output, "[]")
        }
        Type::Function(..) => unimplemented!(),
        Type::User(ref name) => emit!(output, "{}", name),
    }
}

fn emit_delegate(output: &mut IndentedOutput, fun: &Function, skip_first: bool) {
    let params = fun.inputs.iter().map(|&(_, ref ty)| ty);

    emit!(output, "Action");

    if skip_first {
        emit_generic_args(output, params.skip(1));
    } else {
        emit_generic_args(output, params);
    }
}

// Emits generic argument (on instantiation) list, e.g.: `<int, bool, MyType>`.
fn emit_generic_args<'a, T: IntoIterator<Item = &'a Type>>(output: &mut IndentedOutput, params: T) {
    let mut params = params.into_iter().peekable();
    if params.peek().is_none() {
        return;
    }

    emit!(output, "<");

    let mut first = true;
    for ty in params {
        if first {
            first = false;
        } else {
            emit!(output, ", ");
        }

        emit_managed_type(output, ty);
    }

    emit!(output, ">");
}

// Emits generic parameter (on definition) list, e.g.: `<T0, T1>`.
fn emit_generic_params(output: &mut IndentedOutput, count: usize) {
    if count == 0 {
        return;
    }

    emit!(output, "<");

    if count > 0 {
        emit!(output, "T0");
    }

    for index in 1..count {
        emit!(output, ", T{}", index);
    }

    emit!(output, ">");
}

fn emit_callback_wrapper_name(output: &mut IndentedOutput, index: usize, arities: &[usize]) {
    emit!(output, "Call");

    if arities.len() > 1 {
        emit!(output, "{}", index);

        for &arity in arities {
            emit!(output, "_{}", arity);
        }
    }
}

// Emit callback invocation code for functions that have only one callback.
fn emit_single_callback_invocation(output: &mut IndentedOutput, arity: usize, offset: usize) {
    emit!(output, "var cb = (");
    emit_generic_delegate(output, arity, offset);
    emit!(output, ") handle.Target;\n");

    emit!(output, "cb(");
    emit_anonymous_args(output, arity);
    emit!(output, ");\n");
    emit!(output, "handle.Free();\n");
}

// Emit callback invocation code for functions that have multiple callbacks.
fn emit_multiple_callback_invocation(output: &mut IndentedOutput, index: usize, arities: &[usize]) {
    emit!(output, "var cbs = (Tuple<");

    let mut offset = 0;
    let mut first = true;
    for &arity in arities {
        if first {
            first = false
        } else {
            emit!(output, ", ");
        }

        emit_generic_delegate(output, arity, offset);
        offset += arity;
    }

    emit!(output, ">) handle.Target;\n");
    emit!(output, "cbs.Item{}(", index + 1);
    emit_anonymous_args(output, arities[index]);
    emit!(output, ");\n");
    emit!(output, "handle.Free();\n");
}

fn emit_generic_delegate(output: &mut IndentedOutput, arity: usize, offset: usize) {
    emit!(output, "Action");

    if arity > 0 {
        emit!(output, "<T{}", offset);
    }

    for index in 1..arity {
        emit!(output, ", T{}", offset + index);
    }

    if arity > 0 {
        emit!(output, ">");
    }
}

fn emit_anonymous_args(output: &mut IndentedOutput, arity: usize) {
    if arity > 0 {
        emit!(output, "arg0");
    }

    for index in 1..arity {
        emit!(output, ", arg{}", index);
    }
}

fn emit_delegate_holder(output: &mut IndentedOutput, callbacks: &[(&str, &Function)]) {
    emit!(output, "GCHandle.ToIntPtr(GCHandle.Alloc(");

    if callbacks.len() > 1 {
        emit!(output, "Tuple.Create(");

        let mut first = true;
        for &(name, _) in callbacks {
            if first {
                first = false;
            } else {
                emit!(output, ", ");
            }

            emit!(output, "{}", name.to_camel_case());
        }

        emit!(output, ")");
    } else {
        emit!(output, "{}", callbacks[0].0.to_camel_case());
    }

    emit!(output, "))");
}
