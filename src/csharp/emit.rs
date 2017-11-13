//! Utilities for emiting fragments of the target language code.

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

pub fn emit_function_wrapper(output: &mut IndentedOutput, name: &str, fun: &Function) {
    emit_function_decl(
        output,
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
        } else {
            emit!(output, "{}", name.to_camel_case());
        }
    }

    emit!(output, ");\n");

    output.unindent();
    emit!(output, "}}\n\n");
}

pub fn emit_function_extern_decl(
    output: &mut IndentedOutput,
    name: &str,
    fun: &Function,
    lib_name: &str,
) {
    emit!(output, "[DllImport(\"{}\")]\n", lib_name);
    emit_function_decl(output, "private static extern", name, fun, true, false);
    emit!(output, ";\n\n");
}

pub fn emit_struct_field(output: &mut IndentedOutput, access: &str, ty: &Type, name: &str) {
    emit_marshal_as(output, ty, "\n");
    emit!(output, "{} ", access);
    emit_managed_type(output, ty);
    emit!(output, " {};\n", name);
}

pub fn emit_callback_wrappers(output: &mut IndentedOutput, all_arities: &BTreeSet<Vec<usize>>) {
    for arities in all_arities {
        for index in 0..arities.len() {
            emit_callback_wrapper(output, index, &arities);
        }
    }
}

fn emit_function_decl(
    output: &mut IndentedOutput,
    modifiers: &str,
    name: &str,
    fun: &Function,
    marshal_params: bool,
    skip_user_data: bool,
) {
    emit!(output, "{} ", modifiers);
    emit_managed_type(output, &fun.output);
    emit!(output, " {}(", name);

    let mut first = true;
    for &(ref name, ref ty) in &fun.inputs {
        // Skip the user data pointer.
        if skip_user_data && is_user_data(name, ty) {
            continue;
        }

        if first {
            first = false;
        } else {
            emit!(output, ", ");
        }

        if marshal_params {
            emit_marshal_as(output, ty, " ");
        }

        if let Some(fun) = extract_callback(ty) {
            emit_delegate(output, fun, skip_user_data);
        } else {
            emit_managed_type(output, ty);
        }

        emit!(output, " {}", name.to_camel_case());
    }

    emit!(output, ")");

}

fn emit_marshal_as(output: &mut IndentedOutput, ty: &Type, append: &str) {
    if let Some(unmanaged) = unmanaged_type(ty) {
        emit!(output, "[MarshalAs(UnmanagedType.{}", unmanaged);

        if let Type::Array(ref ty, size) = *ty {
            if let Some(unmanaged) = unmanaged_type(ty) {
                emit!(output, ", ArraySubType = UnmanagedType.{}", unmanaged);
            }

            emit!(output, ", SizeConst = {}", size);
        }

        emit!(output, ")]{}", append);
    }
}

fn unmanaged_type(ty: &Type) -> Option<&str> {
    match *ty {
        Type::Bool => Some("Bool"),
        // TODO: consider marshaling as "LPUTF8Str", is possible and useful.
        Type::String => Some("LPStr"),
        Type::Array(..) => Some("ByValArray"),
        _ => None,
    }
}

fn emit_managed_type(output: &mut IndentedOutput, ty: &Type) {
    match *ty {
        Type::Unit => emit!(output, "void"),
        Type::Bool => emit!(output, "bool"),
        Type::CChar => emit!(output, "sbyte"),
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
        Type::Pointer(..) => emit!(output, "IntPtr"),
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
