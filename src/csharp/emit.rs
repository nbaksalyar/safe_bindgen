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

pub fn emit_using_decls(output: &mut IndentedOutput, decls: &BTreeSet<String>) {
    for decl in decls {
        emit!(output, "using {};\n", decl);
    }

    if !decls.is_empty() {
        emit!(output, "\n");
    }
}

pub fn emit_function(output: &mut IndentedOutput, context: &Context, name: &str, item: &Function) {
    if num_callbacks(&item.inputs) <= 1 {
        emit_function_wrapper(output, &name, item);
    }
    emit_function_extern_decl(output, context, &name, item);
}

pub fn emit_callback_delegate(output: &mut IndentedOutput, callback: &Function) {
    emit!(output, "internal delegate void ");
    emit_callback_wrapper_name(output, callback);
    emit!(output, "(");
    emit_managed_function_params(output, &callback.inputs, false);
    emit!(output, ");\n\n");
}

pub fn emit_callback_wrapper(output: &mut IndentedOutput, callback: &Function) {
    emit!(output, "#if __IOS__\n");
    emit!(output, "[MonoPInvokeCallback(typeof(");
    emit_callback_wrapper_name(output, callback);
    emit!(output, "))]\n");
    emit!(output, "#endif\n");

    emit!(output, "private static void On");
    emit_callback_wrapper_name(output, callback);
    emit!(output, "(");
    emit_managed_function_params(output, &callback.inputs, false);
    emit!(output, ") {{\n");
    output.indent();

    emit!(output, "Utilities.CompleteTask(");
    emit_args(output, &callback.inputs[0..2], 0);

    if callback.inputs.len() > 2 {
        emit!(output, ", ");

        if callback.inputs.len() > 3 {
            emit!(output, "(");
        }

        emit_args(output, &callback.inputs[2..], 2);

        if callback.inputs.len() > 3 {
            emit!(output, ")");
        }
    }

    emit!(output, ");\n");

    output.unindent();
    emit!(output, "}}\n\n");
}

pub fn emit_callback_wrapper_name(output: &mut IndentedOutput, callback: &Function) {
    emit_delegate_base_name(output, callback);
    emit!(output, "Cb");
}

pub fn emit_const(output: &mut IndentedOutput, name: &str, item: &Const) {
    emit!(output, "public ");

    match item.value {
        ConstValue::Array(..) |
        ConstValue::Struct(..) => emit!(output, "static readonly "),
        _ => emit!(output, "const "),
    }

    emit_type(output, &item.ty);
    emit!(output, " {} = ", name.to_screaming_snake_case());
    emit_const_value(output, Some(&item.ty), &item.value);
    emit!(output, ";\n");
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

pub fn emit_utilities(
    output: &mut IndentedOutput,
    using_decls: &BTreeSet<String>,
    namespace: &str,
) {
    emit_using_decls(output, using_decls);

    if !namespace.is_empty() {
        emit!(output, "namespace {} {{\n", namespace);
        output.indent();
    }

    emit!(
        output,
        "{}",
        include_str!("../../resources/csharp/Utilities.cs")
    );

    if !namespace.is_empty() {
        output.unindent();
        emit!(output, "}}\n");
    }
}

fn emit_function_wrapper(output: &mut IndentedOutput, name: &str, fun: &Function) {
    let callback = fun.inputs.last().and_then(
        |&(_, ref ty)| extract_callback(ty),
    );

    emit!(output, "public static ");

    if let Some(callback) = callback {
        emit_task(output, &callback.inputs);
    } else {
        emit_type(output, &fun.output);
    }

    emit!(output, " {}(", name.to_pascal_case());
    emit_managed_function_params(output, &fun.inputs, true);
    emit!(output, ") {{\n");
    output.indent();

    if let Some(callback) = callback {
        emit!(output, "var (task, userData) = Utilities.PrepareTask");
        emit_task_generic_args(output, &callback.inputs);
        emit!(output, "();\n");
    } else {
        match fun.output {
            Type::Unit => (),
            _ => emit!(output, "return "),
        }
    }

    emit!(output, "{}(", extern_function_name(name));

    for (index, &(ref name, ref ty)) in fun.inputs.iter().enumerate() {
        if index > 0 {
            emit!(output, ", ");
        }

        if let Some(callback) = extract_callback(ty) {
            emit!(output, "On");
            emit_callback_wrapper_name(output, &callback);
        } else if let Type::Array(_, ArraySize::Dynamic) = *ty {
            let name = name.to_camel_case();
            emit!(output, "{}, (ulong) {}.Length", name, name);
        } else {
            emit!(output, "{}", name.to_camel_case());
        }
    }

    emit!(output, ");\n");

    if callback.is_some() {
        emit!(output, "return task;\n");
    }

    output.unindent();
    emit!(output, "}}\n\n");
}

fn emit_function_extern_decl(
    output: &mut IndentedOutput,
    context: &Context,
    native_name: &str,
    fun: &Function,
) {
    let name = extern_function_name(native_name);

    emit!(
        output,
        "[DllImport(DLL_NAME, EntryPoint = \"{}\")]\n",
        native_name
    );
    emit!(output, "internal static extern ");
    emit_type(output, &fun.output);
    emit!(output, " {}(", name);
    emit_native_function_params(output, context, &fun.inputs, false);
    emit!(output, ");\n\n");
}

fn extern_function_name(name: &str) -> String {
    let mut name = name.to_pascal_case();
    name.push_str("Native");
    name
}

fn emit_task(output: &mut IndentedOutput, params: &[(String, Type)]) {
    emit!(output, "Task");
    emit_task_generic_args(output, params);
}

fn emit_task_generic_args(output: &mut IndentedOutput, params: &[(String, Type)]) {
    // Nore: assuming here the first param is user_data and the second is result.

    if params.len() <= 2 {
        return;
    }

    emit!(output, "<");

    if params.len() > 3 {
        emit!(output, "(");
    }

    for (index, &(_, ref ty)) in params[2..].into_iter().enumerate() {
        if index > 0 {
            emit!(output, ", ");
        }

        emit_type(output, ty);
    }

    if params.len() > 3 {
        emit!(output, ")");
    }

    emit!(output, ">");
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
                emit_type(output, ty);
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
    emit_type(output, ty);
    emit!(output, " {};\n", name);
}

fn emit_managed_function_params(
    output: &mut IndentedOutput,
    params: &[(String, Type)],
    skip_user_data: bool,
) {
    let mut index = 0;
    for &(ref name, ref ty) in params {
        // Skip the user data pointer.
        if skip_user_data && is_user_data(name, ty) {
            continue;
        }

        // Skip callbacks.
        if extract_callback(ty).is_some() {
            continue;
        }

        if index > 0 {
            emit!(output, ", ");
        }

        emit_type(output, ty);
        if name.is_empty() {
            emit!(output, " arg{}", index);
        } else {
            emit!(output, " {}", name.to_camel_case());
        }

        index += 1;
    }

}

fn emit_native_function_params(
    output: &mut IndentedOutput,
    context: &Context,
    params: &[(String, Type)],
    anonymize: bool,
) {
    let mut index = 0;
    for &(ref name, ref ty) in params {
        if index > 0 {
            emit!(output, ", ");
        }

        emit_marshal_as(output, context, ty, Some(index), " ");

        if let Some(callback) = extract_callback(ty) {
            emit_callback_wrapper_name(output, &callback);
        } else {
            emit_type(output, ty);
        }

        let name = param_name(name, index, anonymize);
        emit!(output, " {}", name);

        if let Type::Array(_, ArraySize::Dynamic) = *ty {
            emit!(output, ", ulong {}Len", name);
        }

        index += 1;
    }
}

fn param_name(name: &str, index: usize, anon: bool) -> String {
    if anon || name.is_empty() {
        format!("arg{}", index)
    } else {
        name.to_camel_case()
    }
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
                    emit!(
                        output,
                        ", SizeConst = (int) {}.{}",
                        context.class_name,
                        name
                    )
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

fn emit_type(output: &mut IndentedOutput, ty: &Type) {
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
            emit_type(output, ty);
            emit!(output, "[]")
        }
        Type::Function(..) => unimplemented!(),
        Type::User(ref name) => emit!(output, "{}", name),
    }
}

fn emit_sanitized_type_name(output: &mut IndentedOutput, ty: &Type) {
    match *ty {
        Type::Unit => emit!(output, "Void"),
        Type::Bool => emit!(output, "Bool"),
        Type::Char => emit!(output, "Char"),
        Type::F32 => emit!(output, "Float"),
        Type::F64 => emit!(output, "Double"),
        Type::I8 | Type::CChar => emit!(output, "SByte"),
        Type::I16 => emit!(output, "Short"),
        Type::I32 => emit!(output, "Int"),
        Type::I64 | Type::ISize => emit!(output, "Long"),
        Type::U8 => emit!(output, "Byte"),
        Type::U16 => emit!(output, "UShort"),
        Type::U32 => emit!(output, "UInt"),
        Type::U64 | Type::USize => emit!(output, "ULong"),
        Type::String => emit!(output, "String"),
        Type::Pointer(ref ty) => emit_sanitized_type_name(output, ty),
        Type::Array(ref ty, _) => {
            emit!(output, "ArrayOf");
            emit_sanitized_type_name(output, ty);
        }
        Type::User(ref name) => emit!(output, "{}", name),
        _ => unimplemented!(),
    }
}

fn emit_args(output: &mut IndentedOutput, args: &[(String, Type)], offset: usize) {
    for (index, &(ref name, _)) in args.into_iter().enumerate() {
        if index > 0 {
            emit!(output, ", ");
        }

        if name.is_empty() {
            emit!(output, "arg{}", offset + index);
        } else {
            emit!(output, "{}", name.to_camel_case());
        }
    }
}

fn emit_delegate_base_name(output: &mut IndentedOutput, fun: &Function) {
    if fun.inputs.len() > 1 {
        // Skip the user data param.
        for &(_, ref ty) in &fun.inputs[1..] {
            emit_sanitized_type_name(output, ty);
        }
    } else {
        emit!(output, "None");
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
