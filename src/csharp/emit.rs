//! Utilities for emiting fragments of the target language code.

use super::Context;
use super::intermediate::*;
use inflector::Inflector;
use output::IndentedOutput;
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

pub fn emit_callback_wrappers(output: &mut IndentedOutput, callbacks: &[(&str, &Function)]) {
    for index in 0..callbacks.len() {
        emit_callback_wrapper(output, callbacks, index);
    }
}

pub fn emit_callback_wrapper_name(
    output: &mut IndentedOutput,
    funs: &[(&str, &Function)],
    index: usize,
) {
    for (index, &(_, fun)) in funs.into_iter().enumerate() {
        if index > 0 {
            emit!(output, "And");
        }

        emit_delegate_base_name(output, fun);
    }

    emit!(output, "Cb");

    if funs.len() > 1 {
        emit!(output, "{}", index);
    }
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
    );
    emit!(output, " {{\n");
    output.indent();

    let callbacks = extract_callbacks(&fun.inputs);
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

    emit!(output, "{}(", extern_function_name(name));

    let mut first = true;
    for &(ref name, ref ty) in &fun.inputs {
        if first {
            first = false;
        } else {
            emit!(output, ", ");
        }

        if extract_callback(ty).is_some() {
            emit!(output, "On");
            emit_callback_wrapper_name(output, &callbacks, callback_index);
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
    native_name: &str,
    fun: &Function,
) {
    let name = extern_function_name(native_name);

    emit!(
        output,
        "[DllImport(DLL_NAME, EntryPoint = \"{}\")]\n",
        native_name
    );
    emit_function_decl(output, context, "private static", &name, fun, true);
    emit!(output, ";\n\n");
}

fn extern_function_name(name: &str) -> String {
    let mut name = name.to_pascal_case();
    name.push_str("Native");
    name
}

fn emit_callback_wrapper(output: &mut IndentedOutput, funs: &[(&str, &Function)], index: usize) {
    let params = &funs[index].1.inputs;

    emit!(output, "private delegate void ");
    emit_callback_wrapper_name(output, funs, index);
    emit!(output, "(");
    emit_managed_function_params(output, params, false, true);
    emit!(output, ");\n\n");

    emit!(output, "#if __IOS__\n");
    emit!(output, "[MonoPInvokeCallback(typeof(");
    emit_callback_wrapper_name(output, funs, index);
    emit!(output, "))]\n");
    emit!(output, "#endif\n");

    emit!(output, "private static void On");
    emit_callback_wrapper_name(output, funs, index);
    emit!(output, "(");
    emit_managed_function_params(output, params, false, true);
    emit!(output, ") {{\n");
    output.indent();

    emit!(output, "var handle = GCHandle.FromIntPtr(arg0);\n");
    emit!(output, "var cb = (");

    if funs.len() > 1 {
        emit!(output, "Tuple<");
    }

    for (index, &(_, fun)) in funs.into_iter().enumerate() {
        if index > 0 {
            emit!(output, ", ");
        }

        emit_action(output, fun, true);
    }

    if funs.len() > 1 {
        emit!(output, ">");
    }

    emit!(output, ") handle.Target;\n");

    if funs.len() > 1 {
        emit!(output, "cb.Item{}(", index + 1);
    } else {
        emit!(output, "cb(");
    }

    for index in 1..params.len() {
        if index > 1 {
            emit!(output, ", ");
        }

        emit!(output, "arg{}", index);
    }

    emit!(output, ");\n");
    emit!(output, "handle.Free();\n");

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

fn emit_function_decl(
    output: &mut IndentedOutput,
    context: &Context,
    modifiers: &str,
    name: &str,
    fun: &Function,
    native: bool,
) {
    emit!(output, "{} ", modifiers);

    if native {
        emit!(output, "extern ");
    }

    emit_type(output, &fun.output);
    emit!(output, " {}(", name);
    if native {
        emit_native_function_params(output, context, &fun.inputs, false);
    } else {
        emit_managed_function_params(output, &fun.inputs, true, false);
    }
    emit!(output, ")");
}

fn emit_managed_function_params(
    output: &mut IndentedOutput,
    params: &[(String, Type)],
    skip_user_data: bool,
    anonymize: bool,
) {
    let mut index = 0;
    for &(ref name, ref ty) in params {
        // Skip the user data pointer.
        if skip_user_data && is_user_data(name, ty) {
            continue;
        }

        if index > 0 {
            emit!(output, ", ");
        }

        if let Some(fun) = extract_callback(ty) {
            emit_action(output, fun, true);
        } else {
            emit_type(output, ty);
        }

        let name = param_name(name, index, anonymize);
        emit!(output, " {}", name);

        index += 1;
    }

}

fn emit_native_function_params(
    output: &mut IndentedOutput,
    context: &Context,
    params: &[(String, Type)],
    anonymize: bool,
) {

    let callbacks = extract_callbacks(params);

    let mut index = 0;
    let mut callback_index = 0;

    for &(ref name, ref ty) in params {
        if index > 0 {
            emit!(output, ", ");
        }

        emit_marshal_as(output, context, ty, Some(index), " ");

        if extract_callback(ty).is_some() {
            emit_callback_wrapper_name(output, &callbacks, callback_index);
            callback_index += 1;
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

fn emit_action(output: &mut IndentedOutput, fun: &Function, skip_first: bool) {
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

        emit_type(output, ty);
    }

    emit!(output, ">");
}

fn emit_delegate_base_name(output: &mut IndentedOutput, fun: &Function) {
    for &(_, ref ty) in fun.inputs.iter().skip(1) {
        emit_sanitized_type_name(output, ty);
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
