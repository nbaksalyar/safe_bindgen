//! Utilities for emiting fragments of the target language code.

use super::Context;
use super::intermediate::*;
use inflector::Inflector;
use output::IndentedWriter;
use std::fmt::Write;

macro_rules! emit {
    ($writer:expr, $($arg:tt)*) => {
        write!($writer, $($arg)*).unwrap()
    }
}

pub fn emit_wrapper_function_decl(
    writer: &mut IndentedWriter,
    context: &Context,
    modifiers: &str,
    name: &str,
    fun: &Function,
) {
    if !modifiers.is_empty() {
        emit!(writer, "{} ", modifiers);
    }

    if let Some(callback) = extract_first_callback(&fun.inputs) {
        emit_task(writer, context, &callback.inputs);
        emit!(writer, " {}Async(", name.to_pascal_case());
    } else {
        emit_type(writer, context, &fun.output, PointerMode::Ref);
        emit!(writer, " {}(", name.to_pascal_case());
    }

    emit_wrapper_function_params(writer, context, &fun.inputs, true);
    emit!(writer, ")");
}

pub fn emit_wrapper_function(
    writer: &mut IndentedWriter,
    context: &Context,
    name: &str,
    fun: &Function,
) {
    let callback = extract_first_callback(&fun.inputs);

    emit_wrapper_function_decl(writer, context, "public", name, fun);
    emit!(writer, " {{\n");
    writer.indent();

    if let Some(callback) = callback {
        emit!(
            writer,
            "var (task, userData) = {}.PrepareTask",
            &context.utils_class_name
        );
        emit_task_generic_args(writer, context, &callback.inputs);
        emit!(writer, "();\n");
    } else {
        match fun.output {
            Type::Unit => (),
            _ => emit!(writer, "return "),
        }
    }

    emit!(writer, "{}(", extern_function_name(name));

    for (index, &(ref name, ref ty)) in fun.inputs.iter().enumerate() {
        if index > 0 {
            emit!(writer, ", ");
        }

        if let Some(callback) = extract_callback(ty) {
            emit!(writer, "On");
            emit_callback_wrapper_name(writer, &callback);
        } else {
            let name = param_name(name, index);

            if let Type::Array(_, ArraySize::Dynamic) = *ty {
                emit!(writer, "{}, (IntPtr) {}.Length", name, name)
            } else if let Type::Pointer(ref ty) = *ty {
                emit_pointer_use(
                    writer,
                    context,
                    &**ty,
                    &name.to_camel_case(),
                    PointerMode::Ref,
                )
            } else {
                emit!(writer, "{}", name)
            }
        }
    }

    emit!(writer, ");\n");

    if callback.is_some() {
        emit!(writer, "return task;\n");
    }

    writer.unindent();
    emit!(writer, "}}\n\n");
}

pub fn emit_function_extern_decl(
    writer: &mut IndentedWriter,
    context: &Context,
    native_name: &str,
    fun: &Function,
) {
    let name = extern_function_name(native_name);

    emit!(
        writer,
        "[DllImport(DLL_NAME, EntryPoint = \"{}\")]\n",
        native_name
    );
    emit!(writer, "internal static extern ");
    emit_type(writer, context, &fun.output, PointerMode::Ref);
    emit!(writer, " {}(", name);
    emit_native_function_params(writer, context, &fun.inputs);
    emit!(writer, ");\n\n");
}

pub fn emit_callback_delegate(writer: &mut IndentedWriter, context: &Context, callback: &Function) {
    emit!(writer, "internal delegate void ");
    emit_callback_wrapper_name(writer, callback);
    emit!(writer, "(");
    emit_callback_params(writer, context, &callback.inputs);
    emit!(writer, ");\n\n");
}

pub fn emit_callback_wrapper(writer: &mut IndentedWriter, context: &Context, callback: &Function) {
    emit!(writer, "#if __IOS__\n");
    emit!(writer, "[MonoPInvokeCallback(typeof(");
    emit_callback_wrapper_name(writer, callback);
    emit!(writer, "))]\n");
    emit!(writer, "#endif\n");

    emit!(writer, "private static void On");
    emit_callback_wrapper_name(writer, callback);
    emit!(writer, "(");
    emit_callback_params(writer, context, &callback.inputs);
    emit!(writer, ") {{\n");
    writer.indent();

    emit!(writer, "{}.CompleteTask(", &context.utils_class_name);
    emit_args(writer, context, &callback.inputs[0..2], 0, PointerMode::Ref);

    if callback.inputs.len() > 2 {
        emit!(writer, ", ");

        if callback.inputs.len() > 3 {
            emit!(writer, "(");
        }

        emit_args(
            writer,
            context,
            &callback.inputs[2..],
            2,
            PointerMode::Deref,
        );

        if callback.inputs.len() > 3 {
            emit!(writer, ")");
        }
    }

    emit!(writer, ");\n");

    writer.unindent();
    emit!(writer, "}}\n\n");
}

pub fn emit_callback_wrapper_name(writer: &mut IndentedWriter, callback: &Function) {
    emit_delegate_base_name(writer, callback);
    emit!(writer, "Cb");
}

pub fn emit_const(writer: &mut IndentedWriter, context: &Context, name: &str, item: &Const) {
    emit!(writer, "public ");

    match item.value {
        ConstValue::Array(..) |
        ConstValue::Struct(..) => emit!(writer, "static readonly "),
        _ => emit!(writer, "const "),
    }

    emit_type(writer, context, &item.ty, PointerMode::Opaque);
    emit!(writer, " {} = ", name.to_pascal_case());
    emit_const_value(writer, context, Some(&item.ty), &item.value);
    emit!(writer, ";\n");
}

pub fn emit_enum(writer: &mut IndentedWriter, context: &Context, name: &str, item: &Enum) {
    emit!(writer, "public enum {} {{\n", name);
    writer.indent();

    for variant in &item.variants {
        emit_docs(writer, context, &variant.docs);

        if let Some(value) = variant.value {
            emit!(writer, "{} = {},\n", variant.name, value);
        } else {
            emit!(writer, "{},\n", variant.name);
        }
    }

    writer.unindent();
    emit!(writer, "}}\n\n");
}

pub fn emit_struct(writer: &mut IndentedWriter, context: &Context, name: &str, item: &Struct) {
    emit!(writer, "public struct {}", name);
    emit!(writer, " {{\n");
    writer.indent();

    for field in &item.fields {
        emit_docs(writer, context, &field.docs);
        emit_struct_field(writer, context, &field.ty, &field.name);
    }

    writer.unindent();
    emit!(writer, "}}\n\n");
}

pub fn emit_utilities(writer: &mut IndentedWriter, context: &Context) {
    let content = include_str!("../../resources/csharp/Utils.cs.template");
    let content = content.replace("@Namespace", &context.namespace);
    let content = content.replace("@Class", &context.utils_class_name);

    emit!(writer, "{}", content);
}

pub fn emit_docs(writer: &mut IndentedWriter, context: &Context, docs: &str) {
    if context.preserve_comments {
        emit!(writer, "{}", docs);
    }
}


fn extern_function_name(name: &str) -> String {
    let mut name = name.to_pascal_case();
    name.push_str("Native");
    name
}

fn emit_task(writer: &mut IndentedWriter, context: &Context, params: &[(String, Type)]) {
    emit!(writer, "Task");
    emit_task_generic_args(writer, context, params);
}

fn emit_task_generic_args(
    writer: &mut IndentedWriter,
    context: &Context,
    params: &[(String, Type)],
) {
    // Nore: assuming here the first param is user_data and the second is result.

    if params.len() <= 2 {
        return;
    }

    emit!(writer, "<");

    if params.len() > 3 {
        emit!(writer, "(");
    }

    for (index, &(_, ref ty)) in params[2..].into_iter().enumerate() {
        if index > 0 {
            emit!(writer, ", ");
        }

        emit_type(writer, context, ty, PointerMode::Deref);
    }

    if params.len() > 3 {
        emit!(writer, ")");
    }

    emit!(writer, ">");
}

fn emit_const_value(
    writer: &mut IndentedWriter,
    context: &Context,
    ty: Option<&Type>,
    value: &ConstValue,
) {
    match *value {
        ConstValue::Bool(true) => emit!(writer, "true"),
        ConstValue::Bool(false) => emit!(writer, "false"),
        ConstValue::Char(value) => emit!(writer, "{:?}", value),
        ConstValue::Int(value) => emit!(writer, "{}", value),
        ConstValue::Float(ref value) => emit!(writer, "{}", value),
        ConstValue::String(ref value) => emit!(writer, "{:?}", value),
        ConstValue::Array(ref elements) => {
            if let Some(&Type::Array(ref ty, ..)) = ty {
                emit!(writer, "new ");
                emit_type(writer, context, ty, PointerMode::Opaque);
                emit!(writer, "[] ");
            }

            emit!(writer, "{{ ");

            for (index, element) in elements.iter().enumerate() {
                if index > 0 {
                    emit!(writer, ", ");
                }

                emit_const_value(writer, context, None, element);
            }

            emit!(writer, " }}");
        }
        ConstValue::Struct(ref name, ref fields) => {
            emit!(writer, "new {} {{ ", name);

            for (index, (name, value)) in fields.into_iter().enumerate() {
                if index > 0 {
                    emit!(writer, ", ");
                }

                emit!(writer, "{} = ", name.to_camel_case());
                emit_const_value(writer, context, None, value);
            }

            emit!(writer, " }}");
        }
    }
}

fn emit_const_use(writer: &mut IndentedWriter, context: &Context, name: &str) {
    emit!(
        writer,
        "{}.{}",
        context.consts_class_name,
        name.to_pascal_case()
    );
}

fn emit_struct_field(writer: &mut IndentedWriter, context: &Context, ty: &Type, name: &str) {
    emit_marshal_as(writer, context, ty, None, "\n");
    emit!(writer, "public ");
    emit_type(writer, context, ty, PointerMode::Opaque);
    emit!(writer, " {};\n", name.to_pascal_case());
}

fn emit_wrapper_function_params(
    writer: &mut IndentedWriter,
    context: &Context,
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
            emit!(writer, ", ");
        }

        emit_type(writer, context, ty, PointerMode::Ref);
        if name.is_empty() {
            emit!(writer, " arg{}", index);
        } else {
            emit!(writer, " {}", name.to_camel_case());
        }

        index += 1;
    }
}

fn emit_native_function_params(
    writer: &mut IndentedWriter,
    context: &Context,
    params: &[(String, Type)],
) {
    let mut index = 0;
    for &(ref name, ref ty) in params {
        if index > 0 {
            emit!(writer, ", ");
        }

        emit_marshal_as(writer, context, ty, Some(index), " ");

        if let Some(callback) = extract_callback(ty) {
            emit_callback_wrapper_name(writer, &callback);
        } else {
            emit_type(writer, context, ty, PointerMode::Ref);
        }

        let name = param_name(name, index);
        emit!(writer, " {}", name);

        if let Type::Array(_, ArraySize::Dynamic) = *ty {
            emit!(writer, ", IntPtr {}Len", name);
        }

        index += 1;
    }
}

fn emit_callback_params(writer: &mut IndentedWriter, context: &Context, params: &[(String, Type)]) {
    for (index, &(ref name, ref ty)) in params.into_iter().enumerate() {
        if index > 0 {
            emit!(writer, ", ");
        }

        let name = param_name(name, index);

        match *ty {
            Type::Array(_, ArraySize::Dynamic) => {
                emit!(writer, "IntPtr {}Ptr, ulong {}Len", name, name);
            }
            Type::Array(_, _) => {
                emit!(writer, "IntPtr {}Ptr", name);
            }
            _ => {
                emit_type(writer, context, ty, PointerMode::Ref);
                emit!(writer, " {}", name);
            }
        }
    }
}

fn param_name(name: &str, index: usize) -> String {
    if name.is_empty() {
        format!("arg{}", index)
    } else {
        name.to_camel_case()
    }
}

fn emit_marshal_as(
    writer: &mut IndentedWriter,
    context: &Context,
    ty: &Type,
    index: Option<usize>,
    append: &str,
) {
    if let Some(unmanaged) = unmanaged_type(ty) {
        emit!(writer, "[MarshalAs(UnmanagedType.{}", unmanaged);

        if let Type::Array(ref ty, ref size) = *ty {
            if let Some(unmanaged) = unmanaged_type(ty) {
                emit!(writer, ", ArraySubType = UnmanagedType.{}", unmanaged);
            }

            match *size {
                ArraySize::Lit(value) => emit!(writer, ", SizeConst = {}", value),
                ArraySize::Const(ref name) => {
                    emit!(writer, ", SizeConst = (int) ");
                    emit_const_use(writer, context, name);
                }
                ArraySize::Dynamic => {
                    if let Some(index) = index {
                        emit!(writer, ", SizeParamIndex = {}", index + 1)
                    }
                }
            }
        }

        emit!(writer, ")]{}", append);
    }
}

fn unmanaged_type(ty: &Type) -> Option<&str> {
    match *ty {
        Type::Bool => Some("U1"),
        // TODO: consider marshaling as "LPUTF8Str", is possible and useful.
        Type::String => Some("LPStr"),
        Type::Array(_, ArraySize::Dynamic) => Some("LPArray"),
        Type::Array(_, _) => Some("ByValArray"),
        _ => None,
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum PointerMode {
    Opaque,
    Ref,
    Deref,
}

fn emit_type(writer: &mut IndentedWriter, context: &Context, ty: &Type, mode: PointerMode) {
    match *ty {
        Type::Unit => emit!(writer, "void"),
        Type::Bool => emit!(writer, "bool"),
        Type::CChar => emit!(writer, "sbyte"),
        Type::Char => emit!(writer, "char"),
        Type::F32 => emit!(writer, "float"),
        Type::F64 => emit!(writer, "double"),
        Type::I8 => emit!(writer, "sbyte"),
        Type::I16 => emit!(writer, "short"),
        Type::I32 => emit!(writer, "int"),
        Type::I64 => emit!(writer, "long"),
        Type::ISize => emit!(writer, "IntPtr"),
        Type::U8 => emit!(writer, "byte"),
        Type::U16 => emit!(writer, "ushort"),
        Type::U32 => emit!(writer, "uint"),
        Type::U64 => emit!(writer, "ulong"),
        Type::USize => emit!(writer, "IntPtr"),
        Type::String => emit!(writer, "string"),
        Type::Pointer(ref ty) => {
            match **ty {
                Type::User(ref name) => {
                    if context.opaque_types.contains(name) {
                        emit!(writer, "IntPtr")
                    } else {
                        match mode {
                            PointerMode::Opaque => emit!(writer, "IntPtr"),
                            PointerMode::Ref => emit!(writer, "ref {}", name),
                            PointerMode::Deref => emit!(writer, "{}", name),
                        }
                    }
                }
                Type::Pointer(_) => {
                    if mode == PointerMode::Ref {
                        emit!(writer, "out IntPtr")
                    } else {
                        emit!(writer, "IntPtr")
                    }
                }
                _ => emit!(writer, "IntPtr"),
            }
        }
        Type::Array(ref ty, ..) => {
            emit_type(writer, context, ty, mode);
            emit!(writer, "[]")
        }
        Type::Function(..) => unimplemented!(),
        Type::User(ref name) => emit!(writer, "{}", name),
    }
}

fn emit_args(
    writer: &mut IndentedWriter,
    context: &Context,
    args: &[(String, Type)],
    offset: usize,
    mode: PointerMode,
) {
    for (index, &(ref name, ref ty)) in args.into_iter().enumerate() {
        if index > 0 {
            emit!(writer, ", ");
        }

        let name = param_name(name, offset + index);
        match *ty {
            Type::Array(ref ty, ref size) => {
                emit_copy_utility_name(writer, context, ty);
                emit!(writer, "({}Ptr, ", name);

                match *size {
                    ArraySize::Lit(value) => emit!(writer, "{}", value),
                    ArraySize::Const(ref name) => emit_const_use(writer, context, name),
                    ArraySize::Dynamic => emit!(writer, "{}Len", name),
                }

                emit!(writer, ")");
            }
            Type::Pointer(ref ty) => emit_pointer_use(writer, context, &**ty, &name, mode),
            _ => emit!(writer, "{}", name),
        }
    }
}

fn emit_pointer_use(
    writer: &mut IndentedWriter,
    context: &Context,
    ty: &Type,
    name: &str,
    mode: PointerMode,
) {
    match *ty {
        Type::User(ref pointee)
            if mode == PointerMode::Ref && !context.opaque_types.contains(pointee) => {
            emit!(writer, "ref {}", name);
        }
        Type::Pointer(_) if mode == PointerMode::Ref => {
            emit!(writer, "out {}", name);
        }
        _ => emit!(writer, "{}", name),
    }
}

fn emit_delegate_base_name(writer: &mut IndentedWriter, fun: &Function) {
    if fun.inputs.len() > 1 {
        // Skip the user data param.
        for &(_, ref ty) in &fun.inputs[1..] {
            emit_delegate_base_part_name(writer, ty);
        }
    } else {
        emit!(writer, "None");
    }
}

fn emit_delegate_base_part_name(writer: &mut IndentedWriter, ty: &Type) {
    match *ty {
        Type::Unit => emit!(writer, "Void"),
        Type::Bool => emit!(writer, "Bool"),
        Type::Char => emit!(writer, "Char"),
        Type::F32 => emit!(writer, "Float"),
        Type::F64 => emit!(writer, "Double"),
        Type::I8 | Type::CChar => emit!(writer, "SByte"),
        Type::I16 => emit!(writer, "Short"),
        Type::I32 => emit!(writer, "Int"),
        Type::I64 | Type::ISize => emit!(writer, "Long"),
        Type::U8 => emit!(writer, "Byte"),
        Type::U16 => emit!(writer, "UShort"),
        Type::U32 => emit!(writer, "UInt"),
        Type::U64 | Type::USize => emit!(writer, "ULong"),
        Type::String => emit!(writer, "String"),
        Type::Pointer(ref ty) => emit_delegate_base_part_name(writer, ty),
        Type::Array(ref ty, ref size) => {
            emit_delegate_base_part_name(writer, ty);

            match *size {
                ArraySize::Lit(value) => emit!(writer, "Array{}", value),
                ArraySize::Const(ref name) => emit!(writer, "Array{}", name.to_pascal_case()),
                ArraySize::Dynamic => emit!(writer, "List"),
            }
        }
        Type::User(ref name) => emit!(writer, "{}", name),
        _ => unimplemented!(),
    }
}

fn emit_copy_utility_name(writer: &mut IndentedWriter, context: &Context, ty: &Type) {
    emit!(writer, "{}.CopyTo", context.utils_class_name);
    match *ty {
        Type::F32 => emit!(writer, "SingleArray"),
        Type::F64 => emit!(writer, "DoubleArray"),
        Type::I16 => emit!(writer, "Int16Array"),
        Type::I32 => emit!(writer, "Int32Array"),
        Type::I64 => emit!(writer, "Int64Array"),
        Type::U8 => emit!(writer, "ByteArray"),
        Type::User(ref name) => emit!(writer, "ObjectArray<{}>", name),
        _ => panic!("cannot emit copy utility name for array of {:?}", ty),
    }
}
