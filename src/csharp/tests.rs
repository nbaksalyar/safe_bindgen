use super::*;
use std::path::Path;

macro_rules! compile {
        ($lang:expr, $rust:tt) => { try_compile!($lang, $rust).unwrap() }
    }

macro_rules! try_compile {
        ($lang:expr, $rust:tt) => {{
            let rust_src = stringify!($rust);
            let rust_src = rust_src[1..rust_src.len() - 1].to_string();
            try_compile($lang, rust_src)
        }}
    }

// This is like `assert_eq`, but produces more readable output for multiline
// strings.
macro_rules! assert_multiline_eq {
        ($left:expr, $right:expr) => {{
            use $crate::colored::*;

            let left = $left;
            let right = $right;

            if left != right {
                panic!("assertion failed: `({} == {})`\n```\n{}```\n",
                       "left".red(),
                       "right".green(),
                       format_diff(&left, &right));
            }
        }}
    }

#[test]
fn non_repr_c_types_are_ignored() {
    let actual = compile!(None, {
        pub struct Foo {
            bar: i32,
        }

        pub enum Meta {
            Foo,
            Bar,
            Baz,
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn structs() {
    let actual = compile!(None, {
        #[repr(C)]
        pub struct Record {
            id: u64,
            enabled: bool,
            name: *const c_char,
            random_numbers: [i32; 10],
            widget: Widget,
            gadgets: [Gadget; 100],
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         [StructLayout(LayoutKind.Sequential)]
         public class Record {
             public ulong id;
             [MarshalAs(UnmanagedType.Bool)]
             public bool enabled;
             [MarshalAs(UnmanagedType.LPStr)]
             public String name;
             [MarshalAs(UnmanagedType.ByValArray, SizeConst = 10)]
             public int[] randomNumbers;
             public Widget widget;
             [MarshalAs(UnmanagedType.ByValArray, SizeConst = 100)]
             public Gadget[] gadgets;
         }

         "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn type_aliases() {
    let actual = compile!(None, {
        pub type Id = u64;
        // Double indirection.
        pub type UserId = Id;

        #[repr(C)]
        pub struct Message {
            id: Id,
            sender_id: UserId,
            receiver_ids: [Id; 10],
        }

        #[no_mangle]
        pub extern "C" fn fun(
            id: Id,
            user_data: *mut c_void,
            cb: extern "C" fn(*mut c_void, *const FfiResult, Id),
        ) {
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         [StructLayout(LayoutKind.Sequential)]
         public class Message {
             public ulong id;
             public ulong senderId;
             [MarshalAs(UnmanagedType.ByValArray, SizeConst = 10)]
             public ulong[] receiverIds;
         }

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static Task<ulong> Fun(ulong id) {
                 var (task, userData) = Utilities.PrepareTask<ulong>();
                 FunNative(id, userData, OnFfiResultULongCb);
                 return task;
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun\")]
             internal static extern void FunNative(ulong id, \
                                                   IntPtr userData, \
                                                   FfiResultULongCb cb);

             internal delegate void FfiResultULongCb(IntPtr arg0, FfiResult arg1, ulong arg2);

             #if __IOS__
             [MonoPInvokeCallback(typeof(FfiResultULongCb))]
             #endif
             private static void OnFfiResultULongCb(IntPtr arg0, FfiResult arg1, ulong arg2) {
                 Utilities.CompleteTask(arg0, arg1, arg2);
             }

         }
         "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn enums() {
    let actual = compile!(None, {
        #[repr(C)]
        pub enum Mode {
            ReadOnly,
            WriteOnly,
            ReadAndWrite,
        }

        #[repr(C)]
        pub enum Binary {
            Zero = 0,
            One = 1,
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public enum Mode {
             ReadOnly,
             WriteOnly,
             ReadAndWrite,
         }

         public enum Binary {
             Zero = 0,
             One = 1,
         }

        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_without_extern_and_no_mangle_are_ignored() {
    let actual = compile!(None, {
        pub extern "C" fn fun1() {}

        #[no_mangle]
        pub fn fun2() {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn explicitly_ignored_functions() {
    let mut lang = LangCSharp::new();
    lang.ignore_function("fun1");

    let actual = compile!(lang, {
        #[no_mangle]
        pub extern "C" fn fun1() {}
        #[no_mangle]
        pub extern "C" fn fun2() {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static void Fun2() {
                 Fun2Native();
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun2\")]
             internal static extern void Fun2Native();

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_no_callback_params() {
    let actual = compile!(None, {
        #[no_mangle]
        pub extern "C" fn fun0(engine: *mut Engine) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static void Fun0(Engine engine) {
                 Fun0Native(engine);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun0\")]
             internal static extern void Fun0Native(Engine engine);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_one_callback_param() {
    let actual = compile!(None, {
        #[no_mangle]
        pub extern "C" fn fun1(
            num: i32,
            name: *const c_char,
            user_data: *mut c_void,
            cb: extern "C" fn(user_data: *mut c_void, result: *const FfiResult),
        ) {
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static Task Fun1(int num, String name) {
                 var (task, userData) = Utilities.PrepareTask();
                 Fun1Native(num, name, userData, OnFfiResultCb);
                 return task;
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun1\")]
             internal static extern void Fun1Native(\
                int num, \
                [MarshalAs(UnmanagedType.LPStr)] String name, \
                IntPtr userData, \
                FfiResultCb cb);

             internal delegate void FfiResultCb(IntPtr userData, FfiResult result);

             #if __IOS__
             [MonoPInvokeCallback(typeof(FfiResultCb))]
             #endif
             private static void OnFfiResultCb(IntPtr userData, FfiResult result) {
                 Utilities.CompleteTask(userData, result);
             }

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_multiple_callback_params() {
    let actual = compile!(None, {
        #[no_mangle]
        pub extern "C" fn fun(
            input: i32,
            user_data: *mut c_void,
            cb0: extern "C" fn(user_data: *mut c_void),
            cb1: extern "C" fn(user_data: *mut c_void,
                               result: *const FfiResult,
                               output: i32),
        ) {
        }
    });

    // Only the native declaration should be produced.
    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             [DllImport(DLL_NAME, EntryPoint = \"fun\")]
             internal static extern void FunNative(int input, \
                                                   IntPtr userData, \
                                                   NoneCb cb0, \
                                                   FfiResultIntCb cb1);

             internal delegate void FfiResultIntCb(IntPtr userData, FfiResult result, int output);

             internal delegate void NoneCb(IntPtr userData);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_array_params() {
    let actual = compile!(None, {
        // Support different naming conventions.
        #[no_mangle]
        pub extern "C" fn fun0(data_ptr: *const u8, data_len: usize) {}
        #[no_mangle]
        pub extern "C" fn fun1(data: *const u8, data_len: usize) {}
        #[no_mangle]
        pub extern "C" fn fun2(data: *const u8, len: usize) {}

        // Params before and/or after the array
        #[no_mangle]
        pub extern "C" fn fun3(id: u64, data: *const u8, len: usize) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static void Fun0(byte[] data) {
                 Fun0Native(data, (ulong) data.Length);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun0\")]
             internal static extern void Fun0Native(\
                [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] byte[] data, \
                ulong dataLen\
             );

             public static void Fun1(byte[] data) {
                 Fun1Native(data, (ulong) data.Length);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun1\")]
             internal static extern void Fun1Native(\
                [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] byte[] data, \
                ulong dataLen\
             );

             public static void Fun2(byte[] data) {
                 Fun2Native(data, (ulong) data.Length);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun2\")]
             internal static extern void Fun2Native(\
                [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] byte[] data, \
                ulong dataLen\
             );

             public static void Fun3(ulong id, byte[] data) {
                 Fun3Native(id, data, (ulong) data.Length);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun3\")]
             internal static extern void Fun3Native(\
                ulong id, \
                [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] byte[] data, \
                ulong dataLen\
             );

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn function_with_opaque_params() {
    let mut lang = LangCSharp::new();
    lang.add_opaque_type("Handle");

    let actual = compile!(lang, {
        #[no_mangle]
        pub extern "C" fn fun0(handle: Handle) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         [StructLayout(LayoutKind.Sequential)]
         public struct Handle {
             private IntPtr value;
         }

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static void Fun0(Handle handle) {
                 Fun0Native(handle);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun0\")]
             internal static extern void Fun0Native(Handle handle);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_return_values() {
    let actual = compile!(None, {
        #[no_mangle]
        pub extern "C" fn fun0(arg: i32) -> bool {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public static bool Fun0(int arg) {
                 return Fun0Native(arg);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun0\")]
             internal static extern bool Fun0Native(int arg);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn constants() {
    let mut lang = LangCSharp::new();
    lang.add_custom_decl("public const byte CUSTOM = 45;");

    let actual = compile!(lang, {
        pub const NUMBER: i32 = 123;
        pub const STRING: &'static str = "hello world";
        pub const ARRAY: [u8; 4] = [0, 1, 2, 3];

        pub const STRUCT_VALUE: Record = Record {
            id: 0,
            secret_code: "xyz",
        };

        pub const STRUCT_REF: &'static Record = &Record {
            id: 1,
            secret_code: "xyz",
        };

        pub const EMPTY_STR: *const c_char = 0 as *const c_char;
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             #region custom declarations
             public const byte CUSTOM = 45;
             #endregion

             public const int NUMBER = 123;
             public const String STRING = \"hello world\";
             public static readonly byte[] ARRAY = new byte[] { 0, 1, 2, 3 };
             public static readonly Record STRUCT_VALUE = new Record { \
                 id = 0, secretCode = \"xyz\" };
             public static readonly Record STRUCT_REF = new Record { \
                 id = 1, secretCode = \"xyz\" };
             public const String EMPTY_STR = \"\";

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn arrays() {
    let actual = compile!(None, {
        pub const ARRAY_SIZE: usize = 20;

        #[no_mangle]
        pub extern "C" fn fun(a: [u8; 10], b: [u8; ARRAY_SIZE]) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         public class Backend {
             #if __IOS__
             internal const String DLL_NAME = \"__Internal\";
             #else
             internal const String DLL_NAME = \"backend\";
             #endif

             public const ulong ARRAY_SIZE = 20;

             public static void Fun(byte[] a, byte[] b) {
                 FunNative(a, b);
             }

             [DllImport(DLL_NAME, EntryPoint = \"fun\")]
             internal static extern void FunNative(\
                 [MarshalAs(UnmanagedType.ByValArray, SizeConst = 10)] \
                 byte[] a, \
                 [MarshalAs(UnmanagedType.ByValArray, SizeConst = (int) Backend.ARRAY_SIZE)] \
                 byte[] b);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn namespace() {
    let mut lang = LangCSharp::new();
    lang.set_namespace("Bindings");

    let actual = compile!(lang, {
        #[repr(C)]
        pub struct Record {
            id: u64,
            name: *const c_char,
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;
         using System.Threading.Tasks;

         namespace Bindings {
             [StructLayout(LayoutKind.Sequential)]
             public class Record {
                 public ulong id;
                 [MarshalAs(UnmanagedType.LPStr)]
                 public String name;
             }

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

fn try_compile<T: Into<Option<LangCSharp>>>(
    lang: T,
    rust_src: String,
) -> Result<String, Vec<Error>> {
    use parse;
    use syntax;

    let session = syntax::parse::ParseSess::new();
    let ast = syntax::parse::parse_crate_from_source_str("lib.rs".to_string(), rust_src, &session)
        .unwrap();

    let mut outputs = Outputs::default();
    let mut lang = lang.into().unwrap_or_else(|| LangCSharp::new());

    parse::parse_mod(&mut lang, &ast.module, &mut outputs)?;
    lang.finalise_output(&mut outputs)?;

    Ok(outputs.remove(Path::new("Backend.cs")).unwrap_or(
        String::new(),
    ))
}

fn format_diff(left: &str, right: &str) -> String {
    use diff;
    use std::fmt::Write;
    use colored::*;

    let mut output = String::new();

    for res in diff::lines(left, right) {
        match res {
            diff::Result::Left(line) => writeln!(output, "{}{}", "-".red(), line.red()).unwrap(),
            diff::Result::Right(line) => {
                writeln!(output, "{}{}", "+".green(), line.green()).unwrap()
            }
            diff::Result::Both(line, _) => writeln!(output, " {}", line.white()).unwrap(),
        };
    }

    output
}
