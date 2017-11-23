use super::*;

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
            let left = $left;
            let right = $right;

            if left != right {
                panic!("assertion failed: `(left == right)`\n```\n{}```\n",
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

        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn structs() {
    let actual = compile!(None, {
        /// This comment
        /// spans multiple lines.
        #[repr(C)]
        pub struct Record {
            /// Comment for the `id` field.
            id: u64,
            /// Comment for the `enabled` field
            /// spans multiple lines.
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

         /// This comment
         /// spans multiple lines.
         [StructLayout(LayoutKind.Sequential)]
         public class Record {
             /// Comment for the `id` field.
             public ulong id;
             /// Comment for the `enabled` field
             /// spans multiple lines.
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
        }

        #[no_mangle]
        pub extern "C" fn fun(id: Id) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;

         [StructLayout(LayoutKind.Sequential)]
         public class Message {
             public ulong id;
             public ulong senderId;
         }

         public static class Backend {
             public static void Fun(ulong id) {
                 fun(id);
             }

             [DllImport(\"backend\")]
             private static extern void fun(ulong id);

         }
         "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn enums() {
    let actual = compile!(None, {
        /// Some nice comment here.
        #[repr(C)]
        pub enum Mode {
            /// Comment for the `ReadOnly` variant.
            ReadOnly,
            /// Comment for the `WriteOnly` variant.
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

         /// Some nice comment here.
         public enum Mode {
             /// Comment for the `ReadOnly` variant.
             ReadOnly,
             /// Comment for the `WriteOnly` variant.
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

        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn explicitly_ignored_functions() {
    let mut lang = LangCSharp::new("backend");
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

         public static class Backend {
             public static void Fun2() {
                 fun2();
             }

             [DllImport(\"backend\")]
             private static extern void fun2();

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

         public static class Backend {
             public static void Fun0(Engine engine) {
                 fun0(engine);
             }

             [DllImport(\"backend\")]
             private static extern void fun0(Engine engine);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_one_callback_param() {
    let actual = compile!(None, {
        /// Comment for `fun1`.
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

         public static class Backend {
             /// Comment for `fun1`.
             public static void Fun1(int num, String name, Action<FfiResult> cb) {
                 var userData = GCHandle.ToIntPtr(GCHandle.Alloc(cb));
                 fun1(num, name, userData, new Action<IntPtr, FfiResult>(Call<FfiResult>));
             }

             [DllImport(\"backend\")]
             private static extern void fun1(\
                int num, \
                [MarshalAs(UnmanagedType.LPStr)] String name, \
                IntPtr userData, \
                Action<IntPtr, FfiResult> cb);

             private static void Call<T0>(IntPtr userData, T0 arg0) {
                 var handle = GCHandle.FromIntPtr(userData);
                 var cb = (Action<T0>) handle.Target;
                 cb(arg0);
                 handle.Free();
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
        pub extern "C" fn fun2(
            user_data: *mut c_void,
            cb0: extern "C" fn(user_data: *mut c_void,
                               result: *const FfiResult,
                               data: *const Data),
            cb1: extern "C" fn(user_data: *mut c_void, result: *const FfiResult),
        ) {
        }
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;

         public static class Backend {
             public static void Fun2(Action<FfiResult, Data> cb0, Action<FfiResult> cb1) {
                 var userData = GCHandle.ToIntPtr(GCHandle.Alloc(Tuple.Create(cb0, cb1)));
                 fun2(userData, \
                      new Action<IntPtr, FfiResult, Data>(Call0_2_1<FfiResult, Data, FfiResult>), \
                      new Action<IntPtr, FfiResult>(Call1_2_1<FfiResult, Data, FfiResult>));
             }

             [DllImport(\"backend\")]
             private static extern void fun2(\
                 IntPtr userData, \
                 Action<IntPtr, FfiResult, Data> cb0, \
                 Action<IntPtr, FfiResult> cb1);

             private static void Call0_2_1<T0, T1, T2>(IntPtr userData, T0 arg0, T1 arg1) {
                 var handle = GCHandle.FromIntPtr(userData);
                 var cbs = (Tuple<Action<T0, T1>, Action<T2>>) handle.Target;
                 cbs.Item1(arg0, arg1);
                 handle.Free();
             }

             private static void Call1_2_1<T0, T1, T2>(IntPtr userData, T2 arg0) {
                 var handle = GCHandle.FromIntPtr(userData);
                 var cbs = (Tuple<Action<T0, T1>, Action<T2>>) handle.Target;
                 cbs.Item2(arg0);
                 handle.Free();
             }

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn functions_with_array_params() {
    let actual = compile!(None, {
        #[no_mangle]
        pub extern "C" fn fun0(ids: *const u8, ids_len: usize) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;

         public static class Backend {
             public static void Fun0(byte[] ids) {
                 fun0(ids, (ulong) ids.Length);
             }

             [DllImport(\"backend\")]
             private static extern void fun0(\
                [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] byte[] ids, \
                ulong idsLen\
             );

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn function_with_opaque_params() {
    let mut lang = LangCSharp::new("backend");
    lang.add_opaque_type("Handle");

    let actual = compile!(lang, {
        #[no_mangle]
        pub extern "C" fn fun0(handle: Handle) {}
    });

    let expected = indoc!(
        "using System;
         using System.Runtime.InteropServices;

         [StructLayout(LayoutKind.Sequential)]
         public struct Handle {
             private IntPtr value;
         }

         public static class Backend {
             public static void Fun0(Handle handle) {
                 fun0(handle);
             }

             [DllImport(\"backend\")]
             private static extern void fun0(Handle handle);

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

         public static class Backend {
             public static bool Fun0(int arg) {
                 return fun0(arg);
             }

             [DllImport(\"backend\")]
             private static extern bool fun0(int arg);

         }
        "
    );

    assert_multiline_eq!(actual, expected);
}

#[test]
fn constants() {
    let mut lang = LangCSharp::new("backend");
    lang.add_custom_decl("public const byte CUSTOM = 45;");

    let actual = compile!(lang, {
        /// Comment for `NUMBER`.
        pub const NUMBER: i32 = 123;
        /// Comment for `STRING`.
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

         public static class Backend {
             #region custom declarations
             public const byte CUSTOM = 45;
             #endregion

             /// Comment for `NUMBER`.
             public const int NUMBER = 123;

             /// Comment for `STRING`.
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

         public static class Backend {
             public const ulong ARRAY_SIZE = 20;

             public static void Fun(byte[] a, byte[] b) {
                 fun(a, b);
             }

             [DllImport(\"backend\")]
             private static extern void fun(\
                 [MarshalAs(UnmanagedType.ByValArray, SizeConst = 10)] byte[] a, \
                 [MarshalAs(UnmanagedType.ByValArray, SizeConst = Backend.ARRAY_SIZE)] byte[] b);

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
    let mut lang = lang.into().unwrap_or_else(|| LangCSharp::new("backend"));

    parse::parse_mod(&mut lang, &ast.module, &mut outputs)?;
    lang.finalise_output(&mut outputs)?;

    Ok(
        outputs
            .into_iter()
            .map(|(_, output)| output)
            .next()
            .unwrap_or(String::new()),
    )
}

fn format_diff(left: &str, right: &str) -> String {
    use diff;
    use std::fmt::Write;
    use colored::*;

    let mut output = String::new();

    for res in diff::lines(left, right) {
        match res {
            diff::Result::Left(line) => writeln!(output, "{}", line.red()).unwrap(),
            diff::Result::Right(line) => writeln!(output, "{}", line.green()).unwrap(),
            diff::Result::Both(line, _) => writeln!(output, "{}", line.white()).unwrap(),
        };
    }

    output
}
