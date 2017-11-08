//! moz-cheddar is a library for converting Rust source files into C header files.
//!
//! **A note on versioning:**
//! While moz-cheddar is still pre-`v1.0.0` it will likely go through
//! numerous breaking changes. We attempt to follow semver and bump
//! the minor version any time a new feature is added or output
//! behavior is changed.
//!
//! moz-cheddar targets C99 or later (for single line comments, and
//! use of `stdint.h` and `stdbool.h`).
//!
//! The most useful way to use moz-cheddar is in a build script.
//! To do this add the following `build-dependencies` section to
//! your `Cargo.toml` (to use it as a normal library simply replace
//! `build-dependencies` with `dependencies`):
//!
//! ```toml
//! # Cargo.toml
//!
//! [build-dependencies]
//! moz-cheddar = "0.4.0"
//! ```
//!
//! Then create the following `build.rs`:
//!
//! ```no_run
//! // build.rs
//!
//! extern crate cheddar;
//!
//! fn main() {
//!     cheddar::Cheddar::new().expect("could not read manifest")
//!         .run_build("include/my_header.h");
//! }
//! ```
//!
//! This should work as is providing you've set up your project correctly.
//! **Don't forget to add a `build = ...` to your `[package]` section,
//! see [the cargo docs] for more info.**
//!
//! moz-cheddar will then create a `my_header.h` file in `include/`.
//! Note that moz-cheddar emits very few warnings, it is up to the
//! programmer to write a library which can be correctly called from C.
//!
//! There are also `.compile()` and `.compile_code()` methods for finer control.
//!
//! # Conversions
//!
//! In the examples below, boilerplate has been omitted from the header.
//!
//! ## Typedefs
//!
//! moz-cheddar converts `pub type A = B` into `typedef B A;`.
//! Types containing generics are ignored.
//!
//! Rust:
//!
//! ```ignore
//! type UInt32 = u32;
//! pub type UInt64 = u64;
//! pub type MyOption<T> = Option<T>
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef uint64_t UInt64;
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Enums
//!
//! moz-cheddar will convert public enums which are marked `#[repr(C)]`.
//! If the enum is generic or contains tuple or struct variants then
//! `cheddar` will fail. moz-cheddar should correctly handle explicit
//! discriminants.
//!
//! Rust:
//!
//! ```ignore
//! #[repr(C)]
//! pub enum Colours {
//!     Red = -6,
//!     Blue,
//!     Green = 7,
//!     Yellow,
//! }
//!
//! // This would fail if it was #[repr(C)].
//! pub enum Tastes<T> {
//!     Savoury(T),
//!     Sweet,
//! }
//!
//! // This would fail if it was public.
//! #[repr(C)]
//! enum Units {
//!     Kg(f64),
//!     M(f64),
//!     S(f64),
//!     A(f64),
//!     K(f64),
//!     Mol(f64),
//!     Cd(f64),
//! }
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef enum Colours {
//!         Colours_Red = -6,
//!         Colours_Blue,
//!         Colours_Green = 7,
//!         Colours_Yellow,
//! } Colours;
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Structs
//!
//! Structs are handled very similarly to enums, they must be public,
//! marked `#[repr(C)]`, and they must not contain generics.
//! This currently only checked at the struct-level.
//! Generic fields are not checked.
//!
//! Rust:
//!
//! ```ignore
//! #[repr(C)]
//! pub struct Person {
//!     age: i32,
//!     height: f64,
//!     weight: f64,
//! }
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef struct Person {
//!         int32_t age;
//!         double height;
//!         double weight;
//! } Person;
//! // Some more boilerplate omitted.
//! ```
//!
//! ### Opaque Structs
//!
//! One common C idiom is to hide the implementation of a struct using
//! an opaque struct, which can only be used behind a pointer.
//! This is especially useful in Rust-C interfaces as it allows you
//! to use _any arbitrary Rust struct_ in C.
//!
//! To define an opaque struct you must define a public newtype which
//! is marked as `#[repr(C)]`.
//!
//! Rust:
//!
//! ```ignore
//! struct Foo<T> {
//!     bar: i32,
//!     baz: Option<T>,
//! }
//!
//! #[repr(C)]
//! pub struct MyCrate_Foo(Foo<PathBuf>);
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef struct MyCrate_Foo MyCrate_Foo;
//! // Some boilerplate omitted.
//! ```
//!
//! Note that the newtype _must not_ be generic but the type that
//! it wraps can be arbitrary.
//!
//! ## Functions
//!
//! For moz-cheddar to pick up on a function declaration it must be public,
//! marked `#[no_mangle]` and have one of the following ABIs:
//!
//! - C
//! - Cdecl
//! - Stdcall
//! - Fastcall
//! - System
//!
//! If you believe one of these has been included in error, or if one
//! has been omitted, then please open an issue at the [repo].
//!
//! moz-cheddar will fail on functions which are marked as diverging (`-> !`).
//!
//! Rust:
//!
//! ```ignore
//! use std::ops::Add;
//!
//! #[no_mangle]
//! pub extern fn hello() {
//!     println!("Hello!");
//! }
//!
//! fn add<O, R, L: Add<R, Output=O>>(l: L, r: R) -> O {
//!     l + r
//! }
//!
//! #[no_mangle]
//! #[allow(non_snake_case)]
//! pub extern fn MyAdd_add_u8(l: u8, r: u8) -> u8 {
//!     add(l, r)
//! }
//!
//! #[no_mangle]
//! #[allow(non_snake_case)]
//! pub extern fn MyAdd_add_u16(l: u16, r: u16) -> u16 {
//!     add(l, r)
//! }
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! void hello();
//!
//! uint8_t MyAdd_add_u8(uint8_t l, uint8_t r);
//!
//! uint16_t MyAdd_add_u16(uint16_t l, uint16_t r);
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Paths
//!
//! You must not put types defined in other modules in an exported
//! type signature without hiding it behind an opaque struct.
//! This is because the C compiler must know the layout of the type
//! and moz-cheddar can not yet search other modules.
//!
//! The very important exception to this rule are the C ABI types defined in
//! the `libc` crate and `std::os::raw`. Types from these two modules _must_
//! be fully qualified (e.g. `libc::c_void` or `std::os::raw::c_longlong`)
//! so that they can be converted properly. Importing them with a `use`
//! statement will not work.
//!
//! [the cargo docs]: http://doc.crates.io/build-script.html
//! [repo]: https://github.com/mozilla/moz-cheddar

#![cfg_attr(not(feature = "with-syntex"), feature(rustc_private))]

#[cfg(feature = "with-syntex")]
extern crate syntex_errors as errors;
#[cfg(not(feature = "with-syntex"))]
extern crate rustc_errors as errors;
#[cfg(feature = "with-syntex")]
extern crate syntex_syntax as syntax;
#[cfg(not(feature = "with-syntex"))]
extern crate syntax;
extern crate inflector;
extern crate toml;

use std::convert;
use std::fmt::Display;
use std::fs;
use std::io::{Read, Write};
use std::io::Error as IoError;
use std::path::{self, Path};
use std::collections::HashMap;
use common::{Outputs, Lang};
pub use lang_java::LangJava;
pub use errors::Level;

/// Unwraps Result<Option<..>> if it is Ok(Some(..)) else returns.
macro_rules! try_some {
    ($expr:expr) => {{ match $expr {
        Ok(Some(val)) => val,
        expr => return expr,
    }}};
}

mod common;
// mod lang_c;
mod lang_java;
mod parse;

/// Describes an error encountered by the compiler.
///
/// These can be printed nicely using the `Cheddar::print_err` method.
#[derive(Debug)]
pub struct Error {
    pub level: Level,
    span: Option<syntax::codemap::Span>,
    pub message: String,
}

impl Display for Error {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}: {}", self.level, self.message)
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self.level {
            Level::Bug => "internal error",
            Level::Fatal | Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
            _ => unreachable!(),
        }
    }
}

impl From<IoError> for Error {
    fn from(e: IoError) -> Self {
        Error {
            level: Level::Fatal,
            span: None,
            message: format!("I/O Error: {}", e),
        }
    }
}

impl Error {
    /// Use a ParseSess to print the error in the correct format.
    #[allow(unused_must_use)]
    fn print(&self, sess: &syntax::parse::ParseSess) {
        // TODO: there must be some way to reduce the amount of code here.
        // Throw away the results (with { ...; }) since they are handled elsewhere.
        if let Some(span) = self.span {
            match self.level {
                Level::Bug => {
                    sess.span_diagnostic.span_bug(span, &self.message);
                }
                Level::Fatal => {
                    sess.span_diagnostic.span_fatal(span, &self.message);
                }
                Level::Error => {
                    sess.span_diagnostic.span_err(span, &self.message);
                }
                Level::Warning => {
                    sess.span_diagnostic.span_warn(span, &self.message);
                }
                Level::Note => {
                    sess.span_diagnostic.span_note_without_error(
                        span,
                        &self.message,
                    );
                }
                Level::Help => {
                    sess.span_diagnostic.struct_dummy().span_help(
                        span,
                        &self.message,
                    );
                }
                _ => unreachable!(),
            };
        } else {
            match self.level {
                Level::Bug => {
                    sess.span_diagnostic.bug(&self.message);
                }
                Level::Fatal => {
                    sess.span_diagnostic.fatal(&self.message);
                }
                Level::Error => {
                    sess.span_diagnostic.err(&self.message);
                }
                Level::Warning => {
                    sess.span_diagnostic.warn(&self.message);
                }
                Level::Note => {
                    sess.span_diagnostic.note_without_error(&self.message);
                }
                Level::Help => {
                    sess.span_diagnostic.struct_dummy().help(&self.message);
                }
                _ => unreachable!(),
            };
        }
    }
}

/// Stores configuration for the Cheddar compiler.
///
/// # Examples
///
/// Since construction can only fail if there is an error _while_ reading the cargo manifest it is
/// usually safe to call `.unwrap()` on the result (though `.expect()` is considered better
/// practice).
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest");
/// ```
///
/// If your project is a valid cargo project or follows the same structure, you can simply place
/// the following in your build script.
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest")
///     .run_build("path/to/output/file");
/// ```
///
/// If you use a different structure you should use `.source_file("...")` to set the path to the
/// root crate file.
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest")
///     .source_file("src/root.rs")
///     .run_build("include/my_header.h");
/// ```
pub struct Cheddar {
    /// The root source file of the crate.
    input: path::PathBuf,
    /// Custom C code which is placed after the `#include`s.
    custom_code: String,
    /// The current parser session.
    ///
    /// Used for printing errors.
    session: syntax::parse::ParseSess,
}

impl Cheddar {
    /// Create a new Cheddar compiler.
    ///
    /// This can only fail if there are issues reading the cargo manifest. If there is no cargo
    /// manifest available then the source file defaults to `src/lib.rs`.
    pub fn new() -> std::result::Result<Cheddar, Error> {
        let source_path = try!(source_file_from_cargo());
        let input = path::PathBuf::from(source_path);

        Ok(Cheddar {
            input: input,
            custom_code: String::new(),
            session: syntax::parse::ParseSess::new(),
        })
    }

    /// Set the path to the root source file of the crate.
    ///
    /// This should only be used when not using a `cargo` build system.
    pub fn source_file<T>(&mut self, path: T) -> &mut Cheddar
    where
        path::PathBuf: convert::From<T>,
    {
        self.input = path::PathBuf::from(path);
        self
    }

    /// Insert custom code before the declarations which are parsed from the Rust source.
    ///
    /// If you compile a full header file, this is inserted after the `#include`s.
    ///
    /// This can be called multiple times, each time appending more code.
    pub fn insert_code(&mut self, code: &str) -> &mut Cheddar {
        self.custom_code.push_str(code);
        self
    }

    fn compile_top_level_mod(&self) -> Result<Vec<Vec<String>>, Vec<Error>> {
        let sess = &self.session;
        let krate = syntax::parse::parse_crate_from_file(&self.input, sess).unwrap();
        let mods = parse::imported_mods(&krate.module);

        if mods.is_empty() {
            return Err(vec![
                Error {
                    level: Level::Fatal,
                    span: None,
                    message: "no public-level FFI modules available".to_owned(),
                },
            ]);
        }

        Ok(mods)
    }

    /// Compile just the code into header declarations.
    ///
    /// This does not add any include-guards, includes, or extern declarations. It is mainly
    /// intended for internal use, but may be of interest to people who wish to embed
    /// moz-cheddar's generated code in another file.
    pub fn compile<L: Lang>(&self) -> Result<Outputs, Vec<Error>> {
        let base_path = self.input.parent().unwrap();
        let mods = self.compile_top_level_mod()?;
        let mut outputs = HashMap::new();

        for module in mods {
            let mut mod_path = base_path.join(&format!(
                "{}.rs",
                module.join(&path::MAIN_SEPARATOR.to_string())
            ));

            if !mod_path.exists() {
                mod_path = base_path.join(&format!(
                    "{}/mod.rs",
                    module.join(&path::MAIN_SEPARATOR.to_string())
                ));
            }

            println!("Parsing {:?}", mod_path);

            let krate = syntax::parse::parse_crate_from_file(&mod_path, &self.session).unwrap();
            parse::parse_mod::<L>(&krate.module, &mut outputs)?;

            // TODO: insert custom_code to each module?
            // .map(|source| format!("{}\n\n{}", self.custom_code, source))
        }

        Ok(outputs)
    }

    fn write_outputs<P: AsRef<Path>>(&self, root: P, outputs: &Outputs) -> Result<(), IoError> {
        let root = root.as_ref();

        for (path, contents) in outputs {
            let full_path = root.join(path);

            if let Some(parent_dirs) = full_path.parent() {
                fs::create_dir_all(parent_dirs)?;
            }

            let mut f = fs::File::create(full_path)?;
            f.write_all(contents.as_bytes())?;
            f.sync_all()?;
        }

        Ok(())
    }

    /// Write the header to a file, panicking on error.
    ///
    /// This is a convenience method for use in build scripts. If errors occur during compilation
    /// they will be printed then the function will panic.
    ///
    /// # Panics
    ///
    /// Panics on any compilation error so that the build script exits and prints output.
    pub fn run_build<P: AsRef<path::Path>, L: Lang>(&self, output_dir: P) {
        match self.compile::<L>() {
            Err(errors) => {
                for error in &errors {
                    self.print_error(error);
                }
                panic!("errors compiling header file");
            }
            Ok(outputs) => {
                if let Err(err) = self.write_outputs(output_dir, &outputs) {
                    self.print_error(&From::from(err));
                    panic!("errors writing output");
                }
            }
        }
    }

    /// Print an error using the ParseSess stored in Cheddar.
    pub fn print_error(&self, error: &Error) {
        error.print(&self.session);
    }
}

/// Extract the path to the root source file from a `Cargo.toml`.
fn source_file_from_cargo() -> std::result::Result<String, Error> {
    let cargo_toml = path::Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap_or(
        std::ffi::OsString::from(""),
    )).join("Cargo.toml");

    // If no `Cargo.toml` assume `src/lib.rs` until told otherwise.
    let default = "src/lib.rs";
    let mut cargo_toml = match std::fs::File::open(&cargo_toml) {
        Ok(value) => value,
        Err(..) => return Ok(default.to_owned()),
    };

    let mut buf = String::new();
    match cargo_toml.read_to_string(&mut buf) {
        Ok(..) => {}
        Err(..) => {
            return Err(Error {
                level: Level::Fatal,
                span: None,
                message: "could not read cargo manifest".into(),
            })
        }
    };

    let table = match (&buf).parse::<toml::Value>() {
        Ok(value) => value,
        Err(..) => {
            return Err(Error {
                level: Level::Fatal,
                span: None,
                message: "could not parse cargo manifest".into(),
            })
        }
    };

    // If not explicitly stated then defaults to `src/lib.rs`.
    Ok(
        table
            .get("lib")
            .and_then(|t| t.get("path"))
            .and_then(|s| s.as_str())
            .unwrap_or(default)
            .into(),
    )
}
