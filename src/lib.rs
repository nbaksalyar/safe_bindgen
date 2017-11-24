//! safe_bindgen is a library based on moz-cheddar for converting Rust source files into Java and C# bindings.
//! It is built specifically for the SAFE Client Libs project.

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
#[macro_use]
extern crate quote;
extern crate jni;

#[cfg(test)]
extern crate colored;
#[cfg(test)]
extern crate diff;
#[cfg(test)]
#[macro_use]
extern crate indoc;

use common::{Lang, Outputs};
pub use csharp::LangCSharp;
pub use errors::Level;
pub use java::LangJava;
use std::collections::HashMap;
use std::convert;
use std::fmt::Display;
use std::fs;
use std::io::{Read, Write};
use std::io::Error as IoError;
use std::path::{self, Path};

/// Unwraps Result<Option<..>> if it is Ok(Some(..)) else returns.
macro_rules! try_some {
    ($expr:expr) => {{ match $expr {
        Ok(Some(val)) => val,
        expr => return expr,
    }}};
}

mod common;
// mod lang_c;
mod csharp;
mod java;
mod output;
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

impl From<Error> for Vec<Error> {
    fn from(e: Error) -> Self {
        vec![e]
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

/// Stores configuration for the bindgen.
///
/// # Examples
///
/// Since construction can only fail if there is an error _while_ reading the cargo manifest it is
/// usually safe to call `.unwrap()` on the result (though `.expect()` is considered better
/// practice).
///
/// ```ignore
/// Bindgen::new().expect("unable to read cargo manifest");
/// ```
///
/// If your project is a valid cargo project or follows the same structure, you can simply place
/// the following in your build script.
///
/// ```ignore
/// Bindgen::new().expect("unable to read cargo manifest")
///     .run_build("path/to/output/file");
/// ```
///
/// If you use a different structure you should use `.source_file("...")` to set the path to the
/// root crate file.
///
/// ```ignore
/// Bindgen::new().expect("unable to read cargo manifest")
///     .source_file("src/root.rs")
///     .run_build("include/my_header.h");
/// ```
pub struct Bindgen {
    /// The root source file of the crate.
    input: path::PathBuf,
    /// Custom C code which is placed after the `#include`s.
    custom_code: String,
    /// The current parser session.
    ///
    /// Used for printing errors.
    session: syntax::parse::ParseSess,
}

impl Bindgen {
    /// Create a new bindgen instance.
    ///
    /// This can only fail if there are issues reading the cargo manifest. If there is no cargo
    /// manifest available then the source file defaults to `src/lib.rs`.
    pub fn new() -> std::result::Result<Self, Error> {
        let source_path = source_file_from_cargo()?;
        let input = path::PathBuf::from(source_path);

        Ok(Bindgen {
            input: input,
            custom_code: String::new(),
            session: syntax::parse::ParseSess::new(),
        })
    }

    /// Set the path to the root source file of the crate.
    ///
    /// This should only be used when not using a `cargo` build system.
    pub fn source_file<T>(&mut self, path: T) -> &mut Self
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
    pub fn insert_code(&mut self, code: &str) -> &mut Self {
        self.custom_code.push_str(code);
        self
    }

    /// Compile just the code into header declarations.
    ///
    /// This does not add any include-guards, includes, or extern declarations. It is mainly
    /// intended for internal use, but may be of interest to people who wish to embed
    /// moz-cheddar's generated code in another file.
    pub fn compile<L: Lang>(&self, lang: &mut L, finalise: bool) -> Result<Outputs, Vec<Error>> {
        let base_path = self.input.parent().unwrap();
        let mut outputs = HashMap::new();

        // Parse the top level mod.
        let krate = syntax::parse::parse_crate_from_file(&self.input, &self.session).unwrap();
        parse::parse_mod(lang, &krate.module, &mut outputs)?;

        // Parse other mods.
        let modules = parse::imported_mods(&krate.module);
        for module in modules {
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

            eprintln!("Parsing {:?}", mod_path);

            let krate = syntax::parse::parse_crate_from_file(&mod_path, &self.session).unwrap();
            parse::parse_mod(lang, &krate.module, &mut outputs)?;

            // TODO: insert custom_code to each module?
            // .map(|source| format!("{}\n\n{}", self.custom_code, source))
        }

        if finalise {
            lang.finalise_output(&mut outputs)?;
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
    pub fn run_build<P: AsRef<path::Path>, L: Lang>(&self, lang: &mut L, output_dir: P) {
        match self.compile(lang, true) {
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
