use colored::*;
use common::{Lang, Outputs};
use diff;
use parse;
use std::collections::HashMap;
use std::fmt::Write;
use syntax;
use syntax::codemap::FilePathMapping;
use Error;

macro_rules! compile {
    ($lang:expr, $rust:tt) => {
        try_compile!($lang, $rust).unwrap()
    };
}

macro_rules! try_compile {
    ($lang:expr, $rust:tt) => {{
        let rust_src = stringify!($rust);
        let rust_src = rust_src[1..rust_src.len() - 1].to_string();
        use $crate::test_utils;
        test_utils::try_compile($lang, rust_src)
    }};
}

// This is like `assert_eq`, but produces more readable output for multiline
// strings.
macro_rules! assert_multiline_eq {
    ($left:expr, $right:expr) => {{
        use $crate::colored::*;
        use $crate::test_utils::format_diff;

        let left = $left;
        let right = $right;

        if left != right {
            panic!(
                "assertion failed: `({} == {})`\n```\n{}```\n",
                "left".red(),
                "right".green(),
                format_diff(&left, &right)
            );
        }
    }};
}

pub fn try_compile(
    mut lang: impl Lang,
    rust_src: String,
) -> Result<HashMap<String, String>, Vec<Error>> {
    let session = syntax::parse::ParseSess::new(FilePathMapping::empty());
    let ast = syntax::parse::parse_crate_from_source_str("lib.rs".to_string(), rust_src, &session)
        .unwrap();

    let mut outputs = Outputs::default();

    parse::parse_mod(&mut lang, &ast.module, &[Default::default()], &mut outputs)?;
    lang.finalise_output(&mut outputs)?;

    Ok(outputs)
}

pub fn format_diff(left: &str, right: &str) -> String {
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
