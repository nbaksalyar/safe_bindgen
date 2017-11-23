extern crate safe_bindgen;
#[macro_use]
extern crate clap;

use safe_bindgen::{Cheddar, LangCSharp};
use std::path::PathBuf;

fn main() {
    let matches = clap::App::new("bindgen")
        .version(crate_version!())
        .arg(
            clap::Arg::with_name("INPUT")
                .short("-i")
                .long("--input")
                .takes_value(true)
                .required(true)
                .help("source directory"),
        )
        .arg(
            clap::Arg::with_name("OUTPUT")
                .short("-o")
                .long("--output")
                .takes_value(true)
                .required(true)
                .help("output directory"),
        )
        .get_matches();



    let source_dir = matches.value_of("INPUT").unwrap();
    let output_dir = matches.value_of("OUTPUT").unwrap();

    let mut lang = LangCSharp::new();
    lang.set_lib_name("ffi_utils");
    build(&mut lang, "ffi_utils", source_dir, output_dir);

    lang.set_lib_name("safe_core");
    // NOTE: the values should be taken directly from the rust_sodium crate
    lang.add_custom_decl("public const int ASYM_PUBLIC_KEY_LEN = 32;");
    lang.add_custom_decl("public const int ASYM_SECRET_KEY_LEN = 32;");
    lang.add_custom_decl("public const int ASYM_NONCE_LEN = 24;");
    lang.add_custom_decl("public const int SYM_KEY_LEN = 32;");
    lang.add_custom_decl("public const int SYM_NONCE_LEN = 24;");
    lang.add_custom_decl("public const int SIGN_PUBLIC_KEY_LEN = 32;");
    lang.add_custom_decl("public const int SIGN_SECRET_KEY_LEN = 64;");
    lang.add_custom_decl("public const int XOR_NAME_LEN = 32;");
    build(&mut lang, "safe_core", source_dir, output_dir);

    lang.set_lib_name("safe_app");
    lang.add_opaque_type("App");
    build(&mut lang, "safe_app", source_dir, output_dir);

    lang.set_lib_name("safe_authenticator");
    lang.add_opaque_type("Authenticator");
    build(&mut lang, "safe_authenticator", source_dir, output_dir);
}

fn build(lang: &mut LangCSharp, project: &str, source_dir: &str, output_dir: &str) {
    let source_file = PathBuf::from(source_dir).join(project).join("src/lib.rs");

    let mut bindgen = Cheddar::new().unwrap();
    bindgen.source_file(source_file.to_str().unwrap());
    bindgen.run_build(lang, &output_dir);
}
