extern crate safe_bindgen;
#[macro_use]
extern crate clap;

use safe_bindgen::{Cheddar, LangJava};

fn main() {
    let matches = clap::App::new("bindgen")
        .version(crate_version!())
        .author(
            "Sean Marshallsay <srm.1708@gmail.com>, MaidSafe Developers <dev@maidsafe.net>",
        )
        .about("create binding files using a Rust source file")
        .arg(
            clap::Arg::with_name("FILE")
                .short("-f")
                .long("--file")
                .conflicts_with("STRING")
                .takes_value(true)
                .help("the root source file"),
        )
        .arg(
            clap::Arg::with_name("STRING")
                .short("-s")
                .long("--string")
                .conflicts_with("FILE")
                .takes_value(true)
                .help("use a string as the source code"),
        )
        .arg(
            clap::Arg::with_name("LANG")
                .short("-l")
                .long("--lang")
                .takes_value(true)
                .help("target language: java, c"),
        )
        .arg(
            clap::Arg::with_name("MODULE")
                .short("-m")
                .long("--module")
                .takes_value(true)
                .help("the module containing the C API"),
        )
        .arg(clap::Arg::with_name("OUTPUT").index(1).help(
            "set the output directory",
        ))
        .get_matches();

    let mut bindgen = Cheddar::new().expect("cargo manifest could not be read");

    if let Some(file) = matches.value_of("FILE") {
        bindgen.source_file(&file);
    } else if let Some(string) = matches.value_of("STRING") {
        bindgen.source_string(&string);
    }

    if let Some(module) = matches.value_of("MODULE") {
        if let Err(errs) = bindgen.module(&module) {
            for err in errs {
                bindgen.print_error(&err);
            }

            panic!("errors setting module");
        }
    }

    if let Some(output) = matches.value_of("OUTPUT") {
        bindgen.run_build::<&str, LangJava>(&output);
    } else {
        bindgen.run_build::<&str, LangJava>("bind-gen");
    };
}
