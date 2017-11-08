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
            clap::Arg::with_name("LANG")
                .short("-l")
                .long("--lang")
                .takes_value(true)
                .help("target language: java, c"),
        )
        .arg(clap::Arg::with_name("OUTPUT").index(1).help(
            "set the output directory",
        ))
        .get_matches();

    let mut bindgen = Cheddar::new().expect("cargo manifest could not be read");

    if let Some(file) = matches.value_of("FILE") {
        bindgen.source_file(&file);
    }

    let output_dir = if let Some(output) = matches.value_of("OUTPUT") {
        output
    } else {
        "bind-gen"
    };

    bindgen.run_build::<&str, LangJava>(&output_dir);
}
