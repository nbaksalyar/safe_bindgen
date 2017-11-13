extern crate safe_bindgen;
#[macro_use]
extern crate clap;

use safe_bindgen::{Cheddar, LangJava, LangCSharp};
use std::collections::HashMap;

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
                .required(true)
                .help("target language")
                .possible_values(&["csharp", "java"]),
        )
        .arg(
            clap::Arg::with_name("LIB")
                .long("--lib")
                .takes_value(true)
                .required(true)
                .help("name of the native library to link"),
        )
        .arg(clap::Arg::with_name("OUTPUT").index(1).help(
            "set the output directory",
        ))
        .get_matches();

    let mut bindgen = Cheddar::new().expect("cargo manifest could not be read");
    let lang = matches.value_of("LANG").unwrap();
    let lib = matches.value_of("LIB").unwrap();

    if let Some(file) = matches.value_of("FILE") {
        bindgen.source_file(&file);
    }

    let output_dir = if let Some(output) = matches.value_of("OUTPUT") {
        output.to_string()
    } else {
        format!("bind-gen/{}", lang)
    };

    match lang {
        "csharp" => {
            let mut lang = LangCSharp::new(lib);
            bindgen.run_build(&mut lang, &output_dir)
        }
        "java" => {
	    let mut type_map = HashMap::new();
	    type_map.insert("XorNameArray", "byte[]");
	    type_map.insert("SignSecretKey", "byte[]");
	    type_map.insert("SignPublicKey", "byte[]");
	    type_map.insert("SymSecretKey", "byte[]");
	    type_map.insert("SymNonce", "byte[]");
	    type_map.insert("AsymPublicKey", "byte[]");
	    type_map.insert("AsymSecretKey", "byte[]");
	    type_map.insert("AsymNonce", "byte[]");
	    type_map.insert("CipherOptHandle", "long");
	    type_map.insert("EncryptPubKeyHandle", "long");
	    type_map.insert("EncryptSecKeyHandle", "long");
	    type_map.insert("MDataEntriesHandle", "long");
	    type_map.insert("MDataEntryActionsHandle", "long");
	    type_map.insert("MDataPermissionsHandle", "long");
	    type_map.insert("SelfEncryptorReaderHandle", "long");
	    type_map.insert("SelfEncryptorWriterHandle", "long");
	    type_map.insert("SignPubKeyHandle", "long");
	    type_map.insert("SignSecKeyHandle", "long");
	    type_map.insert("FileContextHandle", "long");

	    let mut java = LangJava::new(type_map);
	    bindgen.run_build(&mut java, &output_dir);
        }
        _ => unreachable!(),
    }
}

