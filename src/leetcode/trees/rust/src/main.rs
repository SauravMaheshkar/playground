use clap::Parser;
use std::collections::HashMap;
use std::fs;

pub mod _100;
pub mod _104;
pub mod _110;
pub mod _226;
pub mod _543;
pub mod _572;
pub mod eval;
pub mod utils;

use crate::eval::{convert_test_cases, TestCase};

#[derive(Parser, Debug)]
struct Cli {
    data_path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();
    let impls: HashMap<i32, &str> = [
        (226, "TreeTree"),
        (104, "TreeNum"),
        (543, "TreeNum"),
        (110, "TreeBool"),
        (100, "TreeTreeBool"),
        (572, "TreeTreeBool"),
    ]
    .iter()
    .cloned()
    .collect();
    let data_path = args.data_path;

    if data_path.is_dir() {
        for entry in fs::read_dir(data_path).expect("Failed to read data directory") {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            if path.is_file() {
                if let Some(extension) = path.extension() {
                    if extension == "json" {
                        if let Some(filename) = path.file_stem() {
                            if let Ok(file_id) = filename.to_str().unwrap().parse::<i32>() {
                                if let Some(&test_type) = impls.get(&file_id) {
                                    let file_content = fs::read_to_string(&path)
                                        .expect("Failed to read JSON file");

                                    let test_case = convert_test_cases(test_type, &file_content);

                                    match file_id {
                                        226 => {
                                            if let TestCase::TreeTreeTestCase(test_cases) =
                                                test_case
                                            {
                                                _226::main(test_cases);
                                            }
                                        }
                                        100 => {
                                            if let TestCase::TreeTreeBoolTestCase(test_cases) =
                                                test_case
                                            {
                                                _100::main(test_cases);
                                            }
                                        }
                                        572 => {
                                            if let TestCase::TreeTreeBoolTestCase(test_cases) =
                                                test_case
                                            {
                                                _572::main(test_cases);
                                            }
                                        }
                                        104 => {
                                            if let TestCase::TreeNumTestCase(test_cases) = test_case
                                            {
                                                _104::main(test_cases);
                                            }
                                        }
                                        110 => {
                                            if let TestCase::TreeBoolTestCase(test_cases) =
                                                test_case
                                            {
                                                _110::main(test_cases);
                                            }
                                        }
                                        543 => {
                                            if let TestCase::TreeNumTestCase(test_cases) = test_case
                                            {
                                                _543::main(test_cases);
                                            }
                                        }
                                        _ => {
                                            println!("No module found for problem id: {}", file_id)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        println!("The provided path is not a directory");
    }
}
