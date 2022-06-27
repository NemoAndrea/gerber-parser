pub mod gerber_parser;

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::fs;

pub fn test_func() {
    println!("this is just a test, move along")
}

pub fn get_templates(template_dir: &Path) -> Vec<std::path::PathBuf> {
    println!("Using templates in: {}", template_dir.display());
    let raw_template_files = fs::read_dir(template_dir).unwrap();

    let mut gerber_files:Vec<std::path::PathBuf> = Vec::new();

    for direntry in raw_template_files {
        let path = direntry.unwrap().path();
        if let Some(extension) = path.extension() {
            if extension == "gbr" {
                gerber_files.push(path);
            }
        }        
    }

    let prettyfiles:Vec<&OsStr>= gerber_files.iter().map(|x| x.file_name().unwrap()).collect();
    println!("Found {} gerber files, namely: {:?}", &gerber_files.len(), prettyfiles);

    gerber_files
}

