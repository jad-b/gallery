//! # Rust File I/O

use std::fs;

// Build a Vector of filenames within a directory, akin to Unix's `ls`.
fn ls(dirpath: &str) -> Vec<String> {
    let v: Vec<fs::DirEntry> = Vec::new();
    for entry in fs::read_dir(dirpath) {
       let s = Ok(entry.path().into_os_string().into_string());
       v.push(s);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        for file in list_files("../../testdata/") {
           //
        }
    }
}

