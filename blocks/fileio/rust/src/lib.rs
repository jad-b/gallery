#[cfg(test)]
mod tests {
    use fileio;

    #[test]
    fn cat() {
        let fp: &str = "../testdata/hi.txt";
        match fileio::cat(fp) {
            Ok(text) => assert_eq!(text, "hi"),
            Err(why) => panic!(why)
        }
    }
}

pub mod fileio {
    use std::fs::File;
    use std::io::prelude::*;
    use std::io;

    // Read the contents of a file into a &str.
    pub fn cat(filepath: &str) -> Result<String, io::Error> {
        // Open file
        let mut f = File::open(filepath)?;
        // Read file contents to a String (UTF-8 byte buffer)
        let mut buffer = String::new();
        f.read_to_string(&mut buffer)?;
        // Return a Result::Ok with the file contents
        Ok(buffer)
    }

}
