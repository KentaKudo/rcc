use std::fmt;

pub mod codegen;
pub mod parse;
pub mod utils;

#[derive(Debug, PartialEq, Default, Copy, Clone)]
struct Location(u64);

// TODO: rename
#[derive(Debug)]
pub struct CustomError(String, Location);

impl fmt::Display for CustomError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CustomError(msg, loc) = self;
        writeln!(f, "{}^ {}", " ".repeat(loc.0 as usize), msg)
    }
}

pub type Result<T> = std::result::Result<T, CustomError>;
