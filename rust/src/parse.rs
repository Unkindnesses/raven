use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

#[derive(Debug)]
pub enum Expr {
    Float(f64),
    Symbol(String),
    Call(Vec<Expr>),
}

fn loadfile(path: &str) -> String {
    let file = File::open(path).unwrap();
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents).unwrap();
    return contents;
}

pub fn parsefile(path: &str) -> Expr {
    let code = loadfile(path);
    println!("Code: {}", code);
    Expr::Call(
        vec![Expr::Symbol(String::from("+")),
             Expr::Float(5.5),
             Expr::Float(1.1)])
}
