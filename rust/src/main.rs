use std::env;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

#[derive(Debug)]
enum Expr {
    Float(f64),
    Symbol(String),
    Call(Vec<Expr>),
}

fn loadfile(path: &String) -> String {
    let file = File::open(path).unwrap();
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents).unwrap();
    return contents;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Specify a file");
        return;
    }
    let path = &args[1];
    let code = loadfile(path);
    println!("{}", code);
    let ex = Expr::Call(
        vec![Expr::Symbol(String::from("+")),
             Expr::Float(5.5),
             Expr::Float(1.1)]);
    println!("{:?}", ex);
}
