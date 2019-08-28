use std::fs::File;
use std::io::{BufReader,SeekFrom};
use std::io::prelude::*;
use Expr::*;

#[derive(Debug)]
pub enum Expr {
    Float(f64),
    Symbol(String),
    List(Vec<Expr>),
}

// TODO: unicode
fn read<T: Read>(buf: &mut T) -> char {
    let mut byte = 0;
    buf.read(std::slice::from_mut(&mut byte)).unwrap();
    let char = char::from(byte);
    return char;
}

fn peekf<T: Read + Seek, O, F: Fn(&mut T)->O>(buf: &mut T, f: F) -> O {
    let pos = buf.seek(SeekFrom::Current(0)).unwrap();
    let result = f(buf);
    buf.seek(SeekFrom::Start(pos)).unwrap();
    return result;
}

fn peek<T: Read + Seek>(buf: &mut T) -> char {
    peekf(buf, &read)
}

fn number<T: Read + Seek>(buf: &mut T) -> Option<Expr> {
    let mut s = String::new();
    loop {
        let ch = read(buf);
        if ('0'..='9').contains(&ch) {
            s.push(ch);
        } else {
            if ch != '\u{0}' { buf.seek(SeekFrom::Current(-1)).unwrap(); };
            break;
        }
    }
    if s.is_empty() { return None; }
    return Some(Float(s.parse::<f64>().unwrap()));
}

fn parse<T: Read + Seek>(buf: &mut T) -> Option<Expr> {
    return number(buf);
}

fn loadfile(path: &str) -> String {
    let file = File::open(path).unwrap();
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents).unwrap();
    return contents;
}

pub fn parsefile(path: &str) -> Expr {
    let file = File::open(path).unwrap();
    let mut buf = BufReader::new(file);
    let code = parse(&mut buf);
    return code.unwrap();
    // List(vec![Symbol(String::from("+")),
    //           Float(5.5),
    //           Float(1.1)])
}
