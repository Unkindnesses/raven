mod parse;

use std::env;
use parse::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Specify a file");
        return;
    }
    let path = &args[1];
    let ex = parsefile(path);
    println!("{:?}", ex);
}
