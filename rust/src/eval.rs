use crate::parse::Expr;
use Value::*;

#[derive(Debug)]
pub enum Value {
    Float(f64),
}

pub fn eval(ex: Expr) -> Value {
    match ex {
        Expr::Float(x) => Float(x),
        Expr::Symbol(x) => panic!("Can't evaluate symbol {:?}", x),
        Expr::List(x) => panic!("Can't evalute list {:?}", x)
    }
}
