use eval::Expr;
use gc::Gc;
use rnix::types::Wrapper;
use scope::Scope;
use std::borrow::Borrow;
use value::NixValue;

#[allow(dead_code)]
fn eval(code: &str) -> NixValue {
    let ast = rnix::parse(&code);
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let out = Expr::parse(root, Gc::new(Scope::Root(path))).unwrap();
    let tmp = out.eval();
    let val: &NixValue = tmp.as_ref().unwrap().borrow();
    val.clone()
}

use super::*;

#[test]
fn integer_division() {
    let code = "1 / 2";
    assert_eq!(eval(code).as_int().unwrap(), 0);
}

#[test]
fn float_division() {
    let code = "1.0 / 2.0";
    assert_eq!(eval(code).as_float().unwrap(), 0.5);
}

#[test]
fn order_of_operations() {
    let code = "1 + 2 * 3";
    assert_eq!(eval(code).as_int().unwrap(), 7);
}

#[test]
fn div_int_by_float() {
    let code = "1 / 2.0";
    assert_eq!(eval(code).as_float().unwrap(), 0.5);
}
