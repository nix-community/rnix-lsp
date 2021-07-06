#![allow(clippy::unwrap_used)]

use eval::Tree;
use gc::{Gc, GcCell};
use rnix::types::Wrapper;
use scope::Scope;
use std::borrow::Borrow;
use std::collections::HashMap;
use value::NixValue;

#[allow(dead_code)]
fn eval(code: &str) -> NixValue {
    let ast = rnix::parse(&code);
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let out = Tree::parse_legacy(
        &root,
        Gc::new(Scope::Root(
            path,
            Some(Gc::new(GcCell::new(HashMap::new()))),
        )),
    )
    .unwrap();
    let tmp = out.eval();
    let val: &NixValue = tmp.as_ref().unwrap().borrow();
    val.clone()
}

use super::*;

#[test]
fn basic_math() {
    let code = "let abc = 10; in 1 + abc";
    assert_eq!(eval(code).as_int().unwrap(), 11);
}

#[test]
fn ptr_equality() {
    let code = "let a = { f = (x: x); }; b = a; in a == b";
    assert_eq!(eval(code).as_bool().unwrap(), true);
}

#[test]
fn shadowing() {
    let code = "let import = 1; in import";
    assert_eq!(eval(code).as_int().unwrap(), 1);
    let code = "let fun = { import }: import; in fun { import = 3; }";
    assert_eq!(eval(code).as_int().unwrap(), 3);
    let code = "builtins.elemAt ({ inherit (builtins) map; }.map (x: x*2) [1]) 0";
    assert_eq!(eval(code).as_int().unwrap(), 2);
}

#[test]
fn fixed_point_combinator() {
    let code = "((f: let x = f x; in x) (self: { x = 1; y = self.x; })).y";
    assert_eq!(eval(code).as_int().unwrap(), 1);
}

#[test]
fn infinite_recursion() {
    let code = "let x = x; in 1";
    assert_eq!(eval(code).as_int().unwrap(), 1);
}

#[test]
fn rec_inherit() {
    let code = "let x = 1; y = rec { inherit x; }; in y.x";
    assert_eq!(eval(code).as_int().unwrap(), 1);
}
