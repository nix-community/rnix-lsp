use std::borrow::Borrow;

use rnix::TextRange;

use crate::{
    error::{EvalError, Located, ValueError},
    eval::{Expr, ExprSource, StringPartSource}, scope::Definition,
};

/// If this node is an identifier, should it be a variable name ?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Ident {
    /// If this node is an identifier, it should be a variable name
    IsVariable,
    /// If this not is an identifier, it is not a variable name
    ///
    /// Example: foo in `bar.foo`
    IsNotVariable,
}

/// If this is an error, maybe report it to acc. Otherwise, visit all children of this node
/// to find evaluation errors and collect diagnostics on this accumulator
fn visit_result<T: Borrow<Expr>>(
    acc: &mut Vec<Located<ValueError>>,
    result: &Result<T, EvalError>,
    range: &Option<TextRange>,
    ident_ctx: Ident,
) {
    match result {
        Err(EvalError::Value(err)) => {
            if let Some(range) = range {
                acc.push(Located {
                    range: range.clone(),
                    kind: err.clone(),
                })
            }
        }
        Err(EvalError::Internal(_)) => {}
        Ok(v) => visit(acc, v.borrow(), ident_ctx),
    }
}

/// Visit all children of this node to find evaluation errors and collect
/// diagnostics on this accumulator
fn visit(acc: &mut Vec<Located<ValueError>>, node: &Expr, ident_ctx: Ident) {
    use Ident::*;
    match &node.source {
        ExprSource::AttrSet { definitions } => {
            for i in definitions.iter() {
                visit_result(acc, i, &node.range, IsVariable)
            }
        }
        ExprSource::LetIn { definitions, body } => {
            for i in definitions.iter() {
                visit_result(acc, i, &node.range, IsVariable)
            }
            visit_result(acc, body, &node.range, IsVariable);
        }
        ExprSource::KeyValuePair { key, value } => {
            visit_result(acc, key, &node.range, IsNotVariable);
            visit_result(acc, value, &node.range, IsVariable);
        }
        ExprSource::Select { from, index } => {
            visit_result(acc, from, &node.range, IsVariable);
            visit_result(acc, index, &node.range, IsNotVariable);
        }
        ExprSource::Lambda { arg, body } => {
            visit_result(acc, body, &node.range, IsVariable);
            visit_result(acc, arg, &node.range, IsNotVariable);
        }
        ExprSource::OrDefault { index, default } => {
            visit_result(acc, index, &node.range, IsVariable);
            visit_result(acc, default, &node.range, IsVariable);
        }
        ExprSource::With { inner, body } => {
            visit_result(acc, body, &node.range, IsVariable);
            visit_result(acc, inner, &node.range, IsVariable);
        }
        ExprSource::Pattern { entries, .. } => {
            for i in entries.values() {
                if let Some(default) = i {
                    visit_result(acc, default, &node.range, IsVariable);
                }
            }
        },
        ExprSource::String { parts } => {
            for i in parts.iter() {
                if let StringPartSource::Expression(expr) = i {
                    visit_result(acc, expr, &node.range, IsVariable);
                }
            }
        },
        ExprSource::List { elements } => {
            for i in elements.iter() {
                visit_result(acc, i, &node.range, IsVariable);
            }
        },
        ExprSource::Ident { name } => {
            if ident_ctx == IsVariable {
                if let Some(range) = &node.range {
                    // check that the variable is bound
                    if matches!(node.scope.get_definition(name), Definition::Unbound) {
                        acc.push(Located {
                            range: range.clone(),
                            kind: ValueError::UnboundIdentifier(name.clone()),
                        });
                    }
                }
            }
        }
        ExprSource::Literal { .. } => {}
        ExprSource::UnaryInvert { value: inner }
        | ExprSource::UnaryNegate { value: inner }
        | ExprSource::Dynamic { inner }
        | ExprSource::Paren { inner } => visit_result(acc, inner, &node.range, IsVariable),
        ExprSource::BinOp { left, right, .. }
        | ExprSource::BoolAnd { left, right }
        | ExprSource::Apply { function: left, arg: right }
        | ExprSource::Implication { left, right }
        | ExprSource::BoolOr { left, right } => {
            visit_result(acc, left, &node.range, IsVariable);
            visit_result(acc, right, &node.range, IsVariable);
        }
    }
}
/// Looks for errors in the user code. Attempts to return no false positives.
///
/// Current analysis can detect undefined variables.
pub fn check(node: &Expr) -> Vec<Located<ValueError>> {
    let mut res = Vec::new();
    visit(&mut res, node, Ident::IsVariable);
    res
}
