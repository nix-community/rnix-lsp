use crate::parse::BinOpKind;
use crate::scope::*;
use crate::value::*;
use crate::EvalError;
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::types::Dynamic;
use rnix::{
    types::{Pattern, Str, TokenWrapper, TypedNode, Wrapper},
    StrPart, SyntaxNode, TextRange,
};
use std::str::FromStr;
use std::{borrow::Borrow, collections::HashMap, path::PathBuf};

pub fn expand_path(base: NixPathAnchor, path: String) -> Option<PathBuf> {
    match &base {
        NixPathAnchor::Absolute => PathBuf::from_str(&path).ok(),
        NixPathAnchor::Relative(rel) => Some(rel.join(path)),
    }
}

pub fn eval_lambda(lambda: &NixLambda, param: Gc<Tree>) -> Result<Gc<NixValue>, EvalError> {
    match &lambda {
        NixLambda::Builtin(builtin) => builtin.call(param),
        NixLambda::Node { lambda, scope } => {
            let arg = lambda.arg().ok_or(EvalError::Parsing)?;
            use rnix::SyntaxKind::*;
            match arg.kind() {
                NODE_PATTERN => {
                    let new_scope_gc = {
                        let new = Scope::Normal {
                            parent: scope.clone(),
                            contents: GcCell::new(HashMap::new()),
                        };
                        Gc::new(new)
                    };

                    let mut map = HashMap::new();

                    let pattern = Pattern::cast(arg).ok_or(EvalError::Parsing)?;
                    if let Some(at) = pattern.at() {
                        map.insert(at.as_str().to_string(), param.clone());
                    }

                    if pattern.entries().next().is_some() {
                        let tmp = param.eval()?;
                        let param = match tmp.borrow() {
                            NixValue::Map(x) => x,
                            x => {
                                return Err(EvalError::Unexpected(format!(
                                    "cannot destructure {:?}",
                                    x
                                )))
                            }
                        };
                        for entry in pattern.entries() {
                            let key = entry.name().ok_or(EvalError::Parsing)?;
                            let name = key.as_str();
                            if let Some(x) = param.get(name) {
                                map.insert(name.to_string(), x.clone());
                            } else if let Some(default) = entry.default() {
                                map.insert(
                                    name.to_string(),
                                    Tree::parse(default, new_scope_gc.clone())?,
                                );
                            } else {
                                return Err(EvalError::Unexpected(format!(
                                    "Lambda param missing attr: {}\n\n{}",
                                    name,
                                    lambda.arg().ok_or(EvalError::Parsing)?.text()
                                )));
                            }
                        }
                    }

                    if let Scope::Normal { contents, .. } = new_scope_gc.borrow() {
                        *contents.borrow_mut() = map;
                    }

                    Tree::parse_legacy(&lambda.body().ok_or(EvalError::Parsing)?, new_scope_gc)
                        .and_then(|x| x.eval())
                }
                NODE_IDENT => {
                    let mut map = HashMap::new();
                    let key = to_string(&arg, scope.clone())?;
                    map.insert(key, param);
                    Tree::parse_legacy(
                        &lambda.body().ok_or(EvalError::Parsing)?,
                        Gc::new(Scope::Normal {
                            parent: scope.clone(),
                            contents: GcCell::new(map),
                        }),
                    )
                    .and_then(|x| x.eval())
                }
                x => unimplemented!("UNEXPECTED {:?}", x),
            }
        }
    }
}

pub fn to_string(root: &SyntaxNode, scope: Gc<Scope>) -> Result<String, EvalError> {
    use rnix::SyntaxKind::*;
    match root.kind() {
        NODE_STRING => Ok(Str::cast(root.clone())
            .ok_or(EvalError::Parsing)?
            .parts()
            .into_iter()
            .map(|part| match part {
                StrPart::Literal(x) => x,
                StrPart::Ast(node) => {
                    match Tree::parse_legacy(&node, scope.clone())
                        .unwrap()
                        .eval()
                        .unwrap()
                        .borrow()
                    {
                        NixValue::Str(ref x) => x.clone(),
                        x => panic!("Cannot cast {:?} to string", x),
                    }
                }
            })
            .collect::<Vec<_>>()
            .join("")),
        NODE_IDENT => Ok(root.text().to_string()),
        NODE_DYNAMIC => {
            let inner = Dynamic::cast(root.clone())
                .ok_or(EvalError::Parsing)?
                .inner()
                .ok_or(EvalError::Parsing)?;
            match Tree::parse_legacy(&inner, scope)?.eval()?.borrow() {
                NixValue::Str(ref x) => Ok(x.clone()),
                _ => Err(EvalError::Parsing),
            }
        }
        x => unimplemented!("Unexpected attr key kind: {:?}\n\n{}", x, root.text()),
    }
}

type TreeResult = Result<Gc<Tree>, EvalError>;

#[derive(Debug, Clone, Trace, Finalize)]
#[allow(dead_code)]
pub enum TreeSource {
    Legacy {
        #[unsafe_ignore_trace]
        syntax: SyntaxNode,
    },
    Literal {
        value: NixValue,
    },
    Assert {
        condition: TreeResult,
        body: TreeResult,
    },
    IfElse {
        condition: TreeResult,
        true_body: TreeResult,
        false_body: TreeResult,
    },
    Paren {
        inner: TreeResult,
    },
    Dynamic {
        inner: TreeResult,
    },
    LetIn {
        inherits: Vec<TreeResult>,    // inherit (foobar) ->abc<- ->xyz<-;
        definitions: Vec<TreeResult>, // ->abc = "hello"<-;
        body: TreeResult,
    },
    LetAttr {
        path: Vec<TreeResult>,
        value: TreeResult,
    },
    Ident {
        name: String,
    },
    Select {
        from: TreeResult,
        index: TreeResult,
    },
    BinOp {
        op: BinOpKind,
        left: TreeResult,
        right: TreeResult,
    },
    BoolAnd {
        left: TreeResult,
        right: TreeResult,
    },
    BoolOr {
        left: TreeResult,
        right: TreeResult,
    },
    InSet {
        set: TreeResult,
        index: TreeResult,
    },
    Implication {
        left: TreeResult,
        right: TreeResult,
    },
    With {
        value: TreeResult,
        body: TreeResult,
    },
    List {
        items: Vec<TreeResult>,
    },
    Apply {
        lambda: TreeResult,
        arg: TreeResult,
    },
    Map {
        inherits: Vec<TreeResult>,
        definitions: Vec<TreeResult>,
    },
    MapAttr {
        path: Vec<TreeResult>,
        value: TreeResult,
    },
    StringInterpol {
        parts: Vec<TreeResult>,
    },
    UnaryInvert {
        value: TreeResult,
    },
    UnaryNegate {
        value: TreeResult,
    },
    Lambda {
        literal: Gc<NixValue>,
        params: Vec<TreeResult>,
        body: TreeResult,
    },
    InferredIdent {
        name: String,
    },
}

#[derive(Clone, Trace, Finalize)]
pub struct Tree {
    #[unsafe_ignore_trace]
    pub range: Option<TextRange>,
    pub hash: GcCell<Option<String>>,
    pub value: GcCell<Option<Gc<NixValue>>>,
    pub source: TreeSource,
    pub scope: Gc<Scope>,
}
impl std::fmt::Debug for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tree")
            .field("value", &self.value)
            .field("source", &self.source)
            .field("range", &self.range)
            .finish()
    }
}

pub fn to_hex_string(bytes: Vec<u8>) -> String {
    let strs: Vec<String> = bytes.iter().map(|b| format!("{:02x}", b)).collect();
    strs.join("")
}

impl Tree {
    pub fn hash(&self) -> Result<String, EvalError> {
        use std::ops::Deref;
        let value_borrow = self.hash.borrow();
        if let Some(ref value) = value_borrow.deref() {
            Ok(value.clone())
        } else {
            drop(value_borrow);
            let value = self.hash_uncached()?;
            *self.hash.borrow_mut() = Some(value.clone());
            Ok(value)
        }
    }

    pub fn completions(&self) -> Option<(String, Vec<String>, TextRange)> {
        match &self.source {
            TreeSource::Ident { name } => Some((name.clone(), self.scope.list(), self.range?)),
            TreeSource::Select { from, index } => Some((
                index.as_ref().ok()?.get_ident().ok()?,
                if let Ok(x) = from {
                    x.get_keys().unwrap_or_default()
                } else {
                    vec![]
                },
                index.as_ref().ok()?.range?,
            )),
            _ => Some(("".to_string(), self.scope.list(), self.range?)),
        }
    }

    pub fn get_keys(&self) -> Result<Vec<String>, EvalError> {
        Ok(self.eval()?.as_map()?.keys().cloned().collect())
    }

    fn hash_uncached(&self) -> Result<String, EvalError> {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(b"(");
        match &self.source {
            TreeSource::Legacy { syntax: _ } => {
                return Err(EvalError::Unimplemented("legacy".to_string()))
            }
            TreeSource::Literal { value } => match value {
                NixValue::Bool(x) => hasher.update(vec![*x as u8]),
                NixValue::Float(x) => hasher.update(format!("{}", x)),
                NixValue::Integer(x) => hasher.update(format!("{}", x)),
                NixValue::Lambda(lambda) => match lambda {
                    NixLambda::Node {
                        lambda: _,
                        scope: _,
                    } => return Err(EvalError::Unimplemented("node".to_string())),
                    NixLambda::Builtin(x) => hasher.update(format!("{:?}", x)),
                },
                NixValue::List(_) => return Err(EvalError::Unimplemented("list".to_string())),
                NixValue::Map(_) => return Err(EvalError::Unimplemented("map".to_string())),
                NixValue::Null => hasher.update("null"),
                NixValue::Path(x, y) => {
                    hasher.update(format!("{:?}", expand_path(x.clone(), y.clone())))
                }
                NixValue::Str(x) => hasher.update(x),
            },
            TreeSource::LetAttr { path: _, value } => {
                hasher.update("letattr");
                hasher.update(value.as_ref()?.hash()?);
            }
            TreeSource::Ident { name } => {
                hasher.update("ident");
                hasher.update(match self.scope.get(name) {
                    Some(x) => x.hash()?,
                    None => return Err(EvalError::Unimplemented("legacy".to_string())),
                });
            }
            TreeSource::Apply { lambda, arg } => {
                hasher.update("apply");
                hasher.update(lambda.as_ref()?.hash()?);
                hasher.update("arg");
                hasher.update(arg.as_ref()?.hash()?);
            }
            TreeSource::Map {
                inherits,
                definitions: _,
            } => {
                for inherit in inherits {
                    hasher.update(inherit.as_ref()?.hash()?);
                }
            }
            _ => return Err(EvalError::Unimplemented("other".to_string())),
        }
        hasher.update(b")");
        let result = hasher.finalize();
        Ok(to_hex_string(result.to_vec()))
    }

    pub fn from_concrete(value: NixValue) -> Self {
        Self {
            value: GcCell::new(None),
            hash: GcCell::new(None),
            source: TreeSource::Literal { value },
            range: None,
            scope: Gc::new(Scope::None),
        }
    }

    pub fn eval(&self) -> Result<Gc<NixValue>, EvalError> {
        use std::ops::Deref;
        let value_borrow = self.value.borrow();
        if let Some(ref value) = value_borrow.deref() {
            Ok(value.clone())
        } else {
            drop(value_borrow);

            if let Ok(hash) = self.hash() {
                if let Some(value) = self.scope.pull_hash(hash) {
                    *self.value.borrow_mut() = Some(value.clone());
                    return Ok(value);
                }
            }

            let value = match self.eval_uncached() {
                Ok(x) => x,
                Err(e) => return Err(EvalError::StackTrace(format!("{}\n{}", self.position(), e))),
            };
            *self.value.borrow_mut() = Some(value.clone());
            if let Ok(hash) = self.hash() {
                self.scope.submit_hash(hash, value.clone());
            }
            Ok(value)
        }
    }

    fn position(&self) -> String {
        let path = match self.scope.root_path() {
            Some(x) => match x.canonicalize() {
                Ok(y) => format!("{:?}", y),
                Err(_) => "[unknown path]".to_string(),
            },
            None => "[unknown path]".to_string(),
        };
        path
    }

    fn eval_uncached(&self) -> Result<Gc<NixValue>, EvalError> {
        match &self.source {
            TreeSource::Legacy { syntax } => crate::parse::eval_node(syntax, self.scope.clone()),
            // TODO: actually evaluate the condition
            TreeSource::Assert { condition: _, body } => body.as_ref()?.eval(),
            TreeSource::IfElse {
                condition,
                true_body,
                false_body,
            } => {
                if condition.as_ref()?.eval()?.as_bool()? {
                    true_body.as_ref()?.eval()
                } else {
                    false_body.as_ref()?.eval()
                }
            }
            TreeSource::Paren { inner } => inner.as_ref()?.eval(),
            TreeSource::Dynamic { inner } => inner.as_ref()?.eval(),
            TreeSource::LetIn {
                inherits: _,
                definitions: _,
                body,
            } => body.as_ref()?.eval(),
            TreeSource::LetAttr { path: _, value } => value.as_ref()?.eval(),
            TreeSource::Literal { value } => Ok(Gc::new(value.clone())),
            TreeSource::Ident { name } => self
                .scope
                .get(name)
                .ok_or(EvalError::Unexpected(format!(
                    "missing {} from scope",
                    name
                )))?
                .eval(),
            TreeSource::Select { from, index } => {
                let key = index.as_ref()?.get_ident()?;
                let tmp = from.as_ref()?.eval()?;
                let map = tmp.as_map()?;
                let val = match map.get(&key) {
                    Some(x) => x,
                    None => return Err(EvalError::Unexpected(format!("missing attr {}", key))),
                };
                val.eval()
            }
            TreeSource::BoolAnd { left, right } => {
                if left.as_ref()?.eval()?.as_bool()? {
                    right.as_ref()?.eval()
                } else {
                    Ok(Gc::new(NixValue::Bool(false)))
                }
            }
            TreeSource::BoolOr { left, right } => {
                if !left.as_ref()?.eval()?.as_bool()? {
                    right.as_ref()?.eval()
                } else {
                    Ok(Gc::new(NixValue::Bool(true)))
                }
            }
            TreeSource::BinOp { op, left, right } => {
                use BinOpKind::*;
                use NixValue::*;
                let tmp1 = left.as_ref()?.eval()?;
                let tmp2 = right.as_ref()?.eval()?;
                let left = tmp1.borrow();
                let right = tmp2.borrow();
                match (left, right) {
                    (Map(x), Map(y)) => Ok(Gc::new(match op {
                        Update => {
                            let mut out = x.clone();
                            for (key, val) in y.iter() {
                                out.insert(key.clone(), val.clone());
                            }
                            Map(out)
                        }
                        Equal => Bool(Gc::ptr_eq(&tmp1, &tmp2) || left.equals(right)?),
                        NotEqual => Bool(!Gc::ptr_eq(&tmp1, &tmp2) && !left.equals(right)?),
                        _ => {
                            return Err(EvalError::Unexpected(format!(
                                "cannot handle map {:?} map",
                                op
                            )))
                        }
                    })),
                    (Path(x, y), Str(z)) => Ok(Gc::new(match op {
                        Add => Path(x.clone(), y.clone() + z),
                        _ => {
                            return Err(EvalError::Unexpected(format!(
                                "cannot handle {:?} {:?} {:?} {:?}",
                                x, y, op, z
                            )))
                        }
                    })),
                    (Bool(x), Bool(y)) => Ok(Gc::new(match op {
                        Concat => {
                            return Err(EvalError::Unexpected("cannot do int ++ int".to_string()))
                        }
                        Update => {
                            return Err(EvalError::Unexpected("cannot do int // int".to_string()))
                        }
                        Add | Sub | Mul | Div | Less | LessOrEq | More | MoreOrEq => {
                            return Err(EvalError::Unexpected(
                                "cannot do boolean arithmetic".to_string(),
                            ))
                        }
                        Equal => Bool(x == y),
                        NotEqual => Bool(x != y),
                    })),
                    (Integer(x), Integer(y)) => Ok(Gc::new(match op {
                        Concat => {
                            return Err(EvalError::Unexpected("cannot do int ++ int".to_string()))
                        }
                        Update => {
                            return Err(EvalError::Unexpected("cannot do int // int".to_string()))
                        }
                        Add => Integer(x + y),
                        Sub => Integer(x - y),
                        Mul => Integer(x * y),
                        Div => Integer(x / y),
                        Equal => Bool(x == y),
                        Less => Bool(x < y),
                        LessOrEq => Bool(x < y),
                        More => Bool(x > y),
                        MoreOrEq => Bool(x >= y),
                        NotEqual => Bool(x != y),
                    })),
                    (Float(x), Float(y)) => Ok(Gc::new(match op {
                        Concat => {
                            return Err(EvalError::Unexpected(
                                "cannot do float ++ float".to_string(),
                            ))
                        }
                        Update => {
                            return Err(EvalError::Unexpected(
                                "cannot do float // float".to_string(),
                            ))
                        }
                        Add => Float(x + y),
                        Sub => Float(x - y),
                        Mul => Float(x * y),
                        Div => Float(x / y),
                        Equal => Bool((x - y) < 0.001),
                        Less => Bool(x < y),
                        LessOrEq => Bool(x < y),
                        More => Bool(x > y),
                        MoreOrEq => Bool(x >= y),
                        NotEqual => Bool((x - y) >= 0.001),
                    })),
                    (Str(x), Str(y)) => Ok(Gc::new(match op {
                        Add => Str(x.to_string() + y),
                        Equal => Bool(x == y),
                        NotEqual => Bool(x != y),
                        _ => {
                            return Err(EvalError::Unexpected(format!(
                                "cannot handle {:?} {:?} {:?}",
                                x, op, y
                            )))
                        }
                    })),
                    (List(x), List(y)) => Ok(Gc::new(match op {
                        Equal => Bool(left.equals(right)?),
                        NotEqual => Bool(!left.equals(right)?),
                        Concat => List({
                            let mut new = vec![];
                            new.extend(x.clone());
                            new.extend(y.clone());
                            new
                        }),
                        _ => {
                            return Err(EvalError::Unexpected(format!(
                                "cannot handle list {:?} list",
                                op
                            )))
                        }
                    })),
                    (Null, Null) => match op {
                        Equal => Ok(Gc::new(Bool(true))),
                        NotEqual => Ok(Gc::new(Bool(false))),
                        _ => Err(EvalError::Unexpected(format!(
                            "cannot handle null {:?} null",
                            op
                        ))),
                    },
                    (x, y) => match op {
                        Equal => Ok(Gc::new(Bool(false))),
                        NotEqual => Ok(Gc::new(Bool(true))),
                        _ => Err(EvalError::Unexpected(format!(
                            "cannot handle {:?} {:?} {:?}",
                            x, op, y
                        ))),
                    },
                }
            }
            TreeSource::InSet { set, index } => Ok(Gc::new(NixValue::Bool({
                let map = set.as_ref()?.eval()?;
                let path = index.as_ref()?.get_idents()?;
                let out = map.contains(&path)?;
                println!("{:?} {}", path, out);
                out
            }))),
            TreeSource::Implication { left, right } => {
                if left.as_ref()?.eval()?.as_bool()? {
                    Ok(Gc::new(NixValue::Bool(right.as_ref()?.eval()?.as_bool()?)))
                } else {
                    Ok(Gc::new(NixValue::Bool(true)))
                }
            }
            TreeSource::With { value: _, body } => body.as_ref()?.eval(),
            TreeSource::List { items } => Ok(Gc::new(NixValue::List(
                items
                    .iter()
                    .map(|x| x.as_ref())
                    .filter_map(Result::ok)
                    .cloned()
                    .collect::<Vec<_>>(),
            ))),
            TreeSource::Apply { lambda, arg } => {
                let lambda = lambda.as_ref()?;
                let lambda = match lambda.eval()?.borrow() {
                    NixValue::Lambda(x) => x.clone(),
                    NixValue::Map(x) => {
                        let tmp = x.get("__functor").ok_or(EvalError::Parsing)?.eval()?;
                        let functor = tmp.as_lambda()?;
                        eval_lambda(&functor, lambda.clone())?.as_lambda()?
                    }
                    unknown => {
                        return Err(EvalError::Unexpected(format!(
                            "cannot apply lambda {:?}",
                            unknown
                        )))
                    }
                };
                eval_lambda(&lambda, arg.as_ref()?.clone())
            }
            TreeSource::Map {
                inherits: _,
                definitions: _,
            } => unreachable!(),
            TreeSource::MapAttr { path: _, value } => value.as_ref()?.eval(),
            TreeSource::StringInterpol { parts } => {
                let mut out = String::new();
                for part in parts {
                    out += &part.as_ref()?.eval()?.as_str()?;
                }
                Ok(Gc::new(NixValue::Str(out)))
            }
            TreeSource::UnaryInvert { value } => {
                Ok(Gc::new(NixValue::Bool(!value.as_ref()?.eval()?.as_bool()?)))
            }
            TreeSource::UnaryNegate { value } => {
                Ok(Gc::new(match value.as_ref()?.eval()?.borrow() {
                    NixValue::Integer(x) => NixValue::Integer(-x),
                    NixValue::Float(x) => NixValue::Float(-x),
                    _ => {
                        return Err(EvalError::TypeError(
                            "cannot negate a non-number".to_string(),
                        ))
                    }
                }))
            }
            TreeSource::Lambda {
                literal,
                params: _,
                body: _,
            } => Ok(literal.clone()),
            TreeSource::InferredIdent { name: _ } => Err(EvalError::Unknown),
        }
    }

    pub fn get_definition(&self) -> Option<Gc<Tree>> {
        use TreeSource::*;
        match &self.source {
            Ident { name } => self.scope.get(&name),
            InferredIdent { name } => self.scope.get(&name),
            Select { from, index } => {
                let idx = index.as_ref().ok()?.get_ident().ok()?;
                Some(
                    from.as_ref()
                        .ok()?
                        .eval()
                        .ok()?
                        .as_map()
                        .ok()?
                        .get(&idx)?
                        .clone(),
                )
            }
            _ => None,
        }
    }

    pub fn children(&self) -> Vec<&Gc<Tree>> {
        match &self.source {
            TreeSource::Legacy { syntax: _ } => vec![],
            TreeSource::Assert { condition, body } => vec![condition, body],
            TreeSource::IfElse {
                condition,
                true_body,
                false_body,
            } => vec![condition, true_body, false_body],
            TreeSource::Paren { inner } => vec![inner],
            TreeSource::Dynamic { inner } => vec![inner],
            TreeSource::LetIn {
                inherits,
                definitions,
                body,
            } => {
                let mut out = vec![body];
                out.extend(inherits);
                out.extend(definitions);
                out
            }
            TreeSource::LetAttr { path: _, value } => vec![value],
            TreeSource::Literal { value: _ } => vec![],
            TreeSource::Ident { name: _ } => vec![],
            TreeSource::Select { from, index } => match index {
                Ok(x) => {
                    let mut out = if let Ok(tmp) = from {
                        vec![tmp]
                    } else {
                        vec![]
                    };
                    out.extend(x.children());
                    return out;
                }
                Err(_) => vec![from],
            },
            TreeSource::BinOp { op: _, left, right } => vec![left, right],
            TreeSource::BoolAnd { left, right } => vec![left, right],
            TreeSource::BoolOr { left, right } => vec![left, right],
            TreeSource::InSet { set, index: _ } => vec![set],
            TreeSource::Implication { left, right } => vec![left, right],
            TreeSource::With { value, body } => vec![value, body],
            TreeSource::List { items } => {
                return items
                    .iter()
                    .map(|x| x.as_ref())
                    .filter_map(Result::ok)
                    .collect()
            }
            TreeSource::Apply { lambda, arg } => vec![lambda, arg],
            TreeSource::Map {
                inherits,
                definitions,
            } => {
                let mut out = vec![];
                out.extend(inherits);
                out.extend(definitions);
                out
            }
            TreeSource::MapAttr { path: _, value } => vec![value],
            TreeSource::StringInterpol { parts } => {
                let mut out = vec![];
                for part in parts {
                    out.push(part);
                }
                out
            }
            TreeSource::UnaryInvert { value } => vec![value],
            TreeSource::UnaryNegate { value } => vec![value],
            TreeSource::Lambda {
                literal: _,
                params,
                body,
            } => {
                let mut out = vec![body];
                out.extend(params);
                out
            }
            TreeSource::InferredIdent { name: _ } => vec![],
        }
        .into_iter()
        .map(|x| x.as_ref())
        .filter_map(Result::ok)
        .collect()
    }

    pub fn get_ident(&self) -> Result<String, EvalError> {
        use TreeSource::*;
        match &self.source {
            Ident { ref name } => Ok(name.clone()),
            Dynamic { ref inner } => inner.as_ref()?.eval()?.as_str(),
            Literal { ref value } => value.as_str(),
            other => Err(EvalError::Unexpected(format!(
                "cannot get ident from {:?}",
                other
            ))),
        }
    }

    pub fn get_idents(&self) -> Result<Vec<String>, EvalError> {
        use TreeSource::*;
        match &self.source {
            Ident { name } => Ok(vec![name.clone()]),
            Literal { ref value } => Ok(vec![value.as_str()?]),
            Dynamic { ref inner } => Ok(vec![inner.as_ref()?.eval()?.as_str()?]),
            Select { from, index } => Ok({
                let mut out = from.as_ref()?.get_idents()?;
                out.push(index.as_ref()?.get_ident()?);
                out
            }),
            _ => Err(EvalError::Unexpected("cannot get ident".to_string())),
        }
    }

    pub fn parse_legacy(node: &SyntaxNode, scope: Gc<Scope>) -> Result<Self, EvalError> {
        match Self::parse(node.clone(), scope) {
            Ok(x) => Ok({
                let tmp: &Tree = x.borrow();
                tmp.clone()
            }),
            Err(e) => Err(e),
        }
    }
}

pub fn merge_values(a: Gc<Tree>, b: Gc<Tree>) -> Result<Gc<Tree>, EvalError> {
    let a = match a.eval()?.borrow() {
        NixValue::Map(x) => x.clone(),
        x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    };
    let b = match b.eval()?.borrow() {
        NixValue::Map(x) => x.clone(),
        x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    };
    let mut out = HashMap::new();
    for (key, val) in a.iter() {
        let tmp = match b.get(key) {
            Some(x) => merge_values(x.clone(), val.clone())?,
            None => val.clone(),
        };
        out.insert(key.clone(), tmp);
    }
    for (key, val) in b.iter() {
        if !a.contains_key(key) {
            out.insert(key.clone(), val.clone());
        }
    }

    Ok(Gc::new(Tree::from_concrete(NixValue::Map(out))))
}
