use crate::{
    eval::{eval_lambda, expand_path, Tree, TreeSource},
    scope::Scope,
    value::*,
    EvalError,
};
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::types::{TokenWrapper, TypedNode, Wrapper};
use std::{borrow::Borrow, collections::HashMap, path::PathBuf, str::FromStr};

#[macro_export]
macro_rules! maybe_push {
    ($map:ident ; $description:expr ; $kind:ident) => {
        let item = NixBuiltin::$kind;
        let tree = Gc::new(Tree {
            value: GcCell::new(None),
            hash: GcCell::new(None),
            source: TreeSource::Literal {
                value: NixValue::Lambda(NixLambda::Builtin(item)),
            },
            range: None,
            scope: Gc::new(Scope::None),
        });
        $map.insert($description.to_string(), tree);
    };
    ($map:ident ; $description:expr ; $kind:ident$(($($member:ty),*))?) => {};
}

#[macro_export]
macro_rules! builtins {
    ($($description:expr ; $kind:ident$(($($varname:ident: $member:ty),*))? => $body:expr)*) => {
        #[derive(Clone, Trace, Finalize)]
        pub enum NixBuiltin {
            $(
                $kind$(($($member),*))?
            ),*
        }

        pub fn make_builtins_map() -> NixValue {
            let mut map = HashMap::<String, Gc<Tree>>::new();

            $(
                maybe_push! { map ; $description ; $kind$(($($member),*))? }
            )*

            map.insert("nixVersion".into(), Gc::new(Tree::from_concrete(
                NixValue::Str("2.2".into()))));
            map.insert("currentSystem".into(), Gc::new(Tree::from_concrete(
                NixValue::Str("x86_64-linux".into()))));

            NixValue::Map(map)
        }

        impl NixBuiltin {
            pub fn call(&self, param: Gc<Tree>) -> Result<Gc<NixValue>, EvalError> {
                match self {
                    $(
                        Self::$kind$(($($varname),*))? => $body(param $($(,$varname)*)?),
                    )*
                }
            }
        }

        impl std::fmt::Debug for NixBuiltin {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$kind$(($($varname),*))? => write!(f, $description),
                    )*
                }
            }
        }
    }
}

type GcLazy = Gc<Tree>;

builtins! {
    "abort" ; Abort => |_param| {
        Err(EvalError::Unimplemented("abort".to_string()))
    }
    "attrNames" ; AttrNames => |param: Gc<Tree>| {
        match param.eval()?.borrow() {
            NixValue::Map(map) => {
                let list = map.keys().map(|x| Gc::new(Tree::from_concrete(
                    NixValue::Str(x.clone()))
                )).collect();
                Ok(Gc::new(NixValue::List(list)))
            },
            x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        }
    }
    "genericClosure" ; GenericClosure => |param: Gc<Tree>| {
        let tmp = param.eval()?;
        let param = match tmp.borrow() {
            NixValue::Map(x) => x,
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let tmp = param.get("startSet").ok_or(EvalError::Parsing)?.eval()?;
        let mut items = match tmp.borrow() {
            NixValue::List(x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let tmp = param.get("operator").ok_or(EvalError::Parsing)?.eval()?;
        let operator = match tmp.borrow() {
            NixValue::Lambda(x) => x,
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        for i in 0.. {
            if i >= items.len() {
                break
            }
            let new = match eval_lambda(operator, items[i].clone())?.borrow() {
                NixValue::List(x) => x.clone(),
                x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
            };
            items.extend(new);
        }
        Ok(Gc::new(NixValue::List(items)))
    }
    "map" ; Map => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Map1(param)))))
    }
    "map <?>" ; Map1(_0: Gc<Tree>) => |param: Gc<Tree>, lambda: &Gc<Tree>| {
        let tmp = param.eval()?;
        let list = tmp.as_list()?;
        let mut out = vec![];
        for item in list {
            let tree = Gc::new(Tree {
                value: GcCell::new(None),
                hash: GcCell::new(None),
                source: TreeSource::Apply {
                    lambda: Ok(lambda.clone()),
                    arg: Ok(item.clone()),
                },
                range: None,
                scope: Gc::new(Scope::None)
            });
            out.push(tree);
        }
        Ok(Gc::new(NixValue::List(out)))
    }
    "filter" ; Filter => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Filter1(param)))))
    }
    "filter <?>" ; Filter1(_0: Gc<Tree>) => |param: Gc<Tree>, lambda: &Gc<Tree>| {
        let tmp = param.eval()?;
        let list = tmp.as_list()?;
        let mut out = vec![];
        let lambda = lambda.eval()?.as_lambda()?;
        for item in list {
            let condition = match eval_lambda(&lambda, item.clone())?.borrow() {
                NixValue::Bool(x) => *x,
                x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
            };
            if condition {
                out.push(item.clone());
            }
        }
        Ok(Gc::new(NixValue::List(out)))
    }
    "functionArgs" ; FunctionArgs => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Lambda(NixLambda::Node { lambda, .. }) => {
            let arg = lambda.arg().ok_or(EvalError::Parsing)?;
            use rnix::SyntaxKind::*;
            match arg.kind() {
                NODE_PATTERN => {
                    let mut map = HashMap::new();
                    let pattern = rnix::types::Pattern::cast(arg).ok_or(EvalError::Parsing)?;
                    for entry in pattern.entries() {
                        let tmp = entry.name().ok_or(EvalError::Parsing)?;
                        let name = tmp.as_str();
                        let value = entry.default().is_some();
                        map.insert(name.to_string(), Gc::new(Tree::from_concrete(
                            NixValue::Bool(value)
                        )));
                    }
                    Ok(Gc::new(NixValue::Map(map)))
                }
                NODE_IDENT => {
                    Ok(Gc::new(NixValue::Map(HashMap::new())))
                }
                x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
            }
        },
        x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    }
    "length" ; Length => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::List(x) => Ok(Gc::new(NixValue::Integer(x.len() as i64))),
        x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    }
    "head" ; Head => |param: Gc<Tree>| param.eval()?.as_list()?[0].eval()
    "tail" ; Tail => |param: Gc<Tree>| Ok(Gc::new(
        NixValue::List(param.eval()?.as_list()?[1..].to_vec())))
    "stringLength" ; StringLength => |param: Gc<Tree>| Ok(Gc::new(
        NixValue::Integer(param.eval()?.as_str()?.len() as i64)))
    "getEnv" ; GetEnv => |param: Gc<Tree>| Ok(Gc::new(NixValue::Str(
        std::env::var(param.eval()?.as_str()?).unwrap_or_else(|_| "".into()))))
    "pathExists" ; PathExists => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Path(x, y) => Ok(Gc::new(NixValue::Bool(expand_path(x.clone(), y.clone()).ok_or(EvalError::Parsing)?.exists()))),
        NixValue::Str(x) => Ok(Gc::new(NixValue::Bool(PathBuf::from_str(x).map_err(|_| EvalError::Parsing)?.exists()))),
        x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    }
    "unsafeDiscardStringContext" ; UnsafeDiscardStringContext => |param: Gc<Tree>| {
        param.eval()
    }
    // FIXME: missing functionality
    "addErrorContext" ; AddErrorContext => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::AddErrorContext1(param)))))
    }
    "addErrorContext <?>" ; AddErrorContext1(_0: GcLazy) => |param: Gc<Tree>, _x| {
        param.eval()
    }
    // FIXME: missing functionality
    "trace" ; Trace => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Trace1(param)))))
    }
    "trace <?>" ; Trace1(_0: GcLazy) => |param: Gc<Tree>, _x| {
        param.eval()
    }
    // FIXME: missing functionality
    "tryEval" ; TryEval => |param: Gc<Tree>| {
        let mut map = HashMap::new();
        let tmp = param.eval();
        map.insert("success".to_string(), Gc::new(Tree::from_concrete(
            NixValue::Bool(tmp.is_ok())
        )));
        match tmp {
            Ok(x) => map.insert("value".to_string(), Gc::new(
                Tree::from_concrete({
                    let a: &NixValue = x.borrow();
                    a.clone()
                }))),
            Err(_) => map.insert("value".to_string(), Gc::new(
                Tree::from_concrete(NixValue::Bool(false)))),
        };
        Ok(Gc::new(NixValue::Map(map)))
    }
    "elemAt" ; ElemAt => |param: Gc<Tree>|  {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::ElemAt1(param)))))
    }
    "elemAt <?>" ; ElemAt1(_0: GcLazy) => |param: Gc<Tree>, list: &GcLazy| {
        let index = param.eval()?.as_int()?;
        match list.eval()?.borrow() {
            NixValue::List(x) => x[index as usize].eval(),
            x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        }
    }
    "hasAttr" ; HasAttr => |param: Gc<Tree>|  {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::HasAttr1(param)))))
    }
    "hasAttr <?>" ; HasAttr1(_0: GcLazy) => |param: Gc<Tree>, index: &GcLazy| {
        let map = param.eval()?.as_map()?;
        let key = index.eval()?.as_str()?;
        Ok(Gc::new(NixValue::Bool(map.contains_key(&key))))
    }
    "elem" ; Elem => |param: Gc<Tree>|  {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Elem1(param)))))
    }
    "elem <?>" ; Elem1(_0: GcLazy) => |param: Gc<Tree>, test: &GcLazy| {
        let tmp = param.eval()?;
        let list = tmp.as_list()?;
        let test = test.eval()?;
        for item in list {
            if item.eval()?.equals(&test)? {
                return Ok(Gc::new(NixValue::Bool(true)))
            }
        }
        Ok(Gc::new(NixValue::Bool(false)))
    }
    "seq" ; Seq => |param: Gc<Tree>|  {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Seq1(param)))))
    }
    "seq <?>" ; Seq1(_0: GcLazy) => |param: Gc<Tree>, first: &GcLazy| {
        first.eval()?;
        param.eval()
    }
    "isString" ; IsString => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Str(_) => Ok(Gc::new(NixValue::Bool(true))),
        _ => Ok(Gc::new(NixValue::Bool(false))),
    }
    "isAttrs" ; IsAttrs => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Map(_) => Ok(Gc::new(NixValue::Bool(true))),
        _ => Ok(Gc::new(NixValue::Bool(false))),
    }
    "isBool" ; IsBool => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Bool(_) => Ok(Gc::new(NixValue::Bool(true))),
        _ => Ok(Gc::new(NixValue::Bool(false))),
    }
    "isFunction" ; IsFunction => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Lambda(_) => Ok(Gc::new(NixValue::Bool(true))),
        _ => Ok(Gc::new(NixValue::Bool(false))),
    }
    "isList" ; IsList => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::List(_) => Ok(Gc::new(NixValue::Bool(true))),
        _ => Ok(Gc::new(NixValue::Bool(false))),
    }
    "genList" ; GenList => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::GenList1(param)))))
    }
    "genList <?>" ; GenList1(_0: Gc<Tree>) => |param: Gc<Tree>, lambda: &Gc<Tree>| {
        let num = param.eval()?.as_int()?;
        let mut out = vec![];
        for i in 0..num {
            let tree = Gc::new(Tree {
                value: GcCell::new(None),
                hash: GcCell::new(None),
                source: TreeSource::Apply {
                    lambda: Ok(lambda.clone()),
                    arg: Ok(Gc::new(Tree::from_concrete(NixValue::Integer(i)))),
                },
                range: None,
                scope: Gc::new(Scope::None)
            });
            out.push(tree);
        }
        Ok(Gc::new(NixValue::List(out)))
    }
    "listToAttrs" ; ListToAttrs => |param: Gc<Tree>| {
        match param.eval()?.borrow() {
            NixValue::List(list) => {
                let mut out_map = HashMap::new();
                for item in list {
                    match item.eval()?.borrow() {
                        NixValue::Map(ref map) => {
                            let tmp = map.get("name").ok_or(EvalError::Parsing)?;
                            let key = match tmp.eval()?.borrow() {
                                NixValue::Str(ref x) => x.clone(),
                                x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
                            };
                            let val = map.get("value").ok_or(EvalError::Parsing)?;
                            out_map.insert(key.to_string(), val.clone());
                        }
                        x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
                    }
                }
                Ok(Gc::new(NixValue::Map(out_map)))
            }
            x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        }
    }
    "split" ; Split => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Split1(param.eval()?.as_str()?)))))
    }
    "split <?>" ; Split1(_0: String) => |param: Gc<Tree>, regex_str: &String| {
        let regex_str = regex_str
            .replace("\\|", "%%PIPE%%")
            .replace("\\(", "%%L_PAREN%%")
            .replace("\\)", "%%R_PAREN%%")
            .replace("|", "\\|")
            .replace("(", "\\(")
            .replace(")", "\\)")
            .replace("%%PIPE%%", "|")
            .replace("%%L_PAREN%%", "(")
            .replace("%%R_PAREN%%", ")");
        let regex = posix_regex::compile::PosixRegexBuilder::new(regex_str.as_bytes())
            .with_default_classes()
            .compile()
            .expect("error compiling regex");
        let param = match param.eval()?.borrow() {
            NixValue::Str(x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let matches = regex.matches(param.as_bytes(), None);

        let mut out = vec![];

        let mut cursor = 0;
        for found in matches {
            let (from, to) = found[0].ok_or(EvalError::Parsing)?;
            out.push(Gc::new(Tree::from_concrete(NixValue::Str(
                param[cursor..from].to_string(),
            ))));
            let mut here = vec![];
            for group in found.iter().skip(1) {
                here.push(match group {
                    Some((x, y)) => {
                        Gc::new(Tree::from_concrete(NixValue::Str(param[*x..*y].to_string())))
                    }
                    None => Gc::new(Tree::from_concrete(NixValue::Null)),
                });
            }
            out.push(Gc::new(Tree::from_concrete(NixValue::List(here))));
            cursor = to;
        }
        out.push(Gc::new(Tree::from_concrete(NixValue::Str(
            param[cursor..].to_string(),
        ))));
        Ok(Gc::new(NixValue::List(out)))
    }
    "concatStringsSep" ; ConcatStringsSep => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::ConcatStringsSep1(param.eval()?.as_str()?)))))
    }
    "concatStringsSep <?>" ; ConcatStringsSep1(_0: String) => |param: Gc<Tree>, sep: &String| {
        let list = param.eval()?.as_list()?;
        let mut out = vec![];
        for item in list {
            out.push(match item.eval()?.borrow() {
                NixValue::Str(ref x) => x.clone(),
                x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
            });
        }
        Ok(Gc::new(NixValue::Str(out.join(sep))))
    }
    "unsafeGetAttrPos" ; UnsafeGetAttrPos => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::UnsafeGetAttrPos1(match param.eval()?.borrow() {
            NixValue::Str(x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        })))))
    }
    // FIXME: missing functionality
    "unsafeGetAttrPos <?>" ; UnsafeGetAttrPos1(_0: String) => |_param: Gc<Tree>, _attr: &String| {
        let mut map = HashMap::new();
        map.insert("column".to_string(), Gc::new(Tree::from_concrete(
            NixValue::Integer(1)
        )));
        map.insert("line".to_string(), Gc::new(Tree::from_concrete(
            NixValue::Integer(1)
        )));
        map.insert("file".to_string(), Gc::new(Tree::from_concrete(
            NixValue::Str("unknown".to_string())
        )));
        Ok(Gc::new(NixValue::Map(map)))
    }
    "toString" ; ToString => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Str(x) => Ok(Gc::new(NixValue::Str(x.clone()))),
        NixValue::Integer(x) => Ok(Gc::new(NixValue::Str(format!("{}", x)))),
        NixValue::Float(x) => Ok(Gc::new(NixValue::Str(format!("{}", x)))),
        // FIXME: incorrect!
        NixValue::Path(_, _) => Ok(Gc::new(NixValue::Str(
            "/nix/store/00000000000000000000000000000000-fake-path".to_string()))),
        x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    }
    "removeAttrs" ; RemoveAttrs => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::RemoveAttrs1(param)))))
    }
    "removeAttrs <?>" ; RemoveAttrs1(_0: Gc<Tree>) => |param: Gc<Tree>, map: &Gc<Tree>| {
        let mut new_map = match map.eval()?.borrow() {
            NixValue::Map(x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let tmp = param.eval()?;
        let items = match tmp.borrow() {
            NixValue::List(x) => x,
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        for item in items {
            let key = match item.eval()?.borrow() {
                NixValue::Str(ref x) => x.clone(),
                x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
            };
            new_map.remove(&key);
        }
        Ok(Gc::new(NixValue::Map(new_map)))
    }
    "intersectAttrs" ; IntersectAttrs => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::IntersectAttrs1(param)))))
    }
    "intersectAttrs <?>" ; IntersectAttrs1(_0: Gc<Tree>) => |param: Gc<Tree>, a: &Gc<Tree>| {
        let a = match a.eval()?.borrow() {
            NixValue::Map(x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let b = match param.eval()?.borrow() {
            NixValue::Map(x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let mut map = HashMap::new();
        for (bkey, bval) in b.iter() {
            if a.contains_key(bkey) {
                map.insert(bkey.clone(), bval.clone());
            }
        }
        Ok(Gc::new(NixValue::Map(map)))
    }
    "concatLists" ; ConcatLists => |param: Gc<Tree>| {
        let list = match param.eval()?.borrow() {
            NixValue::List(ref x) => x.clone(),
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        };
        let mut out = vec![];
        for item in list {
            let sublist = match item.eval()?.borrow() {
                NixValue::List(ref x) => x.clone(),
                x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
            };
            out.extend(sublist);
        }
        Ok(Gc::new(NixValue::List(out)))
    }
    "substring" ; Substring => |param: Gc<Tree>| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Substring1(match param.eval()?.borrow() {
            NixValue::Integer(x) => *x as usize,
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        })))))
    }
    "substring <?>" ; Substring1(_0: usize) => |param: Gc<Tree>, from: &usize| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Substring2(*from, match param.eval()?.borrow() {
            NixValue::Integer(x) => *x as usize,
            x => return Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        })))))
    }
    "substring <?> <?>" ; Substring2(_0: usize, _1: usize) => |param: Gc<Tree>, from: &usize, len: &usize| {
        let param = param.eval()?.as_str()?;
        Ok(Gc::new(NixValue::Str(param[*from..(*from + *len).min(param.len())].to_string())))
    }
    "compareVersions" ; CompareVersions => |param| {
        Ok(Gc::new(NixValue::Lambda(NixLambda::Builtin(NixBuiltin::CompareVersions1(param)))))
    }
    "compareVersions <?>" ; CompareVersions1(_0: Gc<Tree>) => |param: Gc<Tree>, left: &Gc<Tree>| {
        let split = |version: String| -> Vec<String> {
            let mut out = vec![];
            #[derive(PartialEq, Eq)]
            enum Mode {
                Alpha,
                Num,
                None,
            }
            let mut mode = Mode::None;
            let mut curr = String::new();
            for ch in version.chars() {
                match ch {
                    '0'..='9' => {
                        if mode == Mode::Alpha {
                            out.push(curr);
                            curr = String::new();
                        }
                        curr.push(ch);
                        mode = Mode::Num;
                    }
                    'a'..='z' => {
                        if mode == Mode::Num {
                            out.push(curr);
                            curr = String::new();
                        }
                        curr.push(ch);
                        mode = Mode::Alpha;
                    }
                    _ => {
                        if mode != Mode::None {
                            out.push(curr);
                            curr = String::new();
                            mode = Mode::None;
                        }
                    }
                }
            }
            if !curr.is_empty() {
                out.push(curr);
            }
            out
        };
        let left = left.eval()?.as_str()?;
        let right = param.eval()?.as_str()?;
        let a = split(left);
        let b = split(right);
        // if a > b { 1 } else { -1 }
        for i in 0..a.len().max(b.len()) {
            match (a.get(i), b.get(i)) {
                (Some(x), Some(y)) => {
                    if x == "pre" {
                        return Ok(Gc::new(NixValue::Integer(-1)));
                    } else if y == "pre" {
                        return Ok(Gc::new(NixValue::Integer(1)));
                    } else {
                        use std::cmp::Ordering;
                        match (x.parse::<i32>().ok(), y.parse::<i32>().ok()) {
                            (Some(a), Some(b)) => {
                                match a.cmp(&b) {
                                    Ordering::Greater => return Ok(Gc::new(NixValue::Integer(1))),
                                    Ordering::Less => return Ok(Gc::new(NixValue::Integer(-1))),
                                    Ordering::Equal => continue,
                                }
                            }
                            (Some(_), None) => return Ok(Gc::new(NixValue::Integer(-1))),
                            (None, Some(_)) => return Ok(Gc::new(NixValue::Integer(1))),
                            (None, None) => {
                                match x.cmp(&y) {
                                    Ordering::Greater => return Ok(Gc::new(NixValue::Integer(1))),
                                    Ordering::Less => return Ok(Gc::new(NixValue::Integer(-1))),
                                    Ordering::Equal => continue,
                                }
                            }
                        }
                    }
                }
                (Some(_), None) => return Ok(Gc::new(NixValue::Integer(1))),
                (None, Some(_)) => return Ok(Gc::new(NixValue::Integer(-1))),
                (None, None) => panic!(),
            }
        }
        Ok(Gc::new(NixValue::Integer(0)))
    }
    "import" ; Import => |param: Gc<Tree>| {
        match param.eval()?.borrow() {
            NixValue::Path(ref base, ref path) => {
                let path = expand_path(base.clone(), path.clone()).ok_or(EvalError::Parsing)?;
                let path = if path.is_dir() {
                    path.join("default.nix")
                } else {
                    path
                };
                let source = std::fs::read_to_string(&path).map_err(|_| EvalError::Parsing)?;
                let foreign_root = rnix::parse(&source).root().inner().ok_or(EvalError::Parsing)?;
                Ok(Tree::parse_legacy(&foreign_root, Gc::new(Scope::Root(path, None)))?.eval()?)
            }
            x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        }
    }
    "readFile" ; ReadFile => |param: Gc<Tree>| {
        match param.eval()?.borrow() {
            NixValue::Path(x, y) => {
                let path = expand_path(x.clone(), y.clone()).ok_or(EvalError::Parsing)?;
                let source = std::fs::read_to_string(path).map_err(|_| EvalError::Parsing)?;
                Ok(Gc::new(NixValue::Str(source)))
            }
            x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
        }
    }
    "fromJSON" ; FromJSON => |param: Gc<Tree>| match param.eval()?.borrow() {
        NixValue::Str(ref source) => {
            let parsed: serde_json::Value = serde_json::from_str(source).map_err(|_| EvalError::Parsing)?;
            Ok(Gc::new(json_to_nix(&parsed)))
        }
        x => Err(EvalError::Unimplemented(format!("cannot cast {:?}", x))),
    }
    "mapAttrs" ; MapAttrs =>  |param: Gc<Tree>| Ok(Gc::new(NixValue::Lambda(
        NixLambda::Builtin(NixBuiltin::MapAttrs1(param)))))
    "mapAttrs <?>" ; MapAttrs1(_0: Gc<Tree>) => |param: Gc<Tree>, lambda: &Gc<Tree>| {
        let attrs = param.eval()?.as_map()?;
        let mut new_map = HashMap::new();
        for (key, value) in (&attrs).iter() {
            let inner = Gc::new(Tree {
                value: GcCell::new(None),
                hash: GcCell::new(None),
                source: TreeSource::Apply {
                    lambda: Ok(lambda.clone()),
                    arg: Ok(Gc::new(Tree::from_concrete(NixValue::Str(key.clone())))),
                },
                range: None,
                scope: Gc::new(Scope::None)
            });
            let tree = Gc::new(Tree {
                value: GcCell::new(None),
                hash: GcCell::new(None),
                source: TreeSource::Apply {
                    lambda: Ok(inner),
                    arg: Ok(value.clone()),
                },
                range: None,
                scope: Gc::new(Scope::None)
            });
            new_map.insert(key.clone(), tree);
        }
        Ok(Gc::new(NixValue::Map(new_map)))
    }
}

fn json_to_nix(value: &serde_json::Value) -> NixValue {
    match value {
        serde_json::Value::Null => NixValue::Null,
        serde_json::Value::Bool(x) => NixValue::Bool(*x),
        serde_json::Value::Number(num) => {
            if num.is_i64() {
                NixValue::Integer(num.as_i64().unwrap())
            } else {
                NixValue::Float(num.as_f64().unwrap())
            }
        }
        serde_json::Value::String(s) => NixValue::Str(s.clone()),
        serde_json::Value::Array(arr) => NixValue::List(
            arr.iter()
                .map(json_to_nix)
                .map(Tree::from_concrete)
                .map(Gc::new)
                .collect(),
        ),
        serde_json::Value::Object(map) => {
            let mut out_obj = HashMap::new();
            for (key, val) in map.iter() {
                out_obj.insert(key.clone(), Gc::new(Tree::from_concrete(json_to_nix(val))));
            }
            NixValue::Map(out_obj)
        }
    }
}
