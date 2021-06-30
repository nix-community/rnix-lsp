use crate::eval::Tree;
use crate::scope::*;
use crate::{builtins::NixBuiltin, EvalError};
use gc::{Finalize, Gc, Trace};
use rnix::types::{Lambda, PatBind, PatEntry, TokenWrapper, TypedNode};
use std::{borrow::Borrow, collections::HashMap, fmt::Debug, path::PathBuf};

#[derive(Clone, Trace, Finalize)]
pub enum NixValue {
    Bool(bool),
    Float(f64),
    Integer(i64),
    Lambda(NixLambda),
    List(Vec<Gc<Tree>>),
    Map(HashMap<String, Gc<Tree>>),
    Null,
    Path(NixPathAnchor, String),
    Str(String),
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum NixPathAnchor {
    Absolute,
    Relative(PathBuf),
}

#[derive(Clone, Trace, Finalize)]
pub enum NixLambda {
    Node {
        #[unsafe_ignore_trace]
        lambda: Lambda,
        scope: Gc<Scope>,
    },
    Builtin(NixBuiltin),
}

impl NixValue {
    fn type_name(&self) -> String {
        match self {
            NixValue::Bool(_) => "bool",
            NixValue::Float(_) => "float",
            NixValue::Integer(_) => "integer",
            NixValue::Lambda(_) => "lambda",
            NixValue::List(_) => "list",
            NixValue::Map(_) => "map",
            NixValue::Null => "null",
            NixValue::Path(_, _) => "path",
            NixValue::Str(_) => "string",
        }
        .to_string()
    }

    pub fn as_str(&self) -> Result<String, EvalError> {
        match self {
            NixValue::Str(x) => Ok(x.clone()),
            _ => Err(EvalError::TypeError(format!(
                "expected string, got {}",
                self.type_name()
            ))),
        }
    }

    pub fn as_lambda(&self) -> Result<NixLambda, EvalError> {
        match self {
            NixValue::Lambda(x) => Ok(x.clone()),
            _ => Err(EvalError::TypeError(format!(
                "expected lambda, got {}",
                self.type_name()
            ))),
        }
    }

    pub fn as_bool(&self) -> Result<bool, EvalError> {
        match self {
            NixValue::Bool(x) => Ok(*x),
            _ => Err(EvalError::TypeError(format!(
                "expected bool, got {}",
                self.type_name()
            ))),
        }
    }

    pub fn as_int(&self) -> Result<i64, EvalError> {
        match self {
            NixValue::Integer(x) => Ok(*x),
            _ => Err(EvalError::TypeError(format!(
                "expected int, got {}",
                self.type_name()
            ))),
        }
    }

    pub fn as_list(&self) -> Result<Vec<Gc<Tree>>, EvalError> {
        match self {
            NixValue::List(x) => Ok(x.clone()),
            _ => Err(EvalError::TypeError(format!(
                "expected list, got {}",
                self.type_name()
            ))),
        }
    }

    pub fn as_map(&self) -> Result<HashMap<String, Gc<Tree>>, EvalError> {
        match self {
            NixValue::Map(x) => Ok(x.clone()),
            _ => Err(EvalError::TypeError(format!(
                "expected map, got {}",
                self.type_name()
            ))),
        }
    }

    pub fn contains(&self, path: &[String]) -> Result<bool, EvalError> {
        let mut cursor = Gc::new(self.clone());
        for part in path {
            let map = match cursor.as_map() {
                Ok(x) => x,
                Err(_) => return Ok(false),
            };
            match map.get(part) {
                Some(x) => cursor = x.eval()?,
                None => return Ok(false),
            }
        }
        Ok(true)
    }

    pub fn equals(&self, other: &Self) -> Result<bool, EvalError> {
        match (self, other) {
            (NixValue::Integer(x), NixValue::Integer(y)) => Ok(x == y),
            (NixValue::Float(x), NixValue::Float(y)) => Ok((x - y).abs() < 0.001),
            (NixValue::Str(x), NixValue::Str(y)) => Ok(x == y),
            (NixValue::Null, NixValue::Null) => Ok(true),
            (NixValue::Bool(x), NixValue::Bool(y)) => Ok(x == y),
            (NixValue::Map(x), NixValue::Map(y)) => {
                let x: &HashMap<_, _> = x.borrow();
                let y: &HashMap<_, _> = y.borrow();
                for (key, valx) in x.iter() {
                    match y.get(key) {
                        Some(valy) => {
                            let a: Gc<NixValue> = valx.eval()?;
                            let b: Gc<NixValue> = valy.eval()?;
                            if !a.equals(&b)? {
                                println!("MISMATCH: {}", key);
                                return Ok(false);
                            }
                        }
                        None => return Ok(false),
                    }
                }
                for key in y.keys() {
                    if !x.contains_key(key) {
                        println!("MISSING KEY: {}", key);
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (NixValue::List(x), NixValue::List(y)) => {
                let mut x1 = vec![];
                for item in x {
                    x1.push(item.eval()?);
                }
                let mut y1 = vec![];
                for item in y {
                    y1.push(item.eval()?);
                }
                if x1.len() != y1.len() {
                    Ok(false)
                } else {
                    for i in 0..x1.len() {
                        if !x1[i].equals(&y1[i])? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
            }
            _ => Ok(false),
        }
    }

    pub fn format_markdown(&self) -> String {
        use NixValue::*;
        match self {
            Bool(x) => format!("```nix\n{}\n```", x),
            Float(x) => format!("```nix\n{}\n```", x),
            Integer(x) => format!("```nix\n{}\n```", x),
            Null => "```nix\nnull\n```".to_string(),
            // TODO: show definition!
            Lambda(_) => "```nix\n<CODE>\n```".to_string(),
            List(_) => "```nix\n[ ... ]\n```".to_string(),
            // TODO: show derivations and functors!
            Map(map) => {
                if map.is_empty() {
                    "```nix\n{ }\n```".to_string()
                } else if let Some(out) = try_format_derivation(map) {
                    out
                } else {
                    let mut attrs = map
                        .keys()
                        .map(|x| format!("  {} = ...;\n", x))
                        .collect::<Vec<_>>();

                    let limit = 100;
                    if attrs.len() > limit {
                        attrs.truncate(limit);
                        attrs.push("  /* ... */\n".to_string());
                    }

                    format!("```nix\n{{\n{}}}\n```", attrs.join(""))
                }
            }
            Path(_, s) => s.clone(),
            Str(s) => format!("```nix\n{:?}\n```", s),
        }
    }
}

fn try_format_derivation(input: &HashMap<String, Gc<Tree>>) -> Option<String> {
    if input.get("type")?.eval().ok()?.as_str().ok()? != "derivation" {
        return None;
    }

    let name = input.get("name")?.eval().ok()?.as_str().ok()?;
    let meta = input
        .get("meta")
        .and_then(|x| x.eval().ok())
        .and_then(|x| x.as_map().ok());
    let description = meta.and_then(|x| x.get("description")?.eval().ok()?.as_str().ok());

    let body_text = description
        .map(|x| "\n".to_string() + &x)
        .unwrap_or_else(|| "".to_string());

    Some(format!(
        "```nix\nderivation {{ name = {:?}; }}\n```{}",
        name, body_text
    ))
}

impl Debug for NixValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NixValue::Lambda(lambda) => match &lambda {
                &NixLambda::Node { lambda, scope: _ } => {
                    use rnix::SyntaxKind::*;
                    let arg = lambda.arg().unwrap();
                    match arg.kind() {
                        NODE_PATTERN => {
                            let mut current_comment = "".to_string();
                            for child in arg.children_with_tokens().skip(1) {
                                match child.kind() {
                                    TOKEN_WHITESPACE | TOKEN_COMMA => continue,
                                    TOKEN_COMMENT | TOKEN_CURLY_B_CLOSE => {
                                        let s = child.to_string();
                                        let text = s.trim().trim_start_matches('#').trim_start();
                                        current_comment += text;
                                        current_comment += " ";
                                    }
                                    NODE_PAT_ENTRY => {
                                        let node = child.as_node().unwrap().clone();
                                        let entry = PatEntry::cast(node).unwrap();
                                        let name = entry.name().unwrap();
                                        writeln!(
                                            f,
                                            "{} \x1b[2m{}\x1b[m",
                                            name.as_str(),
                                            current_comment
                                        )?;
                                        current_comment = "".to_string();
                                    }
                                    TOKEN_ELLIPSIS => {
                                        writeln!(f, "...")?;
                                    }
                                    NODE_PAT_BIND => {
                                        let node = child.as_node().unwrap().clone();
                                        let bind = PatBind::cast(node).unwrap();
                                        let name = bind.name().unwrap();
                                        writeln!(f, "@ {}", name.as_str())?;
                                    }
                                    x => write!(f, "[unrecognized: {:?}]", x)?,
                                }
                            }

                            Ok(())
                        }
                        NODE_IDENT => {
                            let ident = rnix::types::Ident::cast(arg).unwrap();
                            write!(f, "{}: {}", ident.as_str(), lambda.body().unwrap().text())
                        }
                        _ => write!(f, "??"),
                    }
                }
                NixLambda::Builtin(builtin) => write!(f, "{:?}", builtin),
            },
            NixValue::List(_) => write!(f, "[ ... ]"),
            NixValue::Map(map) => write!(
                f,
                "{{ {} }}",
                map.keys().cloned().collect::<Vec<_>>().join(" ; ")
            ),
            NixValue::Bool(x) => write!(f, "{}", x),
            NixValue::Float(x) => write!(f, "{}", x),
            NixValue::Integer(x) => write!(f, "{}", x),
            NixValue::Null => write!(f, "null"),
            NixValue::Path(_, _) => write!(f, "<PATH>"),
            NixValue::Str(x) => write!(f, "{:?}", x),
        }
    }
}
