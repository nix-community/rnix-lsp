use crate::builtins::NixBuiltin;
use crate::eval::Tree;
use crate::value::{NixLambda, NixValue};
use gc::{Finalize, Gc, GcCell, Trace};
use std::{borrow::Borrow, collections::HashMap, path::PathBuf};

#[derive(Trace, Finalize)]
pub enum Scope {
    None,
    Root(PathBuf, Option<Gc<GcCell<HashMap<String, Gc<NixValue>>>>>),
    Normal {
        parent: Gc<Scope>,
        contents: GcCell<HashMap<String, Gc<Tree>>>,
    },
    With {
        parent: Gc<Scope>,
        contents: Gc<Tree>,
    },
}

impl Scope {
    pub fn root_path(&self) -> Option<PathBuf> {
        match &self {
            Scope::None => None,
            Scope::Root(path, _) => Some(path.clone()),
            Scope::Normal { parent, .. } => parent.root_path(),
            Scope::With { parent, .. } => parent.root_path(),
        }
    }

    pub fn submit_hash(&self, hash: String, value: Gc<NixValue>) {
        match &self {
            Scope::Root(_, None) | Scope::None => (),
            Scope::Root(_, Some(store)) => {
                let tmp: &GcCell<_> = store.borrow();
                tmp.borrow_mut().insert(hash, value);
            }
            Scope::Normal { parent, .. } => parent.submit_hash(hash, value),
            Scope::With { parent, .. } => parent.submit_hash(hash, value),
        }
    }

    pub fn pull_hash(&self, hash: String) -> Option<Gc<NixValue>> {
        match &self {
            Scope::Root(_, None) | Scope::None => None,
            Scope::Root(_, Some(store)) => {
                let tmp: &GcCell<_> = store.borrow();
                tmp.borrow().get(&hash).cloned()
            }
            Scope::Normal { parent, .. } => parent.pull_hash(hash),
            Scope::With { parent, .. } => parent.pull_hash(hash),
        }
    }

    pub fn list(&self) -> Vec<String> {
        match self {
            Scope::None => vec![],
            Scope::Root(_, _) => vec![],
            Scope::Normal { parent, contents } => {
                let mut out: Vec<String> = contents.borrow().keys().cloned().collect();
                out.extend(parent.list());
                out
            }
            Scope::With { parent, contents } => {
                let eval = match contents.eval() {
                    Ok(x) => x,
                    Err(_) => return vec![],
                };
                let map = match eval.as_map() {
                    Ok(x) => x,
                    Err(_) => return vec![],
                };
                let mut out: Vec<String> = map.keys().cloned().collect();
                out.extend(parent.list());
                out
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<Gc<Tree>> {
        self.get_normal(name).or_else(|| self.get_with(name))
    }

    pub fn get_normal(&self, name: &str) -> Option<Gc<Tree>> {
        use rnix::types::Wrapper;
        match self {
            Scope::None | Scope::Root(_, _) => Some(Gc::new(Tree::from_concrete(match name {
                "true" => NixValue::Bool(true),
                "false" => NixValue::Bool(false),
                "null" => NixValue::Null,
                "derivation" => {
                    let source = include_str!("./derivation.nix");
                    let root = rnix::parse(&source).root().inner()?;
                    let tmp = Tree::parse(root, Gc::new(Scope::None)).ok()?.eval().ok()?;
                    let val: &NixValue = tmp.borrow();
                    val.clone()
                }
                "abort" => NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Abort)),
                "import" => NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Import)),
                "map" => NixValue::Lambda(NixLambda::Builtin(NixBuiltin::Map)),
                "toString" => NixValue::Lambda(NixLambda::Builtin(NixBuiltin::ToString)),
                "removeAttrs" => NixValue::Lambda(NixLambda::Builtin(NixBuiltin::RemoveAttrs)),
                "builtins" => crate::builtins::make_builtins_map(),
                _ => return None,
            }))),
            Scope::Normal { parent, contents } => match contents.borrow().get(name) {
                Some(x) => Some(x.clone()),
                None => parent.get_normal(name),
            },
            Scope::With { parent, .. } => parent.get_normal(name),
        }
    }

    fn get_with(&self, name: &str) -> Option<Gc<Tree>> {
        match self {
            Scope::None => None,
            Scope::Root(_, _) => None,
            Scope::Normal { parent, .. } => parent.get_with(name),
            Scope::With { parent, contents } => match contents.eval().ok()?.borrow() {
                NixValue::Map(map) => match map.get(name) {
                    Some(x) => Some(x.clone()),
                    None => parent.get_with(name),
                },
                _ => None,
            },
        }
    }
}
