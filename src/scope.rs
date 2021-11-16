use crate::eval::{Expr, ExprSource};
use crate::value::NixValue;
use gc::{Finalize, Gc, GcCell, Trace};
use std::{collections::HashMap, path::PathBuf};

/// A parent Expr's scope is used to provide tooling for its child Exprs.
/// This enum would provide four scope types:
/// - None: Used for calculated literals. For example, for the string
///   interpolation `"prefix${suffix}"`, the literal `prefix` by itself
///   cannot be referenced anywhere except at its definition, so we don't
///   need context-aware tooling for it. For `${suffix}`, however, we would
///   inherit the parent's scope.
/// - Root: Provided for each file. Used for providing global variables
///   and tracking the `import` dependency tree. Also used for detecting
///   which file an expression is defined in.
/// - Let: Created by `let in` and `rec { }`. All the variable names
///   can be derived using static analysis with rnix-parser; we don't need
///   to evaluate anything to detect if a variable name is in this scope
/// - With: Created by `with $VAR` expressions. We need to evaluate $VAR
///   to determine whether a variable name is in scope.
#[derive(Trace, Finalize)]
pub enum Scope {
    Root(PathBuf),
    Let {
        parent: Gc<Scope>,
        contents: GcCell<HashMap<String, Gc<Expr>>>,
    },
    None,
}

impl Scope {
    /// Finds the Expr of an identifier in the scope.
    ///
    /// This would do two passes up the tree:
    /// 1. Check Scope::Normal and Scope::Root
    /// 2. Check Scope::With, which requires evaluation
    ///
    /// See https://github.com/NixOS/nix/issues/490 for an explanation
    /// of why Nix works this way.
    ///
    /// Examples:
    /// ```plain
    /// nix-repl> let import = 1; in import
    /// 1 # found in Scope::Normal, which we reach before Scope::Root
    /// nix-repl> import
    /// «primop» # found in Scope::Root
    /// nix-repl> with { import = 1; }; import
    /// «primop» # found in Scope::Root, which we reach before Scope::With
    /// ```
    pub fn get(&self, name: &str) -> Option<Gc<Expr>> {
        self.get_let(name) // we haven't yet implemented fallback to a future get_with method
    }

    pub fn get_let(&self, name: &str) -> Option<Gc<Expr>> {
        match self {
            Scope::None | Scope::Root(_) => Some(Gc::new(Expr {
                range: None,
                value: GcCell::new(None),
                source: ExprSource::Literal {
                    // TODO: add more keys here, such as `builtins`
                    value: match name {
                        "true" => NixValue::Bool(true),
                        "false" => NixValue::Bool(false),
                        "null" => NixValue::Null,
                        _ => return None,
                    },
                },
                scope: Gc::new(Scope::None),
            })),
            Scope::Let { parent, contents } => match contents.borrow().get(name) {
                Some(x) => Some(x.clone()),
                None => parent.get_let(name),
            },
        }
    }

    /// List all variable names in a given scope
    pub fn list(&self) -> Vec<String> {
        match self {
            Scope::None => vec![],
            Scope::Root(..) => vec![],
            Scope::Let { parent, contents } => {
                let mut out: Vec<String> = contents.borrow().keys().cloned().collect();
                out.extend(parent.list());
                out
            }
        }
    }

    pub fn root_path(&self) -> Option<PathBuf> {
        match &self {
            Scope::None => None,
            Scope::Root(path) => Some(path.clone()),
            Scope::Let { parent, .. } => parent.root_path(),
        }
    }
}
