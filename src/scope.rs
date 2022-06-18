use crate::eval::{Expr, ExprSource};
use crate::value::NixValue;
use gc::{Finalize, Gc, GcCell, Trace};
use std::collections::HashSet;
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
    /// Binding introduced as a function argument or function argument pattern
    FunctionArguments {
        parent: Gc<Scope>,
        /// names bound
        names: GcCell<HashSet<String>>,
    },
    None,
}

/// How a binding might be defined
#[derive(Debug, Clone)]
pub enum Definition {
    /// Might be either unbound or bound dynamically by `with`
    #[allow(dead_code)]
    PossiblyDynamic,
    /// Here is the expression that defines this variable
    Expression(Gc<Expr>),
    /// this variable is a formal argument of a function
    Argument,
    /// this variable is definitely not defined
    Unbound,
    /// builtins
    Ambient,
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
        match self.get_definition(name) {
            Definition::Expression(x) => Some(x),
            _ => None,
        }
    }

    /// how/if this name is defined
    pub fn get_definition(&self, name: &str) -> Definition {
        match self {
            Scope::None | Scope::Root(_) => Definition::Expression(Gc::new(Expr {
                range: None,
                value: GcCell::new(None),
                source: ExprSource::Literal {
                    // TODO: add more keys here, such as `builtins`
                    value: match name {
                        "true" => NixValue::Bool(true),
                        "false" => NixValue::Bool(false),
                        "null" => NixValue::Null,
                        // list obtained with `nix repl` tab completion
                        "abort"
                        | "__add"
                        | "__addErrorContext"
                        | "__all"
                        | "__any"
                        | "__appendContext"
                        | "__attrNames"
                        | "__attrValues"
                        | "baseNameOf"
                        | "__bitAnd"
                        | "__bitOr"
                        | "__bitXor"
                        | "builtins"
                        | "__catAttrs"
                        | "__ceil"
                        | "__compareVersions"
                        | "__concatLists"
                        | "__concatMap"
                        | "__concatStringsSep"
                        | "__currentSystem"
                        | "__currentTime"
                        | "__deepSeq"
                        | "derivation"
                        | "derivationStrict"
                        | "dirOf"
                        | "__div"
                        | "__elem"
                        | "__elemAt"
                        | "fetchGit"
                        | "fetchMercurial"
                        | "fetchTarball"
                        | "fetchTree"
                        | "__fetchurl"
                        | "__filter"
                        | "__filterSource"
                        | "__findFile"
                        | "__floor"
                        | "__foldl'"
                        | "__fromJSON"
                        | "fromTOML"
                        | "__functionArgs"
                        | "__genericClosure"
                        | "__genList"
                        | "__getAttr"
                        | "__getContext"
                        | "__getEnv"
                        | "__groupBy"
                        | "__hasAttr"
                        | "__hasContext"
                        | "__hashFile"
                        | "__hashString"
                        | "__head"
                        | "import"
                        | "__intersectAttrs"
                        | "__isAttrs"
                        | "__isBool"
                        | "__isFloat"
                        | "__isFunction"
                        | "__isInt"
                        | "__isList"
                        | "isNull"
                        | "__isPath"
                        | "__isString"
                        | "__langVersion"
                        | "__length"
                        | "__lessThan"
                        | "__listToAttrs"
                        | "map"
                        | "__mapAttrs"
                        | "__match"
                        | "__mul"
                        | "__nixPath"
                        | "__nixVersion"
                        | "__parseDrvName"
                        | "__partition"
                        | "__path"
                        | "__pathExists"
                        | "placeholder"
                        | "__readDir"
                        | "__readFile"
                        | "removeAttrs"
                        | "__replaceStrings"
                        | "scopedImport"
                        | "__seq"
                        | "__sort"
                        | "__split"
                        | "__splitVersion"
                        | "__storeDir"
                        | "__storePath"
                        | "__stringLength"
                        | "__sub"
                        | "__substring"
                        | "__tail"
                        | "throw"
                        | "__toFile"
                        | "__toJSON"
                        | "__toPath"
                        | "toString"
                        | "__toXML"
                        | "__tryEval"
                        | "__typeOf"
                        | "__unsafeDiscardOutputDependency"
                        | "__unsafeDiscardStringContext"
                        | "__unsafeGetAttrPos"
                        | "__zipAttrsWith" => return Definition::Ambient,
                        _ => return Definition::Unbound,
                    },
                },
                scope: Gc::new(Scope::None),
            })),
            Scope::Let { parent, contents } => match contents.borrow().get(name) {
                Some(x) => Definition::Expression(x.clone()),
                None => parent.get_definition(name),
            },
            Scope::FunctionArguments { parent, names } => match names.borrow().get(name) {
                Some(_) => Definition::Argument,
                None => parent.get_definition(name),
            },
        }
    }

    pub fn root_path(&self) -> Option<PathBuf> {
        match &self {
            Scope::None => None,
            Scope::Root(path) => Some(path.clone()),
            Scope::Let { parent, .. } => parent.root_path(),
            Scope::FunctionArguments { parent, .. } => parent.root_path(),
        }
    }
}
