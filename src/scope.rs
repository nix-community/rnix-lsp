use crate::eval::Tree;
use gc::{Finalize, Gc, Trace};
use std::path::PathBuf;

/// A parent Tree's scope is used to provide tooling for its child Trees.
/// This enum would provide four scope types:
/// - None: Used for calculated literals. For example, for the string
///   interpolation `"prefix${suffix}"`, the literal `prefix` by itself
///   cannot be referenced anywhere except at its definition, so we don't
///   need context-aware tooling for it. For `${suffix}`, however, we would
///   inherit the parent's scope.
/// - Root: Provided for each file. Used for providing global variables
///   and tracking the `import` dependency tree. Also used for detecting
///   which file an expression is defined in.
/// - Normal: Created by `let in` and `rec { }`. All the variable names
///   can be derived using static analysis with rnix-parser; we don't need
///   to evaluate anything to detect if a variable name is in this scope
/// - With: Created by `with $VAR` expressions. We need to evaluate $VAR
///   to determine whether a variable name is in scope.
#[derive(Trace, Finalize)]
pub enum Scope {
    Root(PathBuf),
}

impl Scope {
    /// Finds the Tree of an identifier in the scope.
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
    #[allow(dead_code)] // this function will be implemented later
    pub fn get(&self, _name: &str) -> Option<Gc<Tree>> {
        None
    }
}
