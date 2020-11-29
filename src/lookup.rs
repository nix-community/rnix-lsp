use crate::{
    utils::{self, Var},
    App,
};
use lsp_types::Url;
use rnix::{types::*, value::Value as ParsedValue, SyntaxNode};
use std::{
    collections::{hash_map::Entry, HashMap},
    fs,
    rc::Rc,
};

use lazy_static::lazy_static;

// FIXME use Nix bindings to dynamically extract existing builtins.
// e.g. use API behind `nix __dump-builtins`.
lazy_static! {
    static ref BUILTINS: Vec<String> = vec![
        "map" , "filter", "fromTOML", "fromJSON", "toJSON", "match", "concatStringsSep",
        "concatLists", "fetchGit", "fetchTarball", "fetchurl", "findFile", "abort", "add"
    ].into_iter().map(String::from).collect::<Vec<_>>();
}

impl App {
    pub fn scope_for_ident(
        &mut self,
        file: Url,
        root: &SyntaxNode,
        offset: usize,
    ) -> Option<(Ident, HashMap<String, (String, Option<Var>)>)> {
        let mut file = Rc::new(file);
        let info = utils::ident_at(&root, offset)?;
        let ident = info.ident;
        let mut entries = utils::scope_for(&file, ident.node().clone())?.into_iter()
            .map(|(x, var)| (x.to_owned(), (var.datatype.clone(), Some(var))))
            .collect::<HashMap<_, _>>();
        for var in info.path {
            if !entries.contains_key(&var) && var == "builtins" {
                entries = BUILTINS.iter()
                    .map(|x| (x.to_owned(), (String::from("Lambda"), None)))
                    .collect::<HashMap<_, _>>();
            } else {
                let node_entry = entries.get(&var)?;
                if let (_, Some(var)) = node_entry {
                    let node = var.value.clone()?;
                    entries = self.scope_from_node(&mut file, node)?.into_iter()
                        .map(|(x, var)| (x.to_owned(), (var.datatype.clone(), Some(var))))
                        .collect::<HashMap<_, _>>();
                }
            }
        }
        Some((Ident::cast(ident.node().clone()).unwrap(), entries))
    }
    pub fn scope_from_node(
        &mut self,
        file: &mut Rc<Url>,
        mut node: SyntaxNode,
    ) -> Option<HashMap<String, Var>> {
        let mut scope = HashMap::new();

        if let Some(entry) = KeyValue::cast(node.clone()) {
            node = entry.value()?;
        }

        // Resolve simple imports
        loop {
            let apply = match Apply::cast(node.clone()) {
                None => break,
                Some(apply) => apply,
            };
            if Ident::cast(apply.lambda()?).map_or(true, |ident| ident.as_str() != "import") {
                break;
            }
            let (_anchor, path) = match Value::cast(apply.value()?) {
                None => break,
                Some(value) => match value.to_value() {
                    Ok(ParsedValue::Path(anchor, path)) => (anchor, path),
                    _ => break,
                },
            };

            // TODO use anchor
            *file = Rc::new(file.join(&path).ok()?);
            let path = utils::uri_path(&file)?;
            node = match self.files.entry((**file).clone()) {
                Entry::Occupied(entry) => {
                    let (ast, _code) = entry.get();
                    ast.root().inner()?.clone()
                }
                Entry::Vacant(placeholder) => {
                    let content = fs::read_to_string(&path).ok()?;
                    let ast = rnix::parse(&content);
                    let node = ast.root().inner()?.clone();
                    placeholder.insert((ast, content));
                    node
                }
            };
        }

        if let Some(set) = AttrSet::cast(node) {
            utils::populate(&file, &mut scope, &set, String::from("Attribute"));
        }
        Some(scope)
    }
}
