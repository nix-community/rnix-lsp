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

impl App {
    pub fn scope_for_ident(
        &mut self,
        file: Url,
        root: &SyntaxNode,
        offset: usize,
    ) -> Option<(Ident, HashMap<String, Var>)> {
        let mut file = Rc::new(file);
        let info = utils::ident_at(&root, offset)?;
        let ident = info.ident;
        let mut entries = utils::scope_for(&file, ident.node().clone())?;
        for var in info.path {
            let node = entries.get(&var)?.value.clone()?;
            entries = self.scope_from_node(&mut file, node)?;
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
            utils::populate(&file, &mut scope, &set);
        }
        Some(scope)
    }
}
