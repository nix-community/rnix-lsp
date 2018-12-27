use crate::{App, utils::{self, Var}};
use rnix::{parser::*, types::*, value::Value as ParsedValue};
use std::{
    collections::{HashMap, hash_map::Entry},
    fs,
    io,
    rc::Rc
};
use url::Url;

impl<'a, W: io::Write> App<'a, W> {
    pub fn scope_for_ident<'b>(&mut self, file: Url, root: &'b Node, offset: usize)
        -> Option<(&'b Ident, HashMap<String, Var>)>
    {
        let mut file = Rc::new(file);
        let info = utils::ident_at(root, offset)?;
        let ident = info.ident;
        let mut entries = utils::scope_for(&file, ident.node());
        for var in info.path {
            let node = entries.get(&var)?.value.as_ref()?;
            entries = self.scope_from_node(&mut file, node)?;
        }
        Some((Ident::cast(ident.node()).unwrap(), entries))
    }
    pub fn scope_from_node(&mut self, file: &mut Rc<Url>, mut node: &Node)
        -> Option<HashMap<String, Var>>
    {
        let mut scope = HashMap::new();

        if let Some(entry) = SetEntry::cast(node) {
            node = entry.value();
        }

        let mut node = node.to_owned();

        // Resolve simple imports
        loop {
            let apply = match Apply::cast(&node) {
                None => break,
                Some(apply) => apply
            };
            if Ident::cast(apply.lambda()).map(|ident| ident.as_str() != "import").unwrap_or(true) {
                break;
            }
            let (_anchor, path) = match Value::cast(apply.value()) {
                None => break,
                Some(value) => match value.to_value() {
                    Ok(ParsedValue::Path(_anchor, path)) => (_anchor, path),
                    _ => break,
                }
            };

            // TODO use anchor
            *file = Rc::new(file.join(&path).ok()?);
            let path = utils::uri_path(&file)?;
            node = match self.files.entry(file.to_string()) {
                Entry::Occupied(entry) => {
                    let (ast, _code) = entry.get();
                    ast.root().inner().to_owned()
                },
                Entry::Vacant(placeholder) => {
                    let code = fs::read_to_string(&path).ok()?;
                    let ast = rnix::parse(&code);
                    let node = ast.root().inner().to_owned();
                    placeholder.insert((ast, code));
                    node
                }
            };
        }

        if let Some(set) = Set::cast(&node) {
            utils::populate(&file, &mut scope, set);
        }
        Some(scope)
    }
}
