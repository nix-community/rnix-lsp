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
    pub fn scope_for_ident(&mut self, file: Url, root: Node<rowan::OwnedRoot<Types>>, offset: usize)
        -> Option<(Ident<rowan::OwnedRoot<Types>>, HashMap<String, Var>)>
    {
        let mut file = Rc::new(file);
        let info = utils::ident_at(root.borrowed(), offset)?;
        let ident = info.ident;
        let mut entries = utils::scope_for(&file, *ident.node());
        for var in info.path {
            let node = entries.get(&var)?.value.as_ref()?.borrowed();
            entries = self.scope_from_node(&mut file, node.owned())?;
        }
        Some((Ident::cast(ident.node().owned()).unwrap(), entries))
    }
    pub fn scope_from_node(&mut self, file: &mut Rc<Url>, mut node: Node<rowan::OwnedRoot<Types>>)
        -> Option<HashMap<String, Var>>
    {
        let mut scope = HashMap::new();
        if let Some(entry) = SetEntry::cast(node.borrowed()) {
            node = entry.value().owned();
        }

        // Resolve simple imports
        loop {
            let apply = match Apply::cast(node.borrowed()) {
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
                    ast.root().inner().owned()
                },
                Entry::Vacant(placeholder) => {
                    let code = fs::read_to_string(&path).ok()?;
                    let ast = rnix::parse(&code);
                    let node = ast.root().inner().owned();
                    placeholder.insert((ast, code));
                    node
                }
            };
        }

        if let Some(set) = Set::cast(node.borrowed()) {
            utils::populate(&file, &mut scope, &set);
        }
        Some(scope)
    }
}
