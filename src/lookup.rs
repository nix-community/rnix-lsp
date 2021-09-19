use crate::{App, eval::Expr, utils::{self, Datatype, Var}};
use lsp_types::Url;
use rnix::{types::*, value::Value as ParsedValue, SyntaxNode};
use std::{
    collections::{hash_map::Entry, HashMap},
    fs,
    rc::Rc,
};

use lazy_static::lazy_static;

use std::{process, str};
use regex;
use gc::Gc;
use crate::scope::Scope;

lazy_static! {
    static ref BUILTINS: Vec<String> = vec![
      // `nix __dump-builtins | jq 'keys'
      "abort", "add", "all", "any", "attrNames", "attrValues", "baseNameOf", "bitAnd", "bitOr",
      "bitXor", "catAttrs", "compareVersions", "concatLists", "concatMap", "concatStringsSep", "deepSeq",
      "dirOf", "div", "elem", "elemAt", "fetchGit", "fetchTarball", "fetchurl", "filter", "filterSource", "foldl'",
      "fromJSON", "functionArgs", "genList", "getAttr", "getEnv", "hasAttr", "hashFile", "hashString", "head",
      "import", "intersectAttrs", "isAttrs", "isBool", "isFloat", "isFunction", "isInt", "isList", "isNull",
      "isPath", "isString", "length", "lessThan", "listToAttrs", "map", "mapAttrs", "match", "mul", "parseDrvName",
      "partition", "path", "pathExists", "placeholder", "readDir", "readFile", "removeAttrs", "replaceStrings",
      "seq", "sort", "split", "splitVersion", "storePath", "stringLength", "sub", "substring", "tail", "throw",
      "toFile", "toJSON", "toPath", "toString", "toXML", "trace", "tryEval", "typeOf"
    ].into_iter().map(String::from).collect::<Vec<_>>();
}

#[derive(Debug)]
pub struct LSPDetails {
    pub datatype: Datatype,
    pub var: Option<Var>,
    pub documentation: Option<String>,
    pub deprecated: bool,
    pub params: Option<String>,
}

impl LSPDetails {
    fn builtin_fallback() -> LSPDetails {
        LSPDetails {
            datatype: Datatype::Lambda,
            var: None,
            documentation: None,
            deprecated: false,
            params: None,
        }
    }

    fn builtin_with_doc(deprecated: bool, params: Option<String>, documentation: String) -> LSPDetails {
        LSPDetails {
            datatype: Datatype::Lambda,
            var: None,
            documentation: Some(documentation),
            deprecated,
            params,
        }
    }

    fn from_scope(datatype: Datatype, var: Var) -> LSPDetails {
        LSPDetails {
            datatype,
            var: Some(var),
            documentation: None,
            deprecated: false,
            params: None,
        }
    }

    pub fn render_detail(&self) -> String {
        match &self.params {
            None => self.datatype.to_string(),
            Some(params) => format!("{}: {} -> Result", self.datatype.to_string(), params),
        }
    }
}

impl App {
    pub fn scope_for_ident(
        &mut self,
        file: Url,
        root: &SyntaxNode,
        offset: usize,
    ) -> Option<(Ident, HashMap<String, LSPDetails>, String)> {

        let mut file = Rc::new(file);
        let info = utils::ident_at(&root, offset)?;
        let ident = info.ident;
        let mut entries = utils::scope_for(&file, ident.node().clone())?
            .into_iter()
            .map(|(x, var)| (x.to_owned(), LSPDetails::from_scope(var.datatype, var)))
            .collect::<HashMap<_, _>>();
        for var in info.path {
            if !entries.contains_key(&var) && var == "builtins" {
                entries = self.load_builtins();
            } else {
                let node_entry = entries.get(&var)?;
                if let Some(var) = &node_entry.var {
                    let node = var.value.clone()?;
                    entries = self
                        .scope_from_node(&mut file, node)?
                        .into_iter()
                        .map(|(x, var)| (x.to_owned(), LSPDetails::from_scope(var.datatype, var)))
                        .collect::<HashMap<_, _>>();
                }
            }
        }
        Some((
            Ident::cast(ident.node().clone()).unwrap(),
            entries,
            info.name,
        ))
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
                    let (ast, _code, _) = entry.get();
                    ast.root().inner()?.clone()
                }
                Entry::Vacant(placeholder) => {
                    let content = fs::read_to_string(&path).ok()?;
                    let ast = rnix::parse(&content);
                    let node = ast.root().inner()?.clone();
                    let gc_root = Gc::new(Scope::Root(path));
                    let evaluated = Expr::parse(node.clone(), gc_root);
                    placeholder.insert((ast, content, evaluated));
                    node
                }
            };
        }

        if let Some(set) = AttrSet::cast(node) {
            utils::populate(&file, &mut scope, &set, Datatype::Attribute);
        }
        Some(scope)
    }

    fn fallback_builtins(&self, list: Vec<String>) -> HashMap<String, LSPDetails> {
        list.into_iter().map(|x| (x, LSPDetails::builtin_fallback())).collect::<HashMap<_, _>>()
    }

    fn load_builtins(&self) -> HashMap<String, LSPDetails> {
        let nixver = process::Command::new("nix").args(&["--version"]).output();

        // `nix __dump-builtins` is only supported on `nixUnstable` a.k.a. Nix 2.4.
        // Thus, we have to check if this is actually available. If not, `rnix-lsp` will fall
        // back to a hard-coded list of builtins which is missing additional info such as documentation
        // or parameter names though.
        match nixver {
            Ok(out) => {
                match str::from_utf8(&out.stdout) {
                    Ok(v) => {
                        let re = regex::Regex::new(r"^nix \(Nix\) (?P<major>\d)\.(?P<minor>\d).*").unwrap();
                        let m = re.captures(v).unwrap();
                        let major = m.name("major").map_or(1, |m| m.as_str().parse::<u8>().unwrap());
                        let minor = m.name("minor").map_or(1, |m| m.as_str().parse::<u8>().unwrap());
                        if major == 2 && minor >= 4 || major > 2 {
                            let builtins_raw = process::Command::new("nix").args(&["__dump-builtins"]).output().unwrap();
                            let v: serde_json::Value = serde_json::from_str(str::from_utf8(&builtins_raw.stdout).unwrap()).unwrap();

                            v.as_object().unwrap()
                                .iter().map(|(x, v)| {
                                    let doc = String::from(v["doc"].as_str().unwrap());
                                    (String::from(x), LSPDetails::builtin_with_doc(
                                        doc.starts_with("**DEPRECATED.**"),
                                        // FIXME make sure that `lib.flip` is taken into account here
                                        v["args"].as_array().map(|x| x.iter().map(|y| y.as_str().unwrap()).collect::<Vec<_>>().join(" -> ")),
                                        doc
                                    ))
                                })
                                .collect::<HashMap<_, _>>()
                        } else {
                            self.fallback_builtins(BUILTINS.to_vec())
                        }
                    },
                    Err(_) => self.fallback_builtins(BUILTINS.to_vec()),
                }
            },
            Err(_) => self.fallback_builtins(BUILTINS.to_vec()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_server::Connection;

    #[test]
    fn test_suggest_let_in_variable() {
        let expr = "let ab = 1; in a";
        let root = rnix::parse(expr).node();
        let c = Connection::memory();
        let suggestions = (App {
            files: HashMap::new(),
            conn: c.0,
        }).scope_for_ident(
            Url::parse("file:///default.nix").unwrap(),
            &root,
            15
        );

        assert!(suggestions.is_some());
        let val = suggestions.unwrap();
        assert_eq!("a", val.2);
        assert!(val.1.contains_key("ab"));
        assert_eq!(Datatype::Variable, val.1.get("ab").unwrap().datatype);
    }

    #[test]
    fn test_find_inherit_scope() {
        let expr = "let ab = { abc = 1; }; in { inherit (ab) abc; }";
        let root = rnix::parse(expr).node();
        let mut app = App {
            files: HashMap::new(),
            conn: Connection::memory().0,
        };
        let suggestions = app.scope_for_ident(
            Url::parse("file:///default.nix").unwrap(),
            &root,
            37
        );

        assert!(suggestions.is_some());
        let val = suggestions.unwrap();
        assert!(val.1.contains_key("ab"));

        let suggestions_attr_set = app.scope_for_ident(
            Url::parse("file:///default.nix").unwrap(),
            &root,
            41
        );
        assert!(suggestions_attr_set.is_some());
        let val = suggestions_attr_set.unwrap();
        assert!(val.1.contains_key("abc"));
    }

    #[test]
    fn test_ident_traverse_attr_path() {
        let root = rnix::parse("let ab = { cd = 2; }; in ab. ").node();
        let mut app = App {
            files: HashMap::new(),
            conn: Connection::memory().0,
        };

        let suggestions = app.scope_for_ident(
            Url::parse("file:///default.nix").unwrap(),
            &root,
            28
        );

        assert!(suggestions.is_some());
        let val = suggestions.unwrap();
        assert!(val.1.contains_key("cd"));
    }

    #[test]
    fn test_provide_builtins() {
        let root = rnix::parse("builtins.map (y: y)").node();
        let mut app = App {
            files: HashMap::new(),
            conn: Connection::memory().0,
        };

        let suggestions = app.scope_for_ident(
            Url::parse("file:///default.nix").unwrap(),
            &root,
            9
        );

        assert!(suggestions.is_some());
        let val = suggestions.unwrap();
        assert!(val.1.contains_key("abort"));
        assert!(val.1.contains_key("trace"));

        assert!(val.1.get("abort").unwrap().documentation.is_some());
    }

    #[test]
    fn test_builtin_fallback() {
        let builtin = LSPDetails::builtin_fallback();
        assert_eq!(Datatype::Lambda, builtin.datatype);
        assert!(builtin.documentation.is_none());
    }

    #[test]
    fn test_builtin_from_dump_builtins() {
        let builtin = LSPDetails::builtin_with_doc(
            false,
            Some(String::from("from -> to")),
            String::from("foo")
        );
        assert_eq!(Datatype::Lambda, builtin.datatype);
        assert_eq!("Lambda: from -> to -> Result", builtin.render_detail());
    }
}
