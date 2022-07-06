use eval::Expr;
use gc::Gc;
use rnix::types::Wrapper;
use scope::Scope;
use std::borrow::Borrow;
use value::NixValue;
use crate::static_analysis;

#[cfg(test)]
use maplit::hashmap;
#[cfg(test)]
use serde_json::json;
#[cfg(test)]
use std::time::Duration;
#[cfg(test)]
use stoppable_thread::*;

#[allow(dead_code)]
/// Evaluates a nix code snippet
fn eval(code: &str) -> NixValue {
    let ast = rnix::parse(&code);
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let out = Expr::parse(root, Gc::new(Scope::Root(path))).unwrap();
    assert_eq!(static_analysis::check(&out), Vec::new());
    let tmp = out.eval();
    let val: &NixValue = tmp.as_ref().unwrap().borrow();
    val.clone()
}

#[allow(dead_code)]
/// Returns the errors found by static_analysis::check.
///
/// As dealing with ranges is cumbersome, returns a map "text matched by the range" => "error text"
fn static_analysis(code: &str) -> HashMap<&str, String> {
    let ast = rnix::parse(&code);
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let out = Expr::parse(root, Gc::new(Scope::Root(path))).unwrap();
    let errors = static_analysis::check(&out);
    let mut res = HashMap::new();
    for error in errors {
        let range = error.range.start().into()..error.range.end().into();
        let text = code.get(range).unwrap();
        res.insert(text, error.kind.to_string());
    }
    res
}

use super::*;

#[test]
fn integer_division() {
    let code = "1 / 2";
    assert_eq!(eval(code).as_int().unwrap(), 0);
}

#[test]
fn float_division() {
    let code = "1.0 / 2.0";
    assert_eq!(eval(code).as_float().unwrap(), 0.5);
}

#[test]
fn order_of_operations() {
    let code = "1 + 2 * 3";
    assert_eq!(eval(code).as_int().unwrap(), 7);
}

#[test]
fn div_int_by_float() {
    let code = "1 / 2.0";
    assert_eq!(eval(code).as_float().unwrap(), 0.5);
}

#[test]
fn unbound_simple() {
    let code = "1+x";
    assert_eq!(static_analysis(code), hashmap!{ "x" => "identifier x is unbound".into() });
}

#[test]
fn with_conservative_binding_analysis() {
    let code = "1 + with import <nixpkgs> {}; some_attr";
    assert_eq!(static_analysis(code), hashmap!{});
}

#[test]
fn binding_analysis_ignores_attrset_selection() {
    let code = "1 + rec { x = 1; y = x; }.y";
    assert_eq!(static_analysis(code), hashmap!{});
}

#[test]
fn unbound_attrset() {
    let code = "1 + rec { x = 1; y = x; z = t; }.y";
    assert_eq!(static_analysis(code), hashmap!{"t" => "identifier t is unbound".into()});
}

#[cfg(test)]
fn prepare_integration_test(code: &str, filename: &str) -> (Connection, StoppableHandle<()>) {
    let (server, client) = Connection::memory();

    // Manually handle LSP communications here. This is needed in order to not wait
    // indefinetely for a message to be able to exit as soon as the test is finished
    // and the thread is stopped.
    let h = spawn(move |stopped| {
        let mut app = App { files: HashMap::new(), conn: server };

        loop {
            if let Ok(msg) = app.conn.receiver.recv_timeout(Duration::from_millis(100)) {
                match msg {
                    Message::Request(req) => app.handle_request(req),
                    Message::Notification(notification) => {
                        let _ = app.handle_notification(notification);
                    }
                    Message::Response(_) => (),
                }
            }
            if stopped.get() {
                break;
            }
        }
    });

    let open = Notification {
        method: String::from("textDocument/didOpen"),
        params: json!({
            "textDocument": { "uri": filename, "text": code, "version": 1, "languageId": "nix" }
        })
    };
    client.sender.send(open.into()).expect("Cannot send didOpen!");

    (client, h)
}

#[cfg(test)]
fn recv_msg(client: &Connection) -> lsp_server::Message {
    client.receiver.recv_timeout(Duration::new(5, 0)).expect("No message within 5 secs!")
}

#[cfg(test)]
fn expect_diagnostics(client: &Connection) {
    let notf = recv_msg(client);
    if let Message::Notification(x) = notf {
        assert_eq!("textDocument/publishDiagnostics", x.method);
    } else {
        panic!("Expected diagnostics notification!");
    }
}

#[cfg(test)]
fn coerce_response(msg: lsp_server::Message) -> lsp_server::Response {
    if let Message::Response(x) = msg {
        x
    } else {
        panic!("Expected LSP message to be a response!");
    }
}

#[test]
fn test_hover_integration() {
    // Since we transmit content via `textDocument/didOpen`, we can
    // use made-up names for paths here that don't need to exist anywhere.
    let urlpath = "file:///code/default.nix";
    let (client, handle) = prepare_integration_test("(1 + 1)", urlpath);

    let r = Request {
        id: RequestId::from(23),
        method: String::from("textDocument/hover"),
        params: json!({
            "textDocument": {
                "uri": "file:///code/default.nix",
            },
            "position": {
                "line": 0,
                "character": 7
            }
        })
    };
    client.sender.send(r.into()).expect("Cannot send hover notification!");

    expect_diagnostics(&client);

    let msg = recv_msg(&client);
    let hover_json = coerce_response(msg).result.expect("Expected hover response!");
    let hover_value = &hover_json.as_object().unwrap()["contents"]["value"];
    assert_eq!("2", *hover_value.to_string().split("\\n").collect::<Vec<_>>().get(1).unwrap());

    handle.stop().join().expect("Failed to gracefully terminate LSP worker thread!");
}

#[test]
fn test_rename() {
    let urlpath = "file:///code/default.nix";
    let (client, handle) = prepare_integration_test("let a = { b = a; }; in a", urlpath);

    let r = Request {
        id: RequestId::from(23),
        method: String::from("textDocument/rename"),
        params: json!({
            "textDocument": {
                "uri": urlpath
            },
            "position": {
                "line": 0,
                "character": 24,
            },
            "newName": "c",
        })
    };
    client.sender.send(r.into()).expect("Cannot send rename request!");

    expect_diagnostics(&client);
    let msg = recv_msg(&client);

    let response = coerce_response(msg).result.expect("Expected rename response!");
    let changes = &response
        .as_object()
        .unwrap()["changes"]["file:///code/default.nix"]
        .as_array()
        .expect("Changes must be an array!");

    // `let a`, `{ b = a; }`, `in a` is where `a` should be replaced with `c`.
    assert_eq!(3, changes.len());

    let first = changes
        .get(0)
        .expect("Array should have three elements!")
        .as_object()
        .expect("Changes should be objects!");

    assert_eq!("c", first["newText"]);
    assert_eq!(4, first["range"]["start"]["character"]);
    assert_eq!(5, first["range"]["end"]["character"]);
    assert_eq!(0, first["range"]["start"]["line"]);
    assert_eq!(0, first["range"]["end"]["line"]);

    let second = changes
        .get(1)
        .expect("Array should have three elements!")
        .as_object()
        .expect("Changes should be objects!");

    assert_eq!("c", second["newText"]);
    assert_eq!(14, second["range"]["start"]["character"]);
    assert_eq!(15, second["range"]["end"]["character"]);
    assert_eq!(0, second["range"]["start"]["line"]);
    assert_eq!(0, second["range"]["end"]["line"]);

    let third = changes
        .get(2)
        .expect("Array should have three elements!")
        .as_object()
        .expect("Changes should be objects!");

    assert_eq!("c", third["newText"]);
    assert_eq!(23, third["range"]["start"]["character"]);
    assert_eq!(24, third["range"]["end"]["character"]);
    assert_eq!(0, third["range"]["start"]["line"]);
    assert_eq!(0, third["range"]["end"]["line"]);

    handle.stop().join().expect("Failed to gracefully terminate LSP worker thread!");
}

#[test]
fn test_reformat_integration() {
    let urlpath = "file:///code/default.nix";
    let input = r#"{
  f = { x
  , y
      }: body;

  testAllTrue = expr: {inherit expr;expected=map (x: true) expr; };
}
"#;
    let (client, handle) = prepare_integration_test(input, urlpath);

    let r = Request {
        id: RequestId::from(23),
        method: String::from("textDocument/formatting"),
        params: json!({
            "textDocument": {
                "uri": "file:///code/default.nix",
            },
            "options": {
                // Tab size isn't respected yet
                "tabSize": 37,
                "insertSpaces": true
            }
        })
    };
    client.sender.send(r.into()).expect("Cannot send reformat request!");

    expect_diagnostics(&client);

    let msg = recv_msg(&client);
    let hover_json = coerce_response(msg).result.expect("Expected reformat response!");
    let edits = hover_json;
    let expected = json!([
        {
            "newText": "\n    ",
            "range": {
                "start": {
                    "character": 5,
                    "line": 1
                },
                "end": {
                    "character": 6,
                    "line": 1
                }
            }
        },
        {
            "newText": "\n    ",
            "range": {
                "start": {
                    "character": 9,
                    "line": 1
                },
                "end": {
                    "character": 2,
                    "line": 2
                }
            }
        },
        {
            "newText": "\n    ",
            "range": {
                "start": {
                    "character": 5,
                    "line": 2
                },
                "end": {
                    "character": 6,
                    "line": 3
                }
            }
        },
        {
            "newText": " ",
            "range": {
                "start": {
                    "character": 23,
                    "line": 5
                },
                "end": {
                    "character": 23,
                    "line": 5
                }
            }
        },
        {
            "newText": " ",
            "range": {
                "start": {
                    "character": 36,
                    "line": 5
                },
                "end": {
                    "character": 36,
                    "line": 5
                }
            }
        },
        {
            "newText": " ",
            "range": {
                "start": {
                    "character": 44,
                    "line": 5
                },
                "end": {
                    "character": 44,
                    "line": 5
                }
            }
        },
        {
            "newText": " ",
            "range": {
                "start": {
                    "character": 45,
                    "line": 5
                },
                "end": {
                    "character": 45,
                    "line": 5
                }
            }
        }
    ]);
    assert_eq!(edits, expected);

    handle.stop().join().expect("Failed to gracefully terminate LSP worker thread!");
}

#[test]
fn attrs_simple() {
    let code = "{ x = 1; y = 2; }.x";
    assert_eq!(eval(code).as_int().unwrap(), 1);
}

#[test]
fn attrs_path() {
    let code = "{ x.y.z = 3; }.x.y.z";
    assert_eq!(eval(code).as_int().unwrap(), 3);
}

#[test]
fn attrs_rec() {
    let code = "rec { x = 4; y = x; }.y";
    assert_eq!(eval(code).as_int().unwrap(), 4);
}

#[test]
fn attrs_rec_nested() {
    let code = "rec { x = { b = 1; }; y = x; }.y.b";
    assert_eq!(eval(code).as_int().unwrap(), 1);
}

#[test]
fn attrs_merge() {
    let code = "{ a = { b = 1; }; a.c = 2; }".to_string();
    assert_eq!(eval(&format!("{}.a.b", code)).as_int().unwrap(), 1);
    assert_eq!(eval(&format!("{}.a.c", code)).as_int().unwrap(), 2);
}

#[test]
fn attrs_merge_conflict() {
    let ast = rnix::parse("{ a = { b = 1; c = 3; }; a.c = 2; }");
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let parse_result = Expr::parse(root, Gc::new(Scope::Root(path)));
    assert!(parse_result.is_err());
}

#[test]
fn attrs_merge_conflict_rec() {
    let ast = rnix::parse("rec { x = { b = 1; }; a = x; a.c = 2; }");
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let parse_result = Expr::parse(root, Gc::new(Scope::Root(path)));
    assert!(parse_result.is_err());
}

#[test]
fn attrs_merge_conflict_inherit() {
    let ast = rnix::parse("{ inherit ({ a = { b = 1; }; }) a; a.c = 2; }");
    let root = ast.root().inner().unwrap();
    let path = std::env::current_dir().unwrap();
    let parse_result = Expr::parse(root, Gc::new(Scope::Root(path)));
    assert!(parse_result.is_err());
}

#[test]
fn attrs_inherit_from() {
    let code = "{ inherit ({ b = 1; }) b; }.b";
    assert_eq!(eval(code).as_int().unwrap(), 1);
}
