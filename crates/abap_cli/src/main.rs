//! ABAP tooling CLI: rustc-style diagnostics on stderr when something fails; human mode is silent on success (`--json` always emits structured output).
//!
//! ```text
//! abap-cli lex [--json] [--errors-only] [FILE]
//! abap-cli parse [--json] [--ast] [--errors-only] [FILE]
//! abap-cli symbols [--json] [--unknown-only] [FILE]
//! abap-cli check [--json] [FILE]
//! ```
//!
//! `FILE` is UTF-8 ABAP source; omit or use `-` for stdin.

mod human;

use std::io::Read;
use std::ops::Range;
use std::path::Path;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_lexer::tokenize;
use abap_parser::parse;
use abap_symbols::{DiagnosticKind, analyze_unit};
use serde_json::{json, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    Lex,
    Parse,
    Symbols,
    Check,
}

struct Cli {
    command: Command,
    /// Emit JSON (legacy / tooling). Default is human-readable output.
    json_output: bool,
    /// Lex: only emit diagnostics. Parse: explicit errors-only (default already errors-only in JSON).
    errors_only: bool,
    /// Parse: include `ast` in JSON output, or print tree outline in human mode.
    parse_show_ast: bool,
    /// Only emit unresolved names (placeholder until resolution exists).
    unknown_only: bool,
    /// Source path, or None / `-` for stdin.
    path: Option<String>,
}

fn usage() -> String {
    r#"ABAP tooling CLI — human-readable diagnostics by default; add --json for structured output.

Usage:
  abap-cli [--json] lex [--errors-only] [FILE]
  abap-cli [--json] parse [--ast] [--errors-only] [FILE]
  abap-cli [--json] symbols [--unknown-only] [FILE]
  abap-cli [--json] check [FILE]

If FILE is omitted or `-`, read source from stdin.

Commands:
  lex       Tokenize (`--json` for tokens on a clean run; human only prints a token list when lexing failed)
  parse     Parser diagnostics (silent human run when clean); `--json --ast` for a syntax tree
  symbols   `--json` for identifier index; human is silent when clean, otherwise diagnostics and a symbol table
  check     Front-end diagnostics only (human silent when clean)

Options:
  --json          Print JSON to stdout (no rustc-style rendering)
  --ast           Parse: include `ast` in JSON output (ignored in human mode; use `--json --ast`)
  --errors-only   Lex: only errors. Parse (JSON): same as default without --ast
  --unknown-only  Symbols: only unknown / unresolved identifiers (empty until wired)

  -h, --help      Show this help
"#
    .to_string()
}

fn parse_cli_args(it: impl Iterator<Item = String>) -> Result<Cli, String> {
    let args: Vec<String> = it.collect();
    if args.is_empty() {
        return Err(usage());
    }

    let mut json_output = false;
    let mut rest = Vec::new();
    for arg in args {
        match arg.as_str() {
            "--json" => json_output = true,
            "-h" | "--help" | "help" => return Err(usage()),
            _ => rest.push(arg),
        }
    }

    let mut it = rest.into_iter();
    let cmd = it.next().ok_or_else(usage)?;
    let command = match cmd.as_str() {
        "lex" => Command::Lex,
        "parse" => Command::Parse,
        "symbols" => Command::Symbols,
        "check" => Command::Check,
        _ => return Err(format!("unknown command {:?}\n{}", cmd, usage())),
    };

    let mut errors_only = false;
    let mut parse_show_ast = false;
    let mut unknown_only = false;
    let mut path: Option<String> = None;

    for arg in it {
        match arg.as_str() {
            "-h" | "--help" => return Err(usage()),
            "--ast" => parse_show_ast = true,
            "--errors-only" => errors_only = true,
            "--unknown-only" => unknown_only = true,
            "--json" => json_output = true,
            "-" => path = Some(arg),
            s if !s.starts_with('-') => {
                if path.is_some() {
                    return Err(format!(
                        "unexpected extra argument {:?}\n{}",
                        s,
                        usage()
                    ));
                }
                path = Some(s.to_string());
            }
            other => return Err(format!("unknown option {other:?}\n{}", usage())),
        }
    }

    if errors_only && !matches!(command, Command::Lex | Command::Parse) {
        return Err(format!(
            "--errors-only only applies to lex and parse\n{}",
            usage()
        ));
    }
    if unknown_only && command != Command::Symbols {
        return Err(format!(
            "--unknown-only only applies to symbols\n{}",
            usage()
        ));
    }
    if parse_show_ast && !matches!(command, Command::Parse) {
        return Err(format!("--ast only applies to parse\n{}", usage()));
    }
    if matches!(command, Command::Parse) && parse_show_ast && errors_only {
        return Err(format!(
            "--ast and --errors-only cannot be used together on parse\n{}",
            usage()
        ));
    }

    Ok(Cli {
        command,
        json_output,
        errors_only,
        parse_show_ast,
        unknown_only,
        path,
    })
}

fn display_path(path: Option<&str>) -> String {
    match path {
        None | Some("-") => "<stdin>".to_string(),
        Some(p) => p.to_string(),
    }
}

fn read_source(path: Option<&str>) -> Result<String, String> {
    match path {
        None | Some("-") => {
            let mut buf = String::new();
            std::io::stdin()
                .read_to_string(&mut buf)
                .map_err(|e| format!("stdin: {e}"))?;
            Ok(buf)
        }
        Some(p) => std::fs::read_to_string(p).map_err(|e| format!("{}: {e}", Path::new(p).display())),
    }
}

fn human_exit(json_output: bool, has_errors: bool) -> i32 {
    if json_output {
        0
    } else if has_errors {
        1
    } else {
        0
    }
}

fn token_json(source: &str, token: &abap_lexer::Token) -> Value {
    json!({
        "kind": token.kind.as_str(),
        "range": [token.range.start, token.range.end],
        "lexeme": token.lexeme(source),
    })
}

fn ast_node_json(tree: &abap_ast::File, id: NodeId, source: &str) -> Value {
    let kind = tree.kind(id);
    let range = tree.range(id);
    let mut obj = serde_json::Map::new();
    obj.insert("kind".to_string(), json!(kind.as_str()));
    obj.insert(
        "id".to_string(),
        json!(id.0),
    );
    obj.insert(
        "range".to_string(),
        json!([range.start, range.end]),
    );

    match kind {
        SyntaxKind::Token | SyntaxKind::ExprIdent | SyntaxKind::ExprLiteral => {
            if let Some(slice) = source.get(range.clone()) {
                obj.insert("lexeme".to_string(), json!(slice));
            }
        }
        _ => {}
    }

    let children: Vec<NodeId> = tree.children(id).collect();
    if !children.is_empty() {
        let ch: Vec<Value> = children
            .iter()
            .map(|&c| ast_node_json(tree, c, source))
            .collect();
        obj.insert("children".to_string(), json!(ch));
    }

    Value::Object(obj)
}

fn parse_errors_to_diags(errors: &[abap_parser::ParseError]) -> Vec<human::Diagnostic<'_>> {
    errors
        .iter()
        .map(|e| human::Diagnostic {
            message: e.message.as_str(),
            range: e.range.clone(),
        })
        .collect()
}

fn main() {
    match run() {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn run() -> Result<i32, String> {
    let cli = parse_cli_args(std::env::args().skip(1)).map_err(|e| e)?;

    let source = read_source(cli.path.as_deref())?;
    let file_label = display_path(cli.path.as_deref());

    match cli.command {
        Command::Lex => {
            let abap_lexer::TokenizeResult { tokens, errors } = tokenize(&source);
            if cli.json_output {
                let err_val: Vec<Value> = errors
                    .iter()
                    .map(|e| {
                        json!({
                            "phase": "lex",
                            "range": [e.range.start, e.range.end],
                            "message": e.message,
                        })
                    })
                    .collect();
                let out = if cli.errors_only {
                    json!({ "phase": "lex", "errors": err_val })
                } else {
                    let token_vals: Vec<Value> =
                        tokens.iter().map(|t| token_json(&source, t)).collect();
                    json!({
                        "phase": "lex",
                        "tokens": token_vals,
                        "errors": err_val,
                    })
                };
                println!(
                    "{}",
                    serde_json::to_string_pretty(&out).map_err(|e| e.to_string())?
                );
                return Ok(0);
            }

            let diags: Vec<human::Diagnostic> = errors
                .iter()
                .map(|e| human::Diagnostic {
                    message: e.message,
                    range: e.range.clone(),
                })
                .collect();
            human::write_diagnostics(&diags, &source, &file_label).map_err(|e| e.to_string())?;

            if cli.errors_only {
                return Ok(human_exit(cli.json_output, !errors.is_empty()));
            }

            if !errors.is_empty() {
                human::write_token_list(&source, &tokens, human::stdout_color_enabled())
                    .map_err(|e| e.to_string())?;
            }
            Ok(human_exit(cli.json_output, !errors.is_empty()))
        }
        Command::Parse => {
            let parsed = parse(&source);
            let err_val: Vec<Value> = parsed
                .errors
                .iter()
                .map(|e| {
                    json!({
                        "phase": "parse",
                        "range": [e.range.start, e.range.end],
                        "message": e.message,
                    })
                })
                .collect();

            if cli.json_output {
                let out = if cli.parse_show_ast {
                    let root = parsed.file.root();
                    json!({
                        "phase": "parse",
                        "ast": ast_node_json(&parsed.file, root, &source),
                        "errors": err_val,
                    })
                } else {
                    json!({ "phase": "parse", "errors": err_val })
                };
                println!(
                    "{}",
                    serde_json::to_string_pretty(&out).map_err(|e| e.to_string())?
                );
                return Ok(0);
            }

            let diags = parse_errors_to_diags(&parsed.errors);
            human::write_diagnostics(&diags, &source, &file_label).map_err(|e| e.to_string())?;

            Ok(human_exit(cli.json_output, !parsed.errors.is_empty()))
        }
        Command::Symbols => {
            let parsed = parse(&source);
            let unit = analyze_unit(file_label.as_str(), &source, &parsed);
            let symbol_rows: Vec<Value> = unit
                .symbols
                .iter()
                .map(|symbol| {
                    json!({
                        "name": symbol.name.to_string(),
                        "kind": format!("{:?}", symbol.kind),
                        "range": [symbol.decl_range.start, symbol.decl_range.end],
                    })
                })
                .collect();
            let unknown: Vec<Value> = unit
                .diagnostics
                .iter()
                .filter(|diag| matches!(diag.kind, DiagnosticKind::UnresolvedReference | DiagnosticKind::WrongNamespace))
                .map(|diag| {
                    json!({
                        "range": [diag.range.start, diag.range.end],
                        "message": diag.message,
                    })
                })
                .collect();

            let field_access_rows: Vec<Value> = unit
                .field_accesses
                .iter()
                .map(|fa| {
                    json!({
                        "base_name": fa.base_name.to_string(),
                        "base_namespace": format!("{:?}", fa.base_namespace),
                        "in_type_position": fa.in_type_position,
                        "segments": fa.field_path.iter().map(|seg| json!({
                            "name": seg.name.to_string(),
                            "range": [seg.range.start, seg.range.end],
                        })).collect::<Vec<_>>(),
                    })
                })
                .collect();

            let reference_rows: Vec<Value> = unit
                .references
                .iter()
                .map(|reference| {
                    json!({
                        "name": reference.name.to_string(),
                        "namespace": format!("{:?}", reference.namespace),
                        "kind": format!("{:?}", reference.kind),
                        "range": [reference.range.start, reference.range.end],
                        "resolved": reference.resolution.is_some(),
                    })
                })
                .collect();

            if cli.json_output {
                let out = if cli.unknown_only {
                    json!({
                        "phase": "symbols",
                        "unknown_symbols": unknown,
                    })
                } else {
                    json!({
                        "phase": "symbols",
                        "symbols": symbol_rows,
                        "references": reference_rows,
                        "field_accesses": field_access_rows,
                        "unknown_symbols": unknown,
                    })
                };
                println!(
                    "{}",
                    serde_json::to_string_pretty(&out).map_err(|e| e.to_string())?
                );
                return Ok(0);
            }

            let diags = parse_errors_to_diags(&parsed.errors);
            human::write_diagnostics(&diags, &source, &file_label).map_err(|e| e.to_string())?;

            if !parsed.errors.is_empty() && !cli.unknown_only {
                let rows: Vec<(String, &str, Range<usize>)> = unit
                    .symbols
                    .iter()
                    .map(|symbol| {
                        (
                            symbol.name.to_string(),
                            match symbol.kind {
                                abap_symbols::SymbolKind::BuiltinType => "BuiltinType",
                                abap_symbols::SymbolKind::BuiltinConstant => "BuiltinConstant",
                                abap_symbols::SymbolKind::BuiltinVariable => "BuiltinVariable",
                                abap_symbols::SymbolKind::Variable => "Variable",
                                abap_symbols::SymbolKind::Constant => "Constant",
                                abap_symbols::SymbolKind::TypeDef => "Type",
                                abap_symbols::SymbolKind::FieldSymbol => "FieldSymbol",
                                abap_symbols::SymbolKind::Form => "Form",
                                abap_symbols::SymbolKind::Class => "Class",
                                abap_symbols::SymbolKind::Interface => "Interface",
                                abap_symbols::SymbolKind::Method => "Method",
                                abap_symbols::SymbolKind::Include => "Include",
                                abap_symbols::SymbolKind::Event => "Event",
                                abap_symbols::SymbolKind::Module => "Module",
                                abap_symbols::SymbolKind::Report => "Report",
                                _ => "Symbol",
                            },
                            symbol.decl_range.clone(),
                        )
                    })
                    .collect();
                human::write_symbols_table(&rows, human::stdout_color_enabled())
                    .map_err(|e| e.to_string())?;
            }

            Ok(human_exit(cli.json_output, !parsed.errors.is_empty()))
        }
        Command::Check => {
            let parsed = parse(&source);
            let unit = analyze_unit(file_label.as_str(), &source, &parsed);
            let lex_parse: Vec<Value> = parsed
                .errors
                .iter()
                .map(|e| {
                    json!({
                        "phase": "front_end",
                        "range": [e.range.start, e.range.end],
                        "message": e.message,
                    })
                })
                .collect();

            if cli.json_output {
                let semantic_diags: Vec<Value> = unit
                    .diagnostics
                    .iter()
                    .map(|diag| {
                        json!({
                            "range": [diag.range.start, diag.range.end],
                            "message": diag.message,
                            "kind": format!("{:?}", diag.kind),
                        })
                    })
                    .collect();
                let out = json!({
                    "phase": "check",
                    "lex_parse_errors": lex_parse,
                    "semantic_diagnostics": semantic_diags,
                    "semantic_note": "Semantic checking currently covers symbol collection, lexical resolution, wrong-namespace diagnostics, and include resolution; deeper type checking is still reserved.",
                });
                println!(
                    "{}",
                    serde_json::to_string_pretty(&out).map_err(|e| e.to_string())?
                );
                return Ok(0);
            }

            let diags = parse_errors_to_diags(&parsed.errors);
            human::write_diagnostics(&diags, &source, &file_label).map_err(|e| e.to_string())?;

            Ok(human_exit(cli.json_output, !parsed.errors.is_empty()))
        }
    }
}
