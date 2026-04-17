//! ABAP tooling CLI: rustc-style diagnostics on stderr when something fails; human mode is silent on success (`--json` always emits structured output).
//!
//! ```text
//! abap-cli lex [--json] [--errors-only] [FILE]
//! abap-cli parse [--json] [--ast] [--errors-only] [FILE]
//! abap-cli symbols [--json] [--unknown-only] [FILE]
//! abap-cli check [--json] [FILE]
//! abap-cli analyze --json [--with-project] [--pretty] [FILE]
//! abap-cli expand [--json] [--pretty] [FILE]
//! abap-cli call-graph --json [--symbol NAME] [--pretty] [FILE]
//! abap-cli remote-candidates [--json] [--pretty] [PATH]
//! ```
//!
//! `FILE` is UTF-8 ABAP source; omit or use `-` for stdin.
//! `PATH` for `remote-candidates` is a workspace file or directory anchor; omit it to use the current directory.

mod human;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_cache::{
    AnalysisSnapshot, CallGraphEdge, CallGraphNode, DocumentInput, DocumentStore, EffectiveSource,
    build_effective_source, load_workspace_documents, manifest_document_metadata, path_to_file_uri,
};
use abap_lexer::tokenize;
use abap_lsp::{RemoteDependencyCandidate, collect_remote_dependency_candidates};
use abap_parser::parse;
use abap_symbols::{
    DiagnosticKind, ProjectAnalysis, ProjectStaticAnalysisSummary, SemanticDossierContext,
    analyze_unit, build_project_routine_analysis, build_project_static_analysis_summary,
    build_semantic_dossier,
};
use serde_json::{Value, json};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    Lex,
    Parse,
    Symbols,
    Check,
    Analyze,
    Expand,
    CallGraph,
    RemoteCandidates,
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
    /// Analyze: load workspace peers for cross-unit resolution.
    analyze_with_project: bool,
    /// Analyze/expand/call-graph/remote-candidates: pretty-print JSON instead of compact output.
    pretty: bool,
    /// Call graph: focus queries on a matching callable name/id.
    call_graph_symbol: Option<String>,
    /// Source path, or for remote-candidates a workspace file/directory anchor.
    path: Option<String>,
}

fn usage() -> String {
    r#"ABAP tooling CLI — human-readable diagnostics by default; add --json for structured output.

Usage:
  abap-cli [--json] lex [--errors-only] [FILE]
  abap-cli [--json] parse [--ast] [--errors-only] [FILE]
  abap-cli [--json] symbols [--unknown-only] [FILE]
  abap-cli [--json] check [FILE]
  abap-cli analyze --json [--with-project] [--pretty] [FILE]
  abap-cli expand [--json] [--pretty] [FILE]
  abap-cli call-graph --json [--symbol NAME] [--pretty] [FILE]
  abap-cli remote-candidates [--json] [--pretty] [PATH]

If FILE is omitted or `-`, read source from stdin.
For `remote-candidates`, PATH may be a file or directory and defaults to the current directory.

Commands:
  lex       Tokenize (`--json` for tokens on a clean run; human only prints a token list when lexing failed)
  parse     Parser diagnostics (silent human run when clean); `--json --ast` for a syntax tree
  symbols   `--json` for identifier index; human is silent when clean, otherwise diagnostics and a symbol table
  check     Front-end diagnostics only (human silent when clean)
  analyze   Semantic dossier export for AI/tooling consumption (`--json` required for structured output)
  expand    Effective source expansion with include source maps
  call-graph  Project-scale call graph export and caller/callee queries (`--json` required)
  remote-candidates  Deduped remote dependency candidates from editable workspace files (`--json` also includes source-to-candidate mapping)

Options:
  --json          Print JSON to stdout (no rustc-style rendering)
  --ast           Parse: include `ast` in JSON output (ignored in human mode; use `--json --ast`)
  --errors-only   Lex: only errors. Parse (JSON): same as default without --ast
  --unknown-only  Symbols: only unknown / unresolved identifiers (empty until wired)
  --with-project  Analyze: load workspace peers around FILE for cross-unit resolution
  --symbol NAME   Call graph: focus on callable nodes matching NAME / qualified name / node id
  --pretty        Analyze/expand/call-graph/remote-candidates: pretty-print JSON

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
        "analyze" => Command::Analyze,
        "expand" => Command::Expand,
        "call-graph" => Command::CallGraph,
        "remote-candidates" => Command::RemoteCandidates,
        _ => return Err(format!("unknown command {:?}\n{}", cmd, usage())),
    };

    let mut errors_only = false;
    let mut parse_show_ast = false;
    let mut unknown_only = false;
    let mut analyze_with_project = false;
    let mut pretty = false;
    let mut call_graph_symbol = None;
    let mut path: Option<String> = None;

    let remaining: Vec<String> = it.collect();
    let mut idx = 0usize;
    while let Some(arg) = remaining.get(idx) {
        match arg.as_str() {
            "-h" | "--help" => return Err(usage()),
            "--ast" => parse_show_ast = true,
            "--errors-only" => errors_only = true,
            "--unknown-only" => unknown_only = true,
            "--with-project" => analyze_with_project = true,
            "--pretty" => pretty = true,
            "--json" => json_output = true,
            "--symbol" => {
                idx += 1;
                let Some(symbol) = remaining.get(idx) else {
                    return Err(format!("missing value for --symbol\n{}", usage()));
                };
                call_graph_symbol = Some(symbol.clone());
            }
            "-" => path = Some(arg.clone()),
            s if !s.starts_with('-') => {
                if path.is_some() {
                    return Err(format!("unexpected extra argument {:?}\n{}", s, usage()));
                }
                path = Some(s.to_string());
            }
            other => return Err(format!("unknown option {other:?}\n{}", usage())),
        }
        idx += 1;
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
    if analyze_with_project && command != Command::Analyze {
        return Err(format!(
            "--with-project only applies to analyze\n{}",
            usage()
        ));
    }
    if pretty
        && !matches!(
            command,
            Command::Analyze | Command::Expand | Command::CallGraph | Command::RemoteCandidates
        )
    {
        return Err(format!(
            "--pretty only applies to analyze, expand, call-graph, and remote-candidates\n{}",
            usage()
        ));
    }
    if call_graph_symbol.is_some() && command != Command::CallGraph {
        return Err(format!("--symbol only applies to call-graph\n{}", usage()));
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
        analyze_with_project,
        pretty,
        call_graph_symbol,
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
        Some(p) => {
            std::fs::read_to_string(p).map_err(|e| format!("{}: {e}", Path::new(p).display()))
        }
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
    obj.insert("id".to_string(), json!(id.0));
    obj.insert("range".to_string(), json!([range.start, range.end]));

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

    if cli.command == Command::Analyze {
        return run_analyze(&cli);
    }
    if cli.command == Command::Expand {
        return run_expand(&cli);
    }
    if cli.command == Command::CallGraph {
        return run_call_graph(&cli);
    }
    if cli.command == Command::RemoteCandidates {
        return run_remote_candidates(&cli);
    }

    let source = read_source(cli.path.as_deref())?;
    let file_label = display_path(cli.path.as_deref());

    match cli.command {
        Command::Lex => {
            let abap_lexer::TokenizeResult { tokens, errors, .. } = tokenize(&source);
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
                .filter(|diag| {
                    matches!(
                        diag.kind,
                        DiagnosticKind::UnresolvedReference
                            | DiagnosticKind::WrongNamespace
                            | DiagnosticKind::UnverifiedOpenSqlSource
                            | DiagnosticKind::InvalidOpenSqlIntoTarget
                    )
                })
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
        Command::Analyze | Command::Expand | Command::CallGraph | Command::RemoteCandidates => {
            unreachable!("handled above")
        }
    }
}

struct AnalyzeSnapshot {
    unit: Arc<abap_symbols::UnitAnalysis>,
    parse: Arc<abap_parser::ParseResult>,
    project: Option<Arc<abap_symbols::ProjectAnalysis>>,
    static_analysis: Option<Arc<ProjectStaticAnalysisSummary>>,
    target_path: Option<String>,
    object_name: Option<String>,
    is_dependency: bool,
    workspace_root_uri: Option<String>,
    manifest_present: bool,
    project_unit_count: Option<usize>,
    dependency_unit_count: Option<usize>,
}

struct ExpandSnapshotSet {
    root: Arc<AnalysisSnapshot>,
    snapshots: HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
}

struct RemoteCandidateWorkspace {
    workspace_root_uri: String,
    editable_document_count: usize,
    source_uris: Vec<String>,
    source_candidates: BTreeMap<String, Vec<RemoteDependencyCandidate>>,
    candidates: Vec<RemoteDependencyCandidate>,
}

fn run_analyze(cli: &Cli) -> Result<i32, String> {
    if !cli.json_output {
        return Err("analyze currently requires --json".to_string());
    }

    let snapshot = if cli.analyze_with_project {
        load_project_analyze_snapshot(cli.path.as_deref())?
    } else {
        load_single_file_analyze_snapshot(cli.path.as_deref())?
    };

    let dossier = build_semantic_dossier(
        snapshot.unit.as_ref(),
        SemanticDossierContext {
            parse_errors: &snapshot.parse.errors,
            project: snapshot.project.as_deref(),
            static_analysis: snapshot.static_analysis.as_deref(),
            target_path: snapshot.target_path.as_deref(),
            object_name: snapshot.object_name.as_deref(),
            is_dependency: snapshot.is_dependency,
            workspace_root_uri: snapshot.workspace_root_uri.as_deref(),
            manifest_present: snapshot.manifest_present,
            project_unit_count: snapshot.project_unit_count,
            dependency_unit_count: snapshot.dependency_unit_count,
        },
    );

    let json = if cli.pretty {
        serde_json::to_string_pretty(&dossier)
    } else {
        serde_json::to_string(&dossier)
    }
    .map_err(|e| e.to_string())?;
    println!("{json}");
    Ok(0)
}

fn run_expand(cli: &Cli) -> Result<i32, String> {
    let snapshots = load_expand_snapshot_set(cli.path.as_deref())?;
    let effective = build_effective_source(snapshots.root.as_ref(), &snapshots.snapshots);

    if cli.json_output {
        let json = if cli.pretty {
            serde_json::to_string_pretty(&effective)
        } else {
            serde_json::to_string(&effective)
        }
        .map_err(|e| e.to_string())?;
        println!("{json}");
        return Ok(0);
    }

    print!("{}", render_effective_source_human(&effective));
    Ok(0)
}

fn run_call_graph(cli: &Cli) -> Result<i32, String> {
    if !cli.json_output {
        return Err("call-graph currently requires --json".to_string());
    }

    let snapshot = load_call_graph_snapshot(cli.path.as_deref())?;
    let graph = snapshot.call_graph();

    let output = if let Some(symbol_query) = cli.call_graph_symbol.as_deref() {
        let matched_nodes: Vec<_> = graph
            .find_nodes(symbol_query)
            .into_iter()
            .cloned()
            .collect();
        let outbound = dedup_edges(
            matched_nodes
                .iter()
                .flat_map(|node| graph.outbound_calls(node.id.as_ref()).into_iter().cloned()),
        );
        let inbound = dedup_edges(
            matched_nodes
                .iter()
                .flat_map(|node| graph.inbound_callers(node.id.as_ref()).into_iter().cloned()),
        );
        let unresolved = dedup_edges(matched_nodes.iter().flat_map(|node| {
            graph
                .unresolved_outbound_calls(node.id.as_ref())
                .into_iter()
                .cloned()
        }));
        let related_edges = dedup_edges(
            outbound
                .iter()
                .cloned()
                .chain(inbound.iter().cloned())
                .chain(unresolved.iter().cloned()),
        );

        let mut node_ids = BTreeSet::new();
        for node in &matched_nodes {
            node_ids.insert(node.id.to_string());
        }
        for edge in &related_edges {
            node_ids.insert(edge.source.to_string());
            if let Some(target) = edge.target.as_ref() {
                node_ids.insert(target.to_string());
            }
        }

        let nodes: Vec<_> = graph
            .nodes
            .iter()
            .filter(|node| node_ids.contains(node.id.as_ref()))
            .map(call_graph_node_json)
            .collect();

        json!({
            "phase": "call_graph",
            "target_uri": snapshot.uri.as_ref(),
            "symbol_query": symbol_query,
            "project_node_count": graph.nodes.len(),
            "project_edge_count": graph.edges.len(),
            "matched_nodes": matched_nodes.iter().map(call_graph_node_json).collect::<Vec<_>>(),
            "nodes": nodes,
            "edges": related_edges.iter().map(call_graph_edge_json).collect::<Vec<_>>(),
            "outbound": outbound.iter().map(call_graph_edge_json).collect::<Vec<_>>(),
            "inbound": inbound.iter().map(call_graph_edge_json).collect::<Vec<_>>(),
            "unresolved": unresolved.iter().map(call_graph_edge_json).collect::<Vec<_>>(),
        })
    } else {
        json!({
            "phase": "call_graph",
            "target_uri": snapshot.uri.as_ref(),
            "project_node_count": graph.nodes.len(),
            "project_edge_count": graph.edges.len(),
            "nodes": graph.nodes.iter().map(call_graph_node_json).collect::<Vec<_>>(),
            "edges": graph.edges.iter().map(call_graph_edge_json).collect::<Vec<_>>(),
        })
    };

    let json = if cli.pretty {
        serde_json::to_string_pretty(&output)
    } else {
        serde_json::to_string(&output)
    }
    .map_err(|e| e.to_string())?;
    println!("{json}");
    Ok(0)
}

fn run_remote_candidates(cli: &Cli) -> Result<i32, String> {
    let workspace = load_remote_candidate_workspace(cli.path.as_deref())?;

    if cli.json_output {
        let output = json!({
            "phase": "remote_candidates",
            "workspace_root_uri": workspace.workspace_root_uri,
            "editable_document_count": workspace.editable_document_count,
            "source_uri_count": workspace.source_uris.len(),
            "candidate_count": workspace.candidates.len(),
            "source_uris": workspace.source_uris,
            "source_candidates": workspace.source_candidates,
            "candidates": workspace.candidates,
        });
        let json = if cli.pretty {
            serde_json::to_string_pretty(&output)
        } else {
            serde_json::to_string(&output)
        }
        .map_err(|e| e.to_string())?;
        println!("{json}");
        return Ok(0);
    }

    for candidate in workspace.candidates {
        println!("{}", candidate.name);
    }
    Ok(0)
}

fn load_remote_candidate_workspace(path: Option<&str>) -> Result<RemoteCandidateWorkspace, String> {
    let anchor_path = resolve_workspace_anchor_path(path)?;
    let workspace_root = find_workspace_root_for_anchor(&anchor_path)?;
    let workspace_root_uri = path_to_file_uri(&workspace_root);
    let workspace = load_workspace_documents(&workspace_root_uri, &HashMap::new());
    let editable_document_count = workspace
        .documents
        .iter()
        .filter(|document| !document.is_dependency)
        .count();

    let inputs: Vec<DocumentInput> = workspace
        .documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect();

    let snapshots = DocumentStore::default().replace_all(inputs);
    let mut deduped = BTreeMap::<String, RemoteDependencyCandidate>::new();
    let mut source_uris = Vec::new();
    let mut source_candidates = BTreeMap::<String, Vec<RemoteDependencyCandidate>>::new();
    for document in workspace
        .documents
        .iter()
        .filter(|document| !document.is_dependency)
    {
        let Some(snapshot) = snapshots.get(document.uri.as_ref()) else {
            continue;
        };
        let mut per_source = BTreeMap::<String, RemoteDependencyCandidate>::new();
        for candidate in collect_remote_dependency_candidates(snapshot.as_ref()) {
            insert_remote_candidate(&mut deduped, candidate.clone());
            insert_remote_candidate(&mut per_source, candidate);
        }
        if !per_source.is_empty() {
            source_uris.push(document.uri.to_string());
            source_candidates.insert(document.uri.to_string(), per_source.into_values().collect());
        }
    }

    let mut candidates: Vec<_> = deduped.into_values().collect();
    candidates.sort_by(|left, right| left.name.cmp(&right.name).then(left.kind.cmp(&right.kind)));

    Ok(RemoteCandidateWorkspace {
        workspace_root_uri,
        editable_document_count,
        source_uris,
        source_candidates,
        candidates,
    })
}

fn insert_remote_candidate(
    deduped: &mut BTreeMap<String, RemoteDependencyCandidate>,
    candidate: RemoteDependencyCandidate,
) {
    let normalized_name = candidate.name.trim().to_ascii_lowercase();
    if normalized_name.is_empty() {
        return;
    }

    let priority = remote_candidate_kind_priority(candidate.kind.as_str());
    match deduped.get(&normalized_name) {
        Some(existing) if remote_candidate_kind_priority(existing.kind.as_str()) >= priority => {}
        _ => {
            deduped.insert(
                normalized_name.clone(),
                RemoteDependencyCandidate {
                    name: normalized_name,
                    kind: candidate.kind.trim().to_ascii_lowercase(),
                },
            );
        }
    }
}

fn remote_candidate_kind_priority(kind: &str) -> usize {
    match kind.trim().to_ascii_lowercase().as_str() {
        "message-class" => 5,
        "include" => 4,
        "function" => 4,
        "static" => 3,
        "type" => 2,
        _ => 1,
    }
}

fn dedup_edges(edges: impl IntoIterator<Item = CallGraphEdge>) -> Vec<CallGraphEdge> {
    let mut edges: Vec<_> = edges.into_iter().collect();
    edges.sort_by(|left, right| {
        left.source
            .cmp(&right.source)
            .then(left.edge_kind.cmp(&right.edge_kind))
            .then(left.resolution_status.cmp(&right.resolution_status))
            .then(left.target.cmp(&right.target))
            .then(left.target_name.cmp(&right.target_name))
            .then(left.source_range.start.cmp(&right.source_range.start))
            .then(left.source_range.end.cmp(&right.source_range.end))
    });
    edges.dedup();
    edges
}

fn call_graph_node_json(node: &CallGraphNode) -> Value {
    json!({
        "id": node.id.as_ref(),
        "kind": node.kind,
        "name": node.name.as_ref(),
        "qualified_name": node.qualified_name.as_ref(),
        "unit_uri": node.unit_uri.as_ref(),
        "decl_range": [node.decl_range.start, node.decl_range.end],
    })
}

fn call_graph_edge_json(edge: &CallGraphEdge) -> Value {
    json!({
        "source": edge.source.as_ref(),
        "target": edge.target.as_ref().map(|target| target.as_ref()),
        "edge_kind": edge.edge_kind,
        "resolution_status": edge.resolution_status,
        "target_name": edge.target_name.as_ref(),
        "source_range": [edge.source_range.start, edge.source_range.end],
    })
}

fn load_single_file_analyze_snapshot(path: Option<&str>) -> Result<AnalyzeSnapshot, String> {
    let source = read_source(path)?;
    let target_path = resolve_target_path(path)?;
    let target_uri = target_path
        .as_ref()
        .map(|path| path_to_file_uri(path))
        .unwrap_or_else(|| display_path(path));
    let parsed = Arc::new(parse(&source));
    let (unit, static_analysis) = build_single_file_static_analysis(analyze_unit(
        target_uri.as_str(),
        &source,
        parsed.as_ref(),
    ));

    Ok(AnalyzeSnapshot {
        unit: Arc::new(unit),
        parse: parsed,
        project: None,
        static_analysis: Some(Arc::new(static_analysis)),
        target_path: target_path.map(|path| path.display().to_string()),
        object_name: None,
        is_dependency: false,
        workspace_root_uri: None,
        manifest_present: false,
        project_unit_count: None,
        dependency_unit_count: None,
    })
}

fn load_call_graph_snapshot(
    path: Option<&str>,
) -> Result<Arc<abap_cache::AnalysisSnapshot>, String> {
    let Some(target_path) = resolve_target_path(path)? else {
        let source = read_source(path)?;
        let store = DocumentStore::default();
        return Ok(store.publish("file:///stdin.abap", 1, &source));
    };

    let target_uri = path_to_file_uri(&target_path);
    let workspace_root = find_workspace_root(&target_path)?;
    let workspace_root_uri = path_to_file_uri(&workspace_root);
    let workspace = load_workspace_documents(&workspace_root_uri, &HashMap::new());

    let mut inputs: Vec<DocumentInput> = workspace
        .documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect();

    if !inputs.iter().any(|input| input.uri.as_ref() == target_uri) {
        let source = std::fs::read_to_string(&target_path)
            .map_err(|e| format!("{}: {e}", target_path.display()))?;
        let (is_dependency, object_name) = workspace
            .manifest
            .as_ref()
            .and_then(|manifest| {
                manifest_document_metadata(
                    &workspace.root_path,
                    &workspace.root_uri,
                    manifest,
                    &target_uri,
                )
            })
            .unwrap_or((false, None));
        inputs.push(DocumentInput {
            uri: Arc::from(target_uri.as_str()),
            version: 1,
            text: Arc::from(source),
            is_dependency,
            object_name,
        });
    }

    let store = DocumentStore::default();
    let snapshots = store.replace_all(inputs);
    snapshots.get(target_uri.as_str()).cloned().ok_or_else(|| {
        format!(
            "workspace call graph did not include {}",
            target_path.display()
        )
    })
}

fn load_project_analyze_snapshot(path: Option<&str>) -> Result<AnalyzeSnapshot, String> {
    let target_path = resolve_target_path(path)?
        .ok_or_else(|| "--with-project requires a file path".to_string())?;
    let target_uri = path_to_file_uri(&target_path);
    let workspace_root = find_workspace_root(&target_path)?;
    let workspace_root_uri = path_to_file_uri(&workspace_root);
    let workspace = load_workspace_documents(&workspace_root_uri, &HashMap::new());
    let dependency_unit_count = workspace
        .documents
        .iter()
        .filter(|document| document.is_dependency)
        .count();
    let manifest_present = workspace.manifest.is_some();

    let mut inputs: Vec<DocumentInput> = workspace
        .documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect();

    if !inputs.iter().any(|input| input.uri.as_ref() == target_uri) {
        let source = std::fs::read_to_string(&target_path)
            .map_err(|e| format!("{}: {e}", target_path.display()))?;
        let (is_dependency, object_name) = workspace
            .manifest
            .as_ref()
            .and_then(|manifest| {
                manifest_document_metadata(
                    &workspace.root_path,
                    &workspace.root_uri,
                    manifest,
                    &target_uri,
                )
            })
            .unwrap_or((false, None));
        inputs.push(DocumentInput {
            uri: Arc::from(target_uri.as_str()),
            version: 1,
            text: Arc::from(source),
            is_dependency,
            object_name,
        });
    }

    let store = DocumentStore::default();
    let snapshots = store.replace_all(inputs);
    let snapshot = snapshots.get(target_uri.as_str()).cloned().ok_or_else(|| {
        format!(
            "workspace analysis did not include {}",
            target_path.display()
        )
    })?;

    Ok(AnalyzeSnapshot {
        unit: Arc::clone(&snapshot.symbols),
        parse: Arc::clone(&snapshot.parse),
        project: Some(Arc::clone(&snapshot.project)),
        static_analysis: snapshot.static_analysis.as_ref().map(Arc::clone),
        target_path: Some(target_path.display().to_string()),
        object_name: snapshot.object_name.as_ref().map(|name| name.to_string()),
        is_dependency: snapshot.is_dependency,
        workspace_root_uri: Some(workspace_root_uri),
        manifest_present,
        project_unit_count: Some(snapshot.project.units.len()),
        dependency_unit_count: Some(dependency_unit_count),
    })
}

fn build_single_file_static_analysis(
    unit: abap_symbols::UnitAnalysis,
) -> (
    abap_symbols::UnitAnalysis,
    abap_symbols::ProjectStaticAnalysisSummary,
) {
    let unit_id = unit.unit_id;
    let unit_uri = Arc::clone(&unit.uri);
    let provided_name_to_unit = unit
        .provided_names
        .iter()
        .cloned()
        .map(|name| (name, unit_id))
        .collect();
    let project = ProjectAnalysis {
        units: vec![unit.clone()],
        uri_to_unit: HashMap::from([(unit_uri, unit_id)]),
        provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    let routine_analysis = build_project_routine_analysis(&project);
    let static_analysis = build_project_static_analysis_summary(&project, &routine_analysis);
    let mut unit = unit;
    for diagnostic in routine_analysis.diagnostics_for_unit(unit_id) {
        if !unit.diagnostics.contains(diagnostic) {
            unit.diagnostics.push(diagnostic.clone());
        }
    }
    (unit, static_analysis)
}

fn load_expand_snapshot_set(path: Option<&str>) -> Result<ExpandSnapshotSet, String> {
    let Some(target_path) = resolve_target_path(path)? else {
        let source = read_source(path)?;
        let store = DocumentStore::default();
        let root = store.publish_input(DocumentInput {
            uri: Arc::from("file:///stdin.abap"),
            version: 1,
            text: Arc::from(source),
            is_dependency: false,
            object_name: None,
        });
        return Ok(ExpandSnapshotSet {
            root: Arc::clone(&root),
            snapshots: HashMap::from([(Arc::clone(&root.uri), root)]),
        });
    };

    let target_uri = path_to_file_uri(&target_path);
    let workspace_root = find_workspace_root(&target_path)?;
    let workspace_root_uri = path_to_file_uri(&workspace_root);
    let workspace = load_workspace_documents(&workspace_root_uri, &HashMap::new());

    let mut inputs: Vec<DocumentInput> = workspace
        .documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect();

    if !inputs.iter().any(|input| input.uri.as_ref() == target_uri) {
        let source = std::fs::read_to_string(&target_path)
            .map_err(|e| format!("{}: {e}", target_path.display()))?;
        let (is_dependency, object_name) = workspace
            .manifest
            .as_ref()
            .and_then(|manifest| {
                manifest_document_metadata(
                    &workspace.root_path,
                    &workspace.root_uri,
                    manifest,
                    &target_uri,
                )
            })
            .unwrap_or((false, None));
        inputs.push(DocumentInput {
            uri: Arc::from(target_uri.as_str()),
            version: 1,
            text: Arc::from(source),
            is_dependency,
            object_name,
        });
    }

    let store = DocumentStore::default();
    let snapshots = store.replace_all(inputs);
    let root = snapshots.get(target_uri.as_str()).cloned().ok_or_else(|| {
        format!(
            "workspace expansion did not include {}",
            target_path.display()
        )
    })?;

    Ok(ExpandSnapshotSet { root, snapshots })
}

fn render_effective_source_human(effective: &EffectiveSource) -> String {
    let mut out = String::new();
    let mut current_marker: Option<(&str, usize, usize)> = None;

    for segment in &effective.segments {
        let next_marker = (
            segment.source_unit.uri.as_str(),
            segment.source_range.start,
            segment.source_range.end,
        );
        if current_marker != Some(next_marker) {
            if !out.is_empty() && !out.ends_with('\n') {
                out.push('\n');
            }
            if current_marker.is_some() {
                out.push_str("* <<< END SOURCE\n");
            }
            out.push_str("* >>> SOURCE ");
            if let Some(object_name) = segment.source_unit.object_name.as_deref() {
                out.push_str(object_name);
                out.push(' ');
            }
            out.push('(');
            out.push_str(segment.source_unit.uri.as_str());
            out.push(')');
            out.push(' ');
            out.push('[');
            out.push_str(&segment.source_range.start.to_string());
            out.push_str("..");
            out.push_str(&segment.source_range.end.to_string());
            out.push_str("]\n");
            current_marker = Some(next_marker);
        }
        out.push_str(
            &effective.expanded_text[segment.expanded_range.start..segment.expanded_range.end],
        );
    }

    if current_marker.is_some() {
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str("* <<< END SOURCE\n");
    }

    if !effective.diagnostics.is_empty() {
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str("\nDiagnostics:\n");
        for diagnostic in &effective.diagnostics {
            out.push_str("- ");
            out.push_str(diagnostic.kind);
            out.push_str(": ");
            out.push_str(&diagnostic.message);
            out.push_str(" @ ");
            out.push_str(&diagnostic.source_uri);
            if let Some(range) = diagnostic.source_range.as_ref() {
                out.push(' ');
                out.push('[');
                out.push_str(&range.start.to_string());
                out.push_str("..");
                out.push_str(&range.end.to_string());
                out.push(']');
            }
            out.push('\n');
        }
    }

    out
}

fn resolve_target_path(path: Option<&str>) -> Result<Option<PathBuf>, String> {
    match path {
        None | Some("-") => Ok(None),
        Some(path) => {
            let path = PathBuf::from(path);
            let absolute = if path.is_absolute() {
                path
            } else {
                std::env::current_dir()
                    .map_err(|e| format!("current dir: {e}"))?
                    .join(path)
            };
            absolute
                .canonicalize()
                .map(normalize_windows_path)
                .map(Some)
                .map_err(|e| format!("{}: {e}", absolute.display()))
        }
    }
}

fn resolve_workspace_anchor_path(path: Option<&str>) -> Result<PathBuf, String> {
    match path {
        None => std::env::current_dir()
            .map_err(|e| format!("current dir: {e}"))?
            .canonicalize()
            .map(normalize_windows_path)
            .map_err(|e| format!("current dir: {e}")),
        Some("-") => Err("remote-candidates does not support stdin".to_string()),
        Some(path) => {
            let path = PathBuf::from(path);
            let absolute = if path.is_absolute() {
                path
            } else {
                std::env::current_dir()
                    .map_err(|e| format!("current dir: {e}"))?
                    .join(path)
            };
            absolute
                .canonicalize()
                .map(normalize_windows_path)
                .map_err(|e| format!("{}: {e}", absolute.display()))
        }
    }
}

fn find_workspace_root(target_path: &Path) -> Result<PathBuf, String> {
    find_workspace_root_for_anchor(target_path)
}

fn find_workspace_root_for_anchor(anchor_path: &Path) -> Result<PathBuf, String> {
    let start = if anchor_path.is_dir() {
        anchor_path
    } else {
        anchor_path
            .parent()
            .ok_or_else(|| format!("{} has no parent directory", anchor_path.display()))?
    };
    for ancestor in start.ancestors() {
        if ancestor.join("abapls.toml").is_file() {
            return Ok(ancestor.to_path_buf());
        }
    }

    let cwd = std::env::current_dir().map_err(|e| format!("current dir: {e}"))?;
    let cwd = cwd
        .canonicalize()
        .map(normalize_windows_path)
        .unwrap_or_else(|_| normalize_windows_path(cwd));
    if anchor_path.starts_with(&cwd) {
        return Ok(cwd);
    }

    Ok(start.to_path_buf())
}

fn normalize_windows_path(path: PathBuf) -> PathBuf {
    let text = path.to_string_lossy();
    if let Some(stripped) = text.strip_prefix(r"\\?\") {
        return PathBuf::from(stripped);
    }
    path
}

#[cfg(test)]
mod tests {
    use super::{load_remote_candidate_workspace, parse_cli_args};
    use std::fs;

    #[test]
    fn parses_remote_candidates_command() {
        let cli = parse_cli_args(
            ["remote-candidates", "--json", "--pretty", "."]
                .into_iter()
                .map(str::to_string),
        )
        .expect("cli");

        assert!(cli.json_output);
        assert!(cli.pretty);
        assert_eq!(cli.path.as_deref(), Some("."));
    }

    #[test]
    fn remote_candidates_only_use_editable_workspace_files() {
        let root = std::env::temp_dir().join("abap-cli-remote-candidates-editable-only");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::create_dir_all(root.join(".abapls/cache/packages/ZPKG/global-class"))
            .expect("dependency dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZDEP"
kind = "global-class"
root_file = ".abapls/cache/packages/ZPKG/global-class/ZDEP.abap"
dependency_of = [
  "src/ZMAIN.abap"
]
"#,
        )
        .expect("manifest");
        fs::write(
            root.join("src/ZMAIN.abap"),
            "REPORT zmain.\nDATA lo_main TYPE REF TO zcl_editable_missing.",
        )
        .expect("main");
        fs::write(
            root.join(".abapls/cache/packages/ZPKG/global-class/ZDEP.abap"),
            "CLASS zdep DEFINITION.\n  PUBLIC SECTION.\n    DATA mo_dep TYPE REF TO zcl_dependency_missing.\nENDCLASS.\nCLASS zdep IMPLEMENTATION.\nENDCLASS.",
        )
        .expect("dep");

        let workspace = load_remote_candidate_workspace(Some(root.to_string_lossy().as_ref()))
            .expect("remote candidates");
        let names: Vec<_> = workspace
            .candidates
            .iter()
            .map(|candidate| candidate.name.as_str())
            .collect();
        let source_candidates = workspace
            .source_candidates
            .values()
            .flat_map(|candidates| candidates.iter().map(|candidate| candidate.name.as_str()))
            .collect::<Vec<_>>();

        assert!(names.contains(&"zcl_editable_missing"), "{names:?}");
        assert!(!names.contains(&"zcl_dependency_missing"), "{names:?}");
        assert!(
            source_candidates.contains(&"zcl_editable_missing"),
            "{source_candidates:?}"
        );
        assert!(
            !source_candidates.contains(&"zcl_dependency_missing"),
            "{source_candidates:?}"
        );

        let _ = fs::remove_dir_all(&root);
    }
}
