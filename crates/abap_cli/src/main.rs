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
//! abap-cli call-dataflow [--json] --target NAME [--caller NAME] [--occurrence N] [--diagram ascii|svg|mermaid|rich-mermaid] [--pretty] [FILE]
//! abap-cli remote-candidates [--json] [--pretty] [PATH]
//! ```
//!
//! `FILE` is UTF-8 ABAP source; omit or use `-` for stdin.
//! `PATH` for `remote-candidates` is a workspace file or directory anchor; omit it to use the current directory.

mod human;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_cache::{
    AnalysisSnapshot, CallDataflowLifecycle, CallDataflowMatch, CallDataflowParameterTrace,
    CallDataflowProvenanceGraph, CallDataflowQuery, CallDataflowSelectedCall, CallDataflowTrace,
    CallGraphEdge, CallGraphNode, DocumentInput, DocumentStore, EffectiveSource,
    LocalExportResolver, SnapshotBuildPlan, build_call_dataflow_trace, build_effective_source,
    file_uri_to_path, load_workspace_documents, manifest_document_metadata, path_to_file_uri,
    resolve_local_export_dependency_document,
};
use abap_lexer::tokenize;
use abap_lsp::{
    RemoteDependencyCandidate, collect_remote_dependency_candidates,
    replace_all_workspace_documents_with_local_exports_for_build_plan,
};
use abap_parser::parse;
use abap_symbols::{
    DiagnosticKind, ProjectAnalysis, ProjectStaticAnalysisSummary, SemanticDossierContext,
    analyze_unit, build_project_routine_analysis, build_project_static_analysis_summary,
    build_semantic_dossier,
};
use serde_json::{Value, json};
use toml::Value as TomlValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    Lex,
    Parse,
    Symbols,
    Check,
    Analyze,
    Expand,
    CallGraph,
    CallDataflow,
    RemoteCandidates,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallDataflowDiagramFormat {
    Ascii,
    Svg,
    Mermaid,
    RichMermaid,
}

impl CallDataflowDiagramFormat {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Ascii => "ascii",
            Self::Svg => "svg",
            Self::Mermaid => "mermaid",
            Self::RichMermaid => "rich-mermaid",
        }
    }
}

#[derive(Debug)]
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
    /// Call-dataflow: target callee name.
    call_dataflow_target: Option<String>,
    /// Call-dataflow: optional caller routine/module/method filter.
    call_dataflow_caller: Option<String>,
    /// Call-dataflow: choose one occurrence after deterministic sorting.
    call_dataflow_occurrence: Option<usize>,
    /// Call-dataflow: human diagram renderer.
    call_dataflow_diagram: CallDataflowDiagramFormat,
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
  abap-cli call-dataflow [--json] --target NAME [--caller NAME] [--occurrence N] [--diagram ascii|svg|mermaid|rich-mermaid] [--pretty] [FILE]
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
  call-dataflow  High-level + technical data-flow trace for one call-site occurrence
  remote-candidates  Deduped remote dependency candidates from editable workspace files (`--json` also includes source-to-candidate mapping)

Options:
  --json          Print JSON to stdout (no rustc-style rendering)
  --ast           Parse: include `ast` in JSON output (ignored in human mode; use `--json --ast`)
  --errors-only   Lex: only errors. Parse (JSON): same as default without --ast
  --unknown-only  Symbols: only unknown / unresolved identifiers (empty until wired)
  --with-project  Analyze: load workspace peers around FILE for cross-unit resolution
  --symbol NAME   Call graph: focus on callable nodes matching NAME / qualified name / node id
  --target NAME   Call dataflow: target function module or method name
  --caller NAME   Call dataflow: optional caller filter
  --occurrence N  Call dataflow: select one deterministic match occurrence
  --diagram KIND  Call dataflow: human diagram renderer (`ascii` default; `svg`, `mermaid`, and `rich-mermaid` available)
  --pretty        Analyze/expand/call-graph/call-dataflow/remote-candidates: pretty-print JSON

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
        "call-dataflow" => Command::CallDataflow,
        "remote-candidates" => Command::RemoteCandidates,
        _ => return Err(format!("unknown command {:?}\n{}", cmd, usage())),
    };

    let mut errors_only = false;
    let mut parse_show_ast = false;
    let mut unknown_only = false;
    let mut analyze_with_project = false;
    let mut pretty = false;
    let mut call_graph_symbol = None;
    let mut call_dataflow_target = None;
    let mut call_dataflow_caller = None;
    let mut call_dataflow_occurrence = None;
    let mut call_dataflow_diagram = CallDataflowDiagramFormat::Ascii;
    let mut saw_call_dataflow_diagram = false;
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
            "--target" => {
                idx += 1;
                let Some(target) = remaining.get(idx) else {
                    return Err(format!("missing value for --target\n{}", usage()));
                };
                call_dataflow_target = Some(target.clone());
            }
            "--caller" => {
                idx += 1;
                let Some(caller) = remaining.get(idx) else {
                    return Err(format!("missing value for --caller\n{}", usage()));
                };
                call_dataflow_caller = Some(caller.clone());
            }
            "--occurrence" => {
                idx += 1;
                let Some(raw_occurrence) = remaining.get(idx) else {
                    return Err(format!("missing value for --occurrence\n{}", usage()));
                };
                let occurrence = raw_occurrence.parse::<usize>().map_err(|_| {
                    format!(
                        "invalid value for --occurrence {:?}\n{}",
                        raw_occurrence,
                        usage()
                    )
                })?;
                if occurrence == 0 {
                    return Err(format!("--occurrence must be >= 1\n{}", usage()));
                }
                call_dataflow_occurrence = Some(occurrence);
            }
            "--diagram" => {
                idx += 1;
                let Some(raw_diagram) = remaining.get(idx) else {
                    return Err(format!("missing value for --diagram\n{}", usage()));
                };
                saw_call_dataflow_diagram = true;
                call_dataflow_diagram = match raw_diagram.to_ascii_lowercase().as_str() {
                    "ascii" => CallDataflowDiagramFormat::Ascii,
                    "svg" => CallDataflowDiagramFormat::Svg,
                    "mermaid" => CallDataflowDiagramFormat::Mermaid,
                    "rich-mermaid" | "rich" => CallDataflowDiagramFormat::RichMermaid,
                    _ => {
                        return Err(format!(
                            "unsupported value for --diagram {:?}; expected `ascii`, `svg`, `mermaid`, or `rich-mermaid`\n{}",
                            raw_diagram,
                            usage()
                        ));
                    }
                };
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
            Command::Analyze
                | Command::Expand
                | Command::CallGraph
                | Command::CallDataflow
                | Command::RemoteCandidates
        )
    {
        return Err(format!(
            "--pretty only applies to analyze, expand, call-graph, call-dataflow, and remote-candidates\n{}",
            usage()
        ));
    }
    if call_graph_symbol.is_some() && command != Command::CallGraph {
        return Err(format!("--symbol only applies to call-graph\n{}", usage()));
    }
    if call_dataflow_target.is_some() && command != Command::CallDataflow {
        return Err(format!(
            "--target only applies to call-dataflow\n{}",
            usage()
        ));
    }
    if call_dataflow_caller.is_some() && command != Command::CallDataflow {
        return Err(format!(
            "--caller only applies to call-dataflow\n{}",
            usage()
        ));
    }
    if call_dataflow_occurrence.is_some() && command != Command::CallDataflow {
        return Err(format!(
            "--occurrence only applies to call-dataflow\n{}",
            usage()
        ));
    }
    if saw_call_dataflow_diagram && command != Command::CallDataflow {
        return Err(format!(
            "--diagram only applies to call-dataflow\n{}",
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
    if command == Command::CallDataflow && call_dataflow_target.is_none() {
        return Err(format!("call-dataflow requires --target NAME\n{}", usage()));
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
        call_dataflow_target,
        call_dataflow_caller,
        call_dataflow_occurrence,
        call_dataflow_diagram,
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
    if cli.command == Command::CallDataflow {
        return run_call_dataflow(&cli);
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
                            | DiagnosticKind::MissingTablesDeclaration
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
        Command::Analyze
        | Command::Expand
        | Command::CallGraph
        | Command::CallDataflow
        | Command::RemoteCandidates => {
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

    let snapshot = load_call_graph_snapshot(cli.path.as_deref(), SnapshotBuildPlan::CALL_GRAPH)?;
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

fn run_call_dataflow(cli: &Cli) -> Result<i32, String> {
    let snapshot = load_call_graph_snapshot(cli.path.as_deref(), SnapshotBuildPlan::CALL_DATAFLOW)?;
    let trace = build_call_dataflow_trace(
        snapshot.as_ref(),
        CallDataflowQuery {
            target: cli
                .call_dataflow_target
                .clone()
                .ok_or_else(|| "call-dataflow requires --target NAME".to_string())?,
            caller: cli.call_dataflow_caller.clone(),
            occurrence: cli.call_dataflow_occurrence,
        },
    );

    if cli.json_output {
        let json = if cli.pretty {
            serde_json::to_string_pretty(&trace)
        } else {
            serde_json::to_string(&trace)
        }
        .map_err(|e| e.to_string())?;
        println!("{json}");
        return Ok(0);
    }

    print!(
        "{}",
        render_call_dataflow_report(&trace, cli.call_dataflow_diagram)
    );
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

    let store = DocumentStore::default();
    let snapshots = replace_all_workspace_documents_with_local_exports_for_build_plan(
        &store,
        &workspace.root_path,
        &workspace.documents,
        SnapshotBuildPlan::REMOTE_CANDIDATES,
        None,
    );
    let mut local_export_resolver = LocalExportResolver::default();
    let mut deduped = BTreeMap::<String, RemoteDependencyCandidate>::new();
    let mut source_uris = Vec::new();
    let mut source_candidates = BTreeMap::<String, Vec<RemoteDependencyCandidate>>::new();
    for document in workspace
        .documents
        .iter()
        .filter(|document| !document.is_dependency)
    {
        let roots = source_local_export_roots(&workspace.root_path, document.uri.as_ref());
        let per_source = collect_transitive_remote_candidates_for_source(
            &snapshots,
            document.uri.as_ref(),
            &roots,
            &mut local_export_resolver,
        );
        for candidate in per_source.values().cloned() {
            insert_remote_candidate(&mut deduped, candidate.clone());
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

fn source_local_export_roots(workspace_root: &Path, source_uri: &str) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    let mut seen = HashSet::new();
    for sidecar_path in source_unit_sidecar_paths(workspace_root, source_uri) {
        for root in read_unit_sidecar_local_roots(&sidecar_path) {
            let key = normalized_local_export_path_key(&root);
            if seen.insert(key) {
                roots.push(root);
            }
        }
    }
    roots
}

fn collect_transitive_remote_candidates_for_source(
    snapshots: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    source_uri: &str,
    roots: &[PathBuf],
    resolver: &mut LocalExportResolver,
) -> BTreeMap<String, RemoteDependencyCandidate> {
    let mut deduped = BTreeMap::<String, RemoteDependencyCandidate>::new();
    let mut visited_uris = HashSet::from([source_uri.to_string()]);
    let mut queue = std::collections::VecDeque::from([source_uri.to_string()]);

    while let Some(current_uri) = queue.pop_front() {
        let Some(snapshot) = snapshots.get(current_uri.as_str()) else {
            continue;
        };

        enqueue_resolved_local_export_dependency_uris_for_cli(
            snapshot.as_ref(),
            roots,
            resolver,
            &mut visited_uris,
            &mut queue,
        );
        for candidate in collect_remote_dependency_candidates(snapshot.as_ref()) {
            if let Some(document) =
                resolve_candidate_from_local_export_roots(&candidate, roots, resolver)
            {
                let dependency_uri = document.uri.to_string();
                if visited_uris.insert(dependency_uri.clone())
                    && snapshots.get(dependency_uri.as_str()).is_some()
                {
                    queue.push_back(dependency_uri);
                }
                continue;
            }
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    deduped
}

fn enqueue_resolved_local_export_dependency_uris_for_cli(
    snapshot: &AnalysisSnapshot,
    roots: &[PathBuf],
    resolver: &mut LocalExportResolver,
    visited_uris: &mut HashSet<String>,
    queue: &mut std::collections::VecDeque<String>,
) {
    if roots.is_empty() {
        return;
    }

    for reference in &snapshot.symbols.references {
        let Some(kind) =
            local_export_candidate_kind_for_reference(reference.kind, reference.namespace)
        else {
            continue;
        };
        let Some(abap_symbols::Resolution::Symbol(handle)) = &reference.resolution else {
            continue;
        };
        let Some(document) = resolve_local_export_dependency_document(
            roots,
            resolver,
            reference.name.as_ref(),
            kind,
        ) else {
            continue;
        };
        let Some(resolved_unit) = snapshot.project.units.get(handle.unit.as_usize()) else {
            continue;
        };
        if resolved_unit.uri.as_ref() != document.uri.as_ref() {
            continue;
        }
        let dependency_uri = document.uri.to_string();
        if dependency_uri != snapshot.uri.as_ref() && visited_uris.insert(dependency_uri.clone()) {
            queue.push_back(dependency_uri);
        }
    }
}

fn local_export_candidate_kind_for_reference(
    kind: abap_symbols::ReferenceKind,
    namespace: abap_symbols::Namespace,
) -> Option<&'static str> {
    match kind {
        abap_symbols::ReferenceKind::Include => Some("include"),
        abap_symbols::ReferenceKind::StaticTarget => Some("static"),
        abap_symbols::ReferenceKind::TypeRef => Some("type"),
        abap_symbols::ReferenceKind::StructuredDeclEnd => None,
        abap_symbols::ReferenceKind::MessageClass => Some("message-class"),
        abap_symbols::ReferenceKind::RoutineCall
            if namespace == abap_symbols::Namespace::Routine =>
        {
            Some("function")
        }
        abap_symbols::ReferenceKind::Identifier | abap_symbols::ReferenceKind::RoutineCall => None,
    }
}

fn source_unit_sidecar_paths(workspace_root: &Path, source_uri: &str) -> Vec<PathBuf> {
    let Some(source_path) = file_uri_to_path(source_uri) else {
        return Vec::new();
    };
    if !source_path.starts_with(workspace_root) {
        return Vec::new();
    }

    let mut sidecar_paths = Vec::new();
    let mut seen = HashSet::new();

    if let Some(file_name) = source_path.file_name().and_then(|value| value.to_str()) {
        let sibling = source_path.with_file_name(format!("{file_name}.abapls-unit.toml"));
        push_sidecar_path_if_exists(&mut sidecar_paths, &mut seen, sibling);
    }

    let mut current_dir = source_path.parent();
    while let Some(dir) = current_dir {
        if !dir.starts_with(workspace_root) {
            break;
        }
        push_sidecar_path_if_exists(&mut sidecar_paths, &mut seen, dir.join("abapls-unit.toml"));
        if dir == workspace_root {
            break;
        }
        current_dir = dir.parent();
    }

    sidecar_paths
}

fn push_sidecar_path_if_exists(
    sidecar_paths: &mut Vec<PathBuf>,
    seen: &mut HashSet<String>,
    path: PathBuf,
) {
    if !path.is_file() {
        return;
    }
    let key = normalized_local_export_path_key(&path);
    if seen.insert(key) {
        sidecar_paths.push(path);
    }
}

fn read_unit_sidecar_local_roots(sidecar_path: &Path) -> Vec<PathBuf> {
    let text = match fs::read_to_string(sidecar_path) {
        Ok(text) => text,
        Err(_) => return Vec::new(),
    };
    let value: TomlValue = match toml::from_str(&text) {
        Ok(value) => value,
        Err(_) => return Vec::new(),
    };
    let roots = match value
        .get("local_export")
        .and_then(TomlValue::as_table)
        .and_then(|table| table.get("roots"))
        .and_then(TomlValue::as_array)
    {
        Some(roots) => roots,
        None => return Vec::new(),
    };

    let mut resolved = Vec::new();
    let mut seen = HashSet::new();
    let base_dir = sidecar_path.parent().unwrap_or_else(|| Path::new("."));
    for root in roots.iter().filter_map(TomlValue::as_str) {
        let root = root.trim();
        if root.is_empty() {
            continue;
        }
        let path = if Path::new(root).is_absolute() {
            PathBuf::from(root)
        } else {
            base_dir.join(root)
        };
        let normalized = match path.canonicalize() {
            Ok(path) => normalize_windows_path(path),
            Err(_) => normalize_windows_path(path),
        };
        let key = normalized_local_export_path_key(&normalized);
        if seen.insert(key) {
            resolved.push(normalized);
        }
    }
    resolved
}

fn resolve_candidate_from_local_export_roots(
    candidate: &RemoteDependencyCandidate,
    roots: &[PathBuf],
    resolver: &mut LocalExportResolver,
) -> Option<abap_cache::WorkspaceDocument> {
    if roots.is_empty() {
        return None;
    }

    resolve_local_export_dependency_document(
        roots,
        resolver,
        candidate.name.as_str(),
        candidate.kind.as_str(),
    )
}

fn normalized_local_export_path_key(path: &Path) -> String {
    normalize_windows_path(path.to_path_buf())
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase()
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
        "report" => 4,
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

fn render_call_dataflow_report(
    trace: &CallDataflowTrace,
    diagram_format: CallDataflowDiagramFormat,
) -> String {
    let mut out = String::new();
    out.push_str("# Call Dataflow\n\n");
    out.push_str(&format!(
        "- Target: `{}`\n",
        markdown_inline_code(&trace.query.target)
    ));
    if let Some(caller) = trace.query.caller.as_deref() {
        out.push_str(&format!(
            "- Caller filter: `{}`\n",
            markdown_inline_code(caller)
        ));
    }
    if let Some(occurrence) = trace.query.occurrence {
        out.push_str(&format!("- Requested occurrence: `{occurrence}`\n"));
    }
    out.push_str(&format!(
        "- Matches: `{}`\n- Parameters: `{}`\n- Mappings: `{}`\n- Diagram: `{}`\n",
        trace.summary.match_count,
        trace.summary.parameter_count,
        trace.summary.mapping_count,
        diagram_format.as_str()
    ));

    if trace.selected_call.is_none() && !trace.matches.is_empty() {
        out.push_str("\n## Ambiguity\n\n");
        out.push_str(
            "More than one matching call site remained after filtering. Re-run with `--occurrence N`.\n\n",
        );
        out.push_str(&render_call_dataflow_matches_table(&trace.matches));
        return out;
    }

    let Some(selected) = trace.selected_call.as_ref() else {
        out.push_str("\n## Result\n\n");
        out.push_str("No matching call sites were found in the loaded project context.\n");
        return out;
    };

    out.push_str("\n## Selected Call\n\n");
    out.push_str(&render_call_dataflow_selected_call(selected));

    let rich_mermaid = diagram_format == CallDataflowDiagramFormat::RichMermaid;
    if !rich_mermaid {
        out.push_str("\n## Lifecycle\n\n");
    }
    if !rich_mermaid && (trace.lifecycle.nodes.is_empty() || trace.lifecycle.edges.is_empty()) {
        out.push_str("_No lifecycle edges resolved._\n");
    } else if !rich_mermaid {
        match diagram_format {
            CallDataflowDiagramFormat::Ascii => {
                out.push_str("_ASCII tree view optimized for terminal reading._\n\n");
            }
            CallDataflowDiagramFormat::Svg => {
                out.push_str(
                    "_Inline SVG markup. Markdown or HTML renderers show the diagram; plain terminals show XML._\n\n",
                );
            }
            CallDataflowDiagramFormat::Mermaid => {
                out.push_str(
                    "_Mermaid source block. Plain terminals show text; Mermaid-capable renderers turn this into a diagram._\n\n",
                );
            }
            CallDataflowDiagramFormat::RichMermaid => unreachable!(),
        }
        out.push_str(&render_call_dataflow_diagram_block(
            &trace.lifecycle,
            trace.selected_call.as_ref(),
            diagram_format,
        ));
        if trace.summary.synthetic_edge_count > 0 {
            out.push_str(&format!(
                "\nSynthetic edges: `{}`\n",
                trace.summary.synthetic_edge_count
            ));
        }
    }

    out.push_str("\n## Parameters\n");
    if trace.parameter_traces.is_empty() {
        out.push_str("\n_No parameter traces were captured for the selected call._\n");
        return out;
    }
    if rich_mermaid {
        out.push_str(
            "\n_Rich Mermaid renders one graph per target parameter instead of one merged all-parameter graph._\n",
        );
    }
    for parameter in &trace.parameter_traces {
        out.push('\n');
        out.push_str(&render_call_dataflow_parameter(
            parameter,
            if rich_mermaid {
                CallDataflowParameterDiagram::RichMermaid
            } else {
                CallDataflowParameterDiagram::Mermaid
            },
        ));
    }

    out
}

fn render_call_dataflow_diagram_block(
    lifecycle: &CallDataflowLifecycle,
    selected: Option<&CallDataflowSelectedCall>,
    diagram_format: CallDataflowDiagramFormat,
) -> String {
    match diagram_format {
        CallDataflowDiagramFormat::Ascii => {
            let mut out = String::new();
            out.push_str("```text\n");
            out.push_str(&render_call_dataflow_ascii(lifecycle, selected));
            out.push_str("```\n");
            out
        }
        CallDataflowDiagramFormat::Svg => render_call_dataflow_svg(lifecycle, selected),
        CallDataflowDiagramFormat::Mermaid => {
            let mut out = String::new();
            out.push_str("```mermaid\n");
            out.push_str(&render_call_dataflow_mermaid(lifecycle));
            out.push_str("```\n");
            out
        }
        CallDataflowDiagramFormat::RichMermaid => unreachable!(),
    }
}

fn render_call_dataflow_selected_call(selected: &CallDataflowSelectedCall) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "- Target: `{}`\n- Occurrence: `{}`\n- Call site: `{}`\n- Argument count: `{}`\n",
        markdown_inline_code(&selected.target_name),
        selected.occurrence,
        markdown_inline_code(&call_dataflow_display_location(
            &selected.unit_uri,
            &selected.call_range,
        )),
        selected.argument_count
    ));
    if let Some(caller_name) = selected.caller_name.as_deref() {
        out.push_str(&format!(
            "- Caller: `{}`",
            markdown_inline_code(caller_name)
        ));
        if let Some(caller_kind) = selected.caller_kind.as_deref() {
            out.push_str(&format!(" (`{}`)", markdown_inline_code(caller_kind)));
        }
        out.push('\n');
    }
    if let Some(caller_unit_uri) = selected.caller_unit_uri.as_deref() {
        out.push_str(&format!(
            "- Caller unit: `{}`\n",
            markdown_inline_code(caller_unit_uri)
        ));
    }
    out
}

fn render_call_dataflow_matches_table(matches: &[CallDataflowMatch]) -> String {
    let mut out = String::new();
    out.push_str("| Occurrence | Caller | Call Site |\n");
    out.push_str("| --- | --- | --- |\n");
    for matched in matches {
        let caller = matched.caller_name.as_deref().unwrap_or("<unknown>");
        out.push_str(&format!(
            "| {} | {} | {} |\n",
            matched.occurrence,
            markdown_table_cell(caller),
            markdown_table_cell(&call_dataflow_display_location(
                &matched.unit_uri,
                &matched.call_range,
            ))
        ));
    }
    out
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallDataflowParameterDiagram {
    Mermaid,
    RichMermaid,
}

fn render_call_dataflow_parameter(
    parameter: &CallDataflowParameterTrace,
    diagram: CallDataflowParameterDiagram,
) -> String {
    let mut out = String::new();
    let name = parameter.parameter_name.as_deref().unwrap_or("<anonymous>");
    out.push_str(&format!("### `{}`\n\n", markdown_inline_code(name)));
    out.push_str(&format!(
        "- Direction: `{}`\n- Argument: `{}`\n",
        markdown_inline_code(&parameter.direction),
        markdown_inline_code(&parameter.argument_text)
    ));
    if let Some(section) = parameter.section.as_deref() {
        out.push_str(&format!("- Section: `{}`\n", markdown_inline_code(section)));
    }
    if let Some(argument_type) = parameter.argument_type.as_deref() {
        out.push_str(&format!(
            "- Type: `{}`\n",
            markdown_inline_code(argument_type)
        ));
    }

    if !parameter.provenance.nodes.is_empty() && !parameter.provenance.edges.is_empty() {
        match diagram {
            CallDataflowParameterDiagram::Mermaid => {
                out.push_str(
                    "\n#### Detailed Provenance\n\n_Mermaid graph of how the parameter or its fields get populated._\n\n",
                );
                out.push_str("```mermaid\n");
                out.push_str(&render_call_dataflow_parameter_provenance_mermaid(
                    &parameter.provenance,
                ));
                out.push_str("```\n");
            }
            CallDataflowParameterDiagram::RichMermaid => {
                out.push_str(
                    "\n#### Rich Diagram\n\n_Single Mermaid graph for this target parameter only._\n\n",
                );
                out.push_str(&render_call_dataflow_parameter_rich_mermaid_block(
                    parameter,
                ));
            }
        }
    }

    if parameter.field_mappings.is_empty() {
        out.push_str("\n_No mappings resolved._\n");
    } else {
        out.push_str("\n| Target Path | Source | Kind | Location |\n");
        out.push_str("| --- | --- | --- | --- |\n");
        for mapping in &parameter.field_mappings {
            out.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                markdown_table_cell(&mapping.target_path),
                markdown_table_cell(&mapping.source_display),
                markdown_table_cell(&mapping.source_kind),
                markdown_table_cell(&call_dataflow_mapping_location(mapping))
            ));
        }
    }

    if !parameter.notes.is_empty() {
        out.push_str("\nNotes:\n");
        for note in &parameter.notes {
            out.push_str(&format!("- {}\n", markdown_table_cell(note)));
        }
    }

    out
}

fn render_call_dataflow_parameter_rich_mermaid_block(
    parameter: &CallDataflowParameterTrace,
) -> String {
    let mut out = String::new();
    out.push_str("```mermaid\n");
    out.push_str(&render_call_dataflow_parameter_rich_mermaid(parameter));
    out.push_str("```\n");
    out
}

fn render_call_dataflow_parameter_rich_mermaid(parameter: &CallDataflowParameterTrace) -> String {
    let simplified =
        compact_rich_mermaid_diagram_noise(simplify_rich_mermaid_provenance(parameter));

    let mut out = String::new();
    out.push_str("flowchart LR\n");

    let mut query_nodes = Vec::new();
    let mut state_nodes = Vec::new();

    for node in &simplified.nodes {
        out.push_str(&format!(
            "  {}[\"{}\"]\n",
            node.id,
            mermaid_node_label(&rich_mermaid_rendered_view_node_label(node, true))
        ));
        match node.kind.as_str() {
            "sql_query" => query_nodes.push(node.id.clone()),
            _ => state_nodes.push(node.id.clone()),
        }
    }

    for edge in &simplified.edges {
        if edge.label.is_empty() {
            out.push_str(&format!("  {} --> {}\n", edge.source, edge.target));
        } else {
            out.push_str(&format!(
                "  {} -->|\"{}\"| {}\n",
                edge.source,
                mermaid_label(&edge.label),
                edge.target
            ));
        }
    }

    out.push_str("  classDef query fill:#eef6ff,stroke:#1d4ed8,color:#0f172a;\n");
    out.push_str("  classDef state fill:#f8fafc,stroke:#475569,color:#0f172a;\n");
    out.push_str("  classDef warn fill:#fee2e2,stroke:#b91c1c,color:#7f1d1d;\n");

    if !query_nodes.is_empty() {
        out.push_str(&format!("  class {} query;\n", query_nodes.join(",")));
    }
    if !state_nodes.is_empty() {
        out.push_str(&format!("  class {} state;\n", state_nodes.join(",")));
    }

    out
}

fn compact_rich_mermaid_diagram_noise(
    view: RichMermaidProvenanceView,
) -> RichMermaidProvenanceView {
    let node_by_id: HashMap<_, _> = view
        .nodes
        .iter()
        .map(|node| (node.id.as_str(), node))
        .collect();
    let mut inbound = HashMap::<&str, Vec<&RichMermaidProvenanceViewEdge>>::new();
    let mut outbound = HashMap::<&str, Vec<&RichMermaidProvenanceViewEdge>>::new();
    for edge in &view.edges {
        inbound.entry(edge.target.as_str()).or_default().push(edge);
        outbound.entry(edge.source.as_str()).or_default().push(edge);
    }

    let removed_ids: HashSet<_> = view
        .nodes
        .iter()
        .filter(|node| rich_mermaid_remove_compacted_node(node, &node_by_id, &inbound, &outbound))
        .map(|node| node.id.clone())
        .collect();
    if removed_ids.is_empty() {
        return view;
    }

    let kept_nodes: Vec<_> = view
        .nodes
        .into_iter()
        .filter(|node| !removed_ids.contains(node.id.as_str()))
        .collect();
    let kept_ids: HashSet<_> = kept_nodes.iter().map(|node| node.id.as_str()).collect();

    let mut seen_edges = BTreeSet::<(String, String, String)>::new();
    let mut edges = Vec::<RichMermaidProvenanceViewEdge>::new();

    for edge in &view.edges {
        if kept_ids.contains(edge.source.as_str())
            && kept_ids.contains(edge.target.as_str())
            && seen_edges.insert((edge.source.clone(), edge.target.clone(), edge.label.clone()))
        {
            edges.push(edge.clone());
        }
    }

    for kept in &kept_nodes {
        let mut stack = Vec::<&str>::new();
        let mut visited = HashSet::<&str>::new();
        for edge in outbound.get(kept.id.as_str()).into_iter().flatten() {
            if removed_ids.contains(edge.target.as_str()) {
                stack.push(edge.target.as_str());
            }
        }

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }
            for edge in outbound.get(current).into_iter().flatten() {
                if removed_ids.contains(edge.target.as_str()) {
                    stack.push(edge.target.as_str());
                    continue;
                }
                if !kept_ids.contains(edge.target.as_str()) || kept.id == edge.target {
                    continue;
                }
                if seen_edges.insert((kept.id.clone(), edge.target.clone(), String::new())) {
                    edges.push(RichMermaidProvenanceViewEdge {
                        source: kept.id.clone(),
                        target: edge.target.clone(),
                        label: String::new(),
                    });
                }
            }
        }
    }

    RichMermaidProvenanceView {
        nodes: kept_nodes,
        edges,
    }
}

fn rich_mermaid_remove_compacted_node(
    node: &RichMermaidProvenanceViewNode,
    node_by_id: &HashMap<&str, &RichMermaidProvenanceViewNode>,
    inbound: &HashMap<&str, Vec<&RichMermaidProvenanceViewEdge>>,
    outbound: &HashMap<&str, Vec<&RichMermaidProvenanceViewEdge>>,
) -> bool {
    if rich_mermaid_compacted_node_kind(node.kind.as_str()) {
        return true;
    }
    if node.kind != "symbol" {
        return false;
    }

    let neighbors = inbound
        .get(node.id.as_str())
        .into_iter()
        .flatten()
        .map(|edge| edge.source.as_str())
        .chain(
            outbound
                .get(node.id.as_str())
                .into_iter()
                .flatten()
                .map(|edge| edge.target.as_str()),
        );

    let mut saw_neighbor = false;
    for neighbor_id in neighbors {
        let Some(neighbor) = node_by_id.get(neighbor_id).copied() else {
            return false;
        };
        saw_neighbor = true;
        if !rich_mermaid_sql_related_kind(neighbor.kind.as_str()) {
            return false;
        }
    }

    saw_neighbor
}

fn rich_mermaid_sql_helper_kind(kind: &str) -> bool {
    matches!(
        kind,
        "sql_source" | "sql_predicate" | "sql_target" | "sql_source_field" | "sql_target_field"
    )
}

fn rich_mermaid_compacted_node_kind(kind: &str) -> bool {
    matches!(
        kind,
        "perform_binding" | "perform_write" | "read_table_binding" | "field_symbol_binding"
    ) || rich_mermaid_sql_helper_kind(kind)
}

fn rich_mermaid_sql_related_kind(kind: &str) -> bool {
    kind == "sql_query" || rich_mermaid_sql_helper_kind(kind)
}

fn render_call_dataflow_parameter_provenance_mermaid(
    provenance: &CallDataflowProvenanceGraph,
) -> String {
    let mut out = String::new();
    out.push_str("flowchart LR\n");
    for node in &provenance.nodes {
        out.push_str(&format!(
            "  {}[\"{}\"]\n",
            node.id,
            mermaid_node_label(&call_dataflow_provenance_node_label(node))
        ));
    }
    for edge in &provenance.edges {
        let label = call_dataflow_provenance_edge_label(edge);
        if label.is_empty() {
            out.push_str(&format!("  {} --> {}\n", edge.source, edge.target));
        } else {
            out.push_str(&format!(
                "  {} -->|\"{}\"| {}\n",
                edge.source,
                mermaid_label(&label),
                edge.target
            ));
        }
    }

    let mut target_nodes = Vec::new();
    let mut transform_nodes = Vec::new();
    let mut query_nodes = Vec::new();
    let mut source_nodes = Vec::new();
    for node in &provenance.nodes {
        match node.kind.as_str() {
            "parameter" | "target_value" | "target_field" | "target_table_row"
            | "target_table_field" => target_nodes.push(node.id.clone()),
            "sql_query" | "sql_source" | "sql_predicate" | "sql_source_field"
            | "sql_target_field" => query_nodes.push(node.id.clone()),
            "assignment"
            | "append_row"
            | "loop_binding"
            | "perform_binding"
            | "perform_write"
            | "read_table_binding"
            | "field_symbol_binding" => transform_nodes.push(node.id.clone()),
            _ => source_nodes.push(node.id.clone()),
        }
    }
    if !target_nodes.is_empty() {
        out.push_str("  classDef target fill:#dcfce7,stroke:#166534,color:#14532d;\n");
        out.push_str(&format!("  class {} target;\n", target_nodes.join(",")));
    }
    if !transform_nodes.is_empty() {
        out.push_str("  classDef transform fill:#fff7ed,stroke:#c2410c,color:#7c2d12;\n");
        out.push_str(&format!(
            "  class {} transform;\n",
            transform_nodes.join(",")
        ));
    }
    if !query_nodes.is_empty() {
        out.push_str("  classDef query fill:#dbeafe,stroke:#1d4ed8,color:#1e3a8a;\n");
        out.push_str(&format!("  class {} query;\n", query_nodes.join(",")));
    }
    if !source_nodes.is_empty() {
        out.push_str("  classDef source fill:#f8fafc,stroke:#475569,color:#0f172a;\n");
        out.push_str(&format!("  class {} source;\n", source_nodes.join(",")));
    }
    out
}

fn rich_mermaid_rendered_view_node_label(
    node: &RichMermaidProvenanceViewNode,
    strip_scope_name: bool,
) -> String {
    if !strip_scope_name {
        return node.label.clone();
    }

    match node.kind.as_str() {
        "sql_query" => rich_mermaid_sql_query_label(node),
        "perform_binding" => {
            let compact = node
                .raw_label
                .split_once(" -> ")
                .map(|(source, target)| {
                    let target = target
                        .split_once('.')
                        .map(|(_, field)| field)
                        .unwrap_or(target);
                    format!("{source} -> {target}")
                })
                .unwrap_or_else(|| node.raw_label.clone());
            rich_mermaid_format_provenance_label(
                "perform bind",
                &compact,
                node.unit_uri.as_deref(),
                node.range.as_ref(),
            )
        }
        "perform_write" => {
            let compact = node
                .raw_label
                .split_once(" writes ")
                .map(|(_, target)| target)
                .unwrap_or(node.raw_label.as_str());
            rich_mermaid_format_provenance_label(
                "perform write",
                compact,
                node.unit_uri.as_deref(),
                node.range.as_ref(),
            )
        }
        "collapsed_summary" => rich_mermaid_compact_summary_label(&node.raw_label),
        _ => node.label.clone(),
    }
}

fn rich_mermaid_sql_query_label(node: &RichMermaidProvenanceViewNode) -> String {
    let mut query = node
        .raw_label
        .lines()
        .filter(|line| !line.trim_start().starts_with("HOSTS "))
        .collect::<Vec<_>>()
        .join("\n");
    query = truncate_display(&query, 320);

    let mut label = format!("sql query: {query}");
    if let (Some(unit_uri), Some(range)) = (node.unit_uri.as_deref(), node.range.as_ref()) {
        label.push_str("\n@ ");
        label.push_str(&call_dataflow_short_location(unit_uri, range));
    }
    label
}

fn rich_mermaid_format_provenance_label(
    prefix: &str,
    raw_label: &str,
    unit_uri: Option<&str>,
    range: Option<&abap_cache::CallDataflowByteRange>,
) -> String {
    let mut label = format!("{prefix}: {}", truncate_display(raw_label, 96));
    if let (Some(unit_uri), Some(range)) = (unit_uri, range) {
        label.push_str(" @ ");
        label.push_str(&call_dataflow_short_location(unit_uri, range));
    }
    label
}

fn rich_mermaid_compact_summary_label(raw_label: &str) -> String {
    let Some(rest) = raw_label.strip_prefix("FORM ") else {
        return raw_label.to_string();
    };
    let Some((_, body)) = rest.split_once(':') else {
        return raw_label.to_string();
    };
    let body = body.trim_start_matches('\n');
    if body.is_empty() {
        "updates:".to_string()
    } else {
        format!("updates:\n{body}")
    }
}

#[derive(Debug, Clone)]
struct RichMermaidProvenanceView {
    nodes: Vec<RichMermaidProvenanceViewNode>,
    edges: Vec<RichMermaidProvenanceViewEdge>,
}

#[derive(Debug, Clone)]
struct RichMermaidProvenanceViewNode {
    id: String,
    kind: String,
    label: String,
    raw_label: String,
    unit_uri: Option<String>,
    range: Option<abap_cache::CallDataflowByteRange>,
}

#[derive(Debug, Clone)]
struct RichMermaidProvenanceViewEdge {
    source: String,
    target: String,
    label: String,
}

#[derive(Debug, Clone)]
struct RichMermaidCollapseGroup {
    summary_id: String,
    label: String,
    unit_uri: Option<String>,
    range: Option<abap_cache::CallDataflowByteRange>,
    member_ids: HashSet<String>,
}

fn simplify_rich_mermaid_provenance(
    parameter: &CallDataflowParameterTrace,
) -> RichMermaidProvenanceView {
    let groups = build_rich_mermaid_collapse_groups(parameter);
    if groups.is_empty() {
        return RichMermaidProvenanceView {
            nodes: parameter
                .provenance
                .nodes
                .iter()
                .map(|node| RichMermaidProvenanceViewNode {
                    id: node.id.clone(),
                    kind: node.kind.clone(),
                    label: call_dataflow_provenance_node_label(node),
                    raw_label: node.label.clone(),
                    unit_uri: node.unit_uri.clone(),
                    range: node.range.clone(),
                })
                .collect(),
            edges: parameter
                .provenance
                .edges
                .iter()
                .map(|edge| RichMermaidProvenanceViewEdge {
                    source: edge.source.clone(),
                    target: edge.target.clone(),
                    label: call_dataflow_provenance_edge_label(edge),
                })
                .collect(),
        };
    }

    let mut member_to_group = HashMap::<&str, &RichMermaidCollapseGroup>::new();
    for group in &groups {
        for member_id in &group.member_ids {
            member_to_group.insert(member_id.as_str(), group);
        }
    }

    let mut nodes = Vec::new();
    for node in &parameter.provenance.nodes {
        if member_to_group.contains_key(node.id.as_str()) {
            continue;
        }
        nodes.push(RichMermaidProvenanceViewNode {
            id: node.id.clone(),
            kind: node.kind.clone(),
            label: call_dataflow_provenance_node_label(node),
            raw_label: node.label.clone(),
            unit_uri: node.unit_uri.clone(),
            range: node.range.clone(),
        });
    }
    for group in &groups {
        nodes.push(RichMermaidProvenanceViewNode {
            id: group.summary_id.clone(),
            kind: "collapsed_summary".to_string(),
            label: group.label.clone(),
            raw_label: group.label.clone(),
            unit_uri: group.unit_uri.clone(),
            range: group.range.clone(),
        });
    }

    let mut seen_edges = BTreeSet::<(String, String, String)>::new();
    let mut edges = Vec::new();
    for edge in &parameter.provenance.edges {
        let source_group = member_to_group.get(edge.source.as_str()).copied();
        let target_group = member_to_group.get(edge.target.as_str()).copied();
        let source = source_group
            .map(|group| group.summary_id.clone())
            .unwrap_or_else(|| edge.source.clone());
        let target = target_group
            .map(|group| group.summary_id.clone())
            .unwrap_or_else(|| edge.target.clone());
        if source == target {
            continue;
        }
        let label =
            rich_mermaid_summary_edge_label(edge, source_group.is_some() || target_group.is_some());
        if !seen_edges.insert((source.clone(), target.clone(), label.clone())) {
            continue;
        }
        edges.push(RichMermaidProvenanceViewEdge {
            source,
            target,
            label,
        });
    }

    RichMermaidProvenanceView { nodes, edges }
}

fn build_rich_mermaid_collapse_groups(
    parameter: &CallDataflowParameterTrace,
) -> Vec<RichMermaidCollapseGroup> {
    let node_by_id: HashMap<_, _> = parameter
        .provenance
        .nodes
        .iter()
        .map(|node| (node.id.as_str(), node))
        .collect();
    let mut inbound = HashMap::<&str, Vec<&abap_cache::CallDataflowProvenanceEdge>>::new();
    let mut outbound = HashMap::<&str, Vec<&abap_cache::CallDataflowProvenanceEdge>>::new();
    for edge in &parameter.provenance.edges {
        inbound.entry(edge.target.as_str()).or_default().push(edge);
        outbound.entry(edge.source.as_str()).or_default().push(edge);
    }

    let mut groups = Vec::new();
    let mut claimed = HashSet::<String>::new();
    for anchor in &parameter.provenance.nodes {
        if !matches!(anchor.kind.as_str(), "parameter" | "target_table_row") {
            continue;
        }

        let mut target_nodes = Vec::<&abap_cache::CallDataflowProvenanceNode>::new();
        let mut target_ids = HashSet::<String>::new();
        let mut writer_ids = HashSet::<String>::new();
        let mut member_ids = HashSet::<String>::new();

        for edge in inbound.get(anchor.id.as_str()).into_iter().flatten() {
            let Some(source_node) = node_by_id.get(edge.source.as_str()).copied() else {
                continue;
            };
            if !matches!(edge.kind.as_str(), "populates") {
                continue;
            }
            if !matches!(
                source_node.kind.as_str(),
                "target_field" | "target_table_field"
            ) {
                continue;
            }
            if claimed.contains(source_node.id.as_str())
                || !target_ids.insert(source_node.id.clone())
            {
                continue;
            }
            member_ids.insert(source_node.id.clone());
            target_nodes.push(source_node);

            for writer_edge in inbound.get(source_node.id.as_str()).into_iter().flatten() {
                if !matches!(writer_edge.kind.as_str(), "writes" | "appends" | "produces") {
                    continue;
                }
                let Some(writer_node) = node_by_id.get(writer_edge.source.as_str()).copied() else {
                    continue;
                };
                if !rich_mermaid_collapsible_writer_kind(writer_node.kind.as_str())
                    || claimed.contains(writer_node.id.as_str())
                    || !writer_ids.insert(writer_node.id.clone())
                {
                    continue;
                }
                member_ids.insert(writer_node.id.clone());
            }
        }

        if target_nodes.len() < 4 {
            continue;
        }

        rich_mermaid_absorb_leaf_constants(&mut member_ids, &node_by_id, &outbound);

        let field_details =
            rich_mermaid_collapsed_field_details(&target_nodes, &node_by_id, &inbound);
        let field_summary = rich_mermaid_collapsed_field_summary(&target_nodes);
        if field_summary.is_empty() && field_details.is_empty() {
            continue;
        }

        let scope_name =
            rich_mermaid_collapsed_scope_name(anchor, &target_nodes, &node_by_id, &outbound);
        let mut label = if let Some(scope_name) = scope_name.as_deref() {
            format!("FORM {scope_name}:")
        } else if anchor.kind == "target_table_row" {
            "row build:".to_string()
        } else {
            "field updates:".to_string()
        };
        if field_details.is_empty() {
            label.push(' ');
            label.push_str(&field_summary);
        } else {
            for detail in field_details {
                label.push('\n');
                label.push_str(&detail);
            }
        }
        let (unit_uri, range, location) =
            rich_mermaid_collapsed_location(member_ids.iter(), &node_by_id);
        if let Some(location) = location {
            label.push_str("\n@ ");
            label.push_str(&location);
        }

        claimed.extend(member_ids.iter().cloned());
        groups.push(RichMermaidCollapseGroup {
            summary_id: format!("collapsed_{}", groups.len()),
            label,
            unit_uri,
            range,
            member_ids,
        });
    }

    groups
}

fn rich_mermaid_collapsible_writer_kind(kind: &str) -> bool {
    matches!(kind, "assignment" | "append_row" | "call_output")
}

fn rich_mermaid_collapsed_field_summary(
    target_nodes: &[&abap_cache::CallDataflowProvenanceNode],
) -> String {
    let mut names = Vec::<String>::new();
    let mut seen = HashSet::<String>::new();
    for node in target_nodes {
        let name = rich_mermaid_target_field_name(&node.label);
        if !seen.insert(name.to_ascii_lowercase()) {
            continue;
        }
        names.push(name);
    }
    match names.len() {
        0 => String::new(),
        1..=6 => names.join(", "),
        _ => format!("{}, +{} more", names[..6].join(", "), names.len() - 6),
    }
}

fn rich_mermaid_collapsed_field_details(
    target_nodes: &[&abap_cache::CallDataflowProvenanceNode],
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowProvenanceNode>,
    inbound: &HashMap<&str, Vec<&abap_cache::CallDataflowProvenanceEdge>>,
) -> Vec<String> {
    let mut details = Vec::<String>::new();
    let mut seen_fields = HashSet::<String>::new();

    for node in target_nodes {
        let field_name = rich_mermaid_target_field_name(&node.label);
        if !seen_fields.insert(field_name.to_ascii_lowercase()) {
            continue;
        }

        let mut values = Vec::<String>::new();
        let mut seen_values = HashSet::<String>::new();
        for edge in inbound.get(node.id.as_str()).into_iter().flatten() {
            if !matches!(edge.kind.as_str(), "writes" | "appends" | "produces") {
                continue;
            }
            let Some(writer_node) = node_by_id.get(edge.source.as_str()).copied() else {
                continue;
            };
            let Some(value) = rich_mermaid_collapsed_field_value(writer_node) else {
                continue;
            };
            let normalized = value.to_ascii_lowercase();
            if !seen_values.insert(normalized) {
                continue;
            }
            values.push(value);
        }

        if values.is_empty() {
            details.push(format!("{field_name}=..."));
            continue;
        }

        details.push(format!(
            "{field_name}={}",
            truncate_display(&values.join(" | "), 72)
        ));
    }

    const MAX_FIELD_DETAILS: usize = 8;
    if details.len() > MAX_FIELD_DETAILS {
        let remaining = details.len() - MAX_FIELD_DETAILS;
        details.truncate(MAX_FIELD_DETAILS);
        details.push(format!("+{remaining} more"));
    }

    details
}

fn rich_mermaid_collapsed_field_value(
    node: &abap_cache::CallDataflowProvenanceNode,
) -> Option<String> {
    match node.kind.as_str() {
        "assignment" => rich_mermaid_assignment_rhs(
            node.statement_text
                .as_deref()
                .unwrap_or(node.label.as_str()),
        ),
        "call_output" | "literal_or_expression" | "composite_expression" => {
            Some(truncate_display(&node.label.replace('\n', " "), 72))
        }
        other if rich_mermaid_collapsible_writer_kind(other) => {
            Some(truncate_display(&node.label.replace('\n', " "), 72))
        }
        _ => None,
    }
}

fn rich_mermaid_assignment_rhs(statement: &str) -> Option<String> {
    let trimmed = statement.trim().trim_end_matches('.');
    let (_, rhs) = trimmed.split_once('=')?;
    let rhs = rhs.trim();
    (!rhs.is_empty()).then(|| rhs.to_string())
}

fn rich_mermaid_target_field_name(label: &str) -> String {
    label.rsplit('.').next().unwrap_or(label).trim().to_string()
}

fn rich_mermaid_absorb_leaf_constants(
    member_ids: &mut HashSet<String>,
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowProvenanceNode>,
    outbound: &HashMap<&str, Vec<&abap_cache::CallDataflowProvenanceEdge>>,
) {
    loop {
        let mut changed = false;
        for (node_id, node) in node_by_id {
            if node.kind != "constant" || member_ids.contains(*node_id) {
                continue;
            }
            let Some(edges) = outbound.get(node_id) else {
                continue;
            };
            if edges.is_empty() {
                continue;
            }
            if edges
                .iter()
                .all(|edge| edge.kind == "flows_to" && member_ids.contains(edge.target.as_str()))
            {
                member_ids.insert((*node_id).to_string());
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

fn rich_mermaid_collapsed_scope_name(
    anchor: &abap_cache::CallDataflowProvenanceNode,
    target_nodes: &[&abap_cache::CallDataflowProvenanceNode],
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowProvenanceNode>,
    outbound: &HashMap<&str, Vec<&abap_cache::CallDataflowProvenanceEdge>>,
) -> Option<String> {
    let mut scopes = Vec::<String>::new();
    let mut seen = HashSet::<String>::new();
    for node_id in
        std::iter::once(anchor.id.as_str()).chain(target_nodes.iter().map(|node| node.id.as_str()))
    {
        for edge in outbound.get(node_id).into_iter().flatten() {
            let Some(target_node) = node_by_id.get(edge.target.as_str()).copied() else {
                continue;
            };
            if target_node.kind != "perform_write" {
                continue;
            }
            let Some(scope_name) = target_node.label.split(" writes ").next() else {
                continue;
            };
            if !seen.insert(scope_name.to_ascii_lowercase()) {
                continue;
            }
            scopes.push(scope_name.to_string());
        }
    }
    (scopes.len() == 1).then(|| scopes.remove(0))
}

fn rich_mermaid_collapsed_location<'a>(
    member_ids: impl Iterator<Item = &'a String>,
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowProvenanceNode>,
) -> (
    Option<String>,
    Option<abap_cache::CallDataflowByteRange>,
    Option<String>,
) {
    let mut unit_uri = None::<&str>;
    let mut start = None::<usize>;
    let mut end = None::<usize>;

    for member_id in member_ids {
        let Some(node) = node_by_id.get(member_id.as_str()).copied() else {
            continue;
        };
        let (Some(node_unit_uri), Some(range)) = (node.unit_uri.as_deref(), node.range.as_ref())
        else {
            continue;
        };
        match unit_uri {
            None => unit_uri = Some(node_unit_uri),
            Some(existing) if existing == node_unit_uri => {}
            Some(_) => return (None, None, None),
        }
        start = Some(start.map_or(range.start, |value| value.min(range.start)));
        end = Some(end.map_or(range.end, |value| value.max(range.end)));
    }

    let (Some(unit_uri), Some(start), Some(end)) = (unit_uri, start, end) else {
        return (None, None, None);
    };
    let range = abap_cache::CallDataflowByteRange { start, end };
    (
        Some(unit_uri.to_string()),
        Some(range.clone()),
        Some(call_dataflow_short_location(unit_uri, &range)),
    )
}

fn rich_mermaid_summary_edge_label(
    edge: &abap_cache::CallDataflowProvenanceEdge,
    touches_summary: bool,
) -> String {
    if touches_summary
        && matches!(
            edge.kind.as_str(),
            "writes" | "appends" | "produces" | "binds_to" | "flows_to" | "populates"
        )
    {
        String::new()
    } else {
        call_dataflow_provenance_edge_label(edge)
    }
}

struct CallDataflowLifecycleIndex<'a> {
    node_by_id: HashMap<&'a str, &'a abap_cache::CallDataflowLifecycleNode>,
    outbound: HashMap<&'a str, Vec<&'a abap_cache::CallDataflowLifecycleEdge>>,
    roots: Vec<&'a abap_cache::CallDataflowLifecycleNode>,
}

struct CallDataflowTreeEntry {
    kind: String,
    name: String,
    edge_label: Option<String>,
    depth: usize,
    parent_index: Option<usize>,
    node_synthetic: bool,
    edge_synthetic: bool,
    selected: bool,
}

fn build_call_dataflow_lifecycle_index<'a>(
    lifecycle: &'a CallDataflowLifecycle,
) -> CallDataflowLifecycleIndex<'a> {
    let active_node_ids = call_dataflow_active_node_ids(lifecycle);
    let node_by_id: HashMap<_, _> = lifecycle
        .nodes
        .iter()
        .filter(|node| active_node_ids.contains(node.id.as_str()))
        .map(|node| (node.id.as_str(), node))
        .collect();
    let mut outbound = HashMap::<&str, Vec<&abap_cache::CallDataflowLifecycleEdge>>::new();
    let mut inbound_count = HashMap::<&str, usize>::new();
    for node in node_by_id.values() {
        inbound_count.insert(node.id.as_str(), 0);
    }
    for edge in &lifecycle.edges {
        if !node_by_id.contains_key(edge.source.as_str()) {
            continue;
        }
        outbound.entry(edge.source.as_str()).or_default().push(edge);
        if node_by_id.contains_key(edge.target.as_str()) {
            *inbound_count.entry(edge.target.as_str()).or_insert(0) += 1;
        }
    }
    for edges in outbound.values_mut() {
        edges.sort_by(|left, right| {
            ascii_edge_sort_key(left, &node_by_id).cmp(&ascii_edge_sort_key(right, &node_by_id))
        });
    }

    let mut roots: Vec<_> = lifecycle
        .nodes
        .iter()
        .filter(|node| {
            node_by_id.contains_key(node.id.as_str())
                && inbound_count.get(node.id.as_str()).copied().unwrap_or(0) == 0
                && outbound.contains_key(node.id.as_str())
        })
        .collect();
    if roots.is_empty() {
        roots = lifecycle
            .nodes
            .iter()
            .filter(|node| outbound.contains_key(node.id.as_str()))
            .collect();
    }
    roots.sort_by(|left, right| ascii_node_sort_key(left).cmp(&ascii_node_sort_key(right)));

    CallDataflowLifecycleIndex {
        node_by_id,
        outbound,
        roots,
    }
}

fn render_call_dataflow_ascii(
    lifecycle: &CallDataflowLifecycle,
    selected: Option<&CallDataflowSelectedCall>,
) -> String {
    let index = build_call_dataflow_lifecycle_index(lifecycle);
    let mut out = String::new();
    let selected_target_id = selected.and_then(|selected| selected.target_node_id.as_deref());
    for (idx, root) in index.roots.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        out.push_str(&ascii_node_label(root, selected_target_id));
        out.push('\n');
        let mut path = BTreeSet::new();
        path.insert(root.id.clone());
        render_call_dataflow_ascii_children(
            root.id.as_str(),
            "",
            &index.outbound,
            &index.node_by_id,
            selected_target_id,
            &mut path,
            &mut out,
        );
    }
    out
}

fn render_call_dataflow_svg(
    lifecycle: &CallDataflowLifecycle,
    selected: Option<&CallDataflowSelectedCall>,
) -> String {
    let entries = build_call_dataflow_tree_entries(lifecycle, selected);
    if entries.is_empty() {
        return String::new();
    }

    const MARGIN_X: usize = 32;
    const MARGIN_Y: usize = 32;
    const BOX_WIDTH: usize = 240;
    const BOX_HEIGHT: usize = 64;
    const DEPTH_GAP: usize = 320;
    const ROW_GAP: usize = 96;
    const EDGE_BEND: usize = 28;

    let max_depth = entries.iter().map(|entry| entry.depth).max().unwrap_or(0);
    let width = MARGIN_X * 2 + BOX_WIDTH + max_depth * DEPTH_GAP;
    let height = MARGIN_Y * 2 + BOX_HEIGHT + entries.len().saturating_sub(1) * ROW_GAP;
    let positions: Vec<_> = entries
        .iter()
        .enumerate()
        .map(|(row, entry)| (MARGIN_X + entry.depth * DEPTH_GAP, MARGIN_Y + row * ROW_GAP))
        .collect();

    let mut out = String::new();
    out.push_str(&format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\" viewBox=\"0 0 {width} {height}\" role=\"img\" aria-labelledby=\"call-dataflow-title\">\n"
    ));
    out.push_str("  <title id=\"call-dataflow-title\">Call dataflow lifecycle</title>\n");
    out.push_str("  <defs>\n");
    out.push_str("    <marker id=\"call-dataflow-arrow\" viewBox=\"0 0 10 10\" refX=\"9\" refY=\"5\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto-start-reverse\">\n");
    out.push_str("      <path d=\"M 0 0 L 10 5 L 0 10 z\" fill=\"#475569\"/>\n");
    out.push_str("    </marker>\n");
    out.push_str("  </defs>\n");
    out.push_str("  <style>\n");
    out.push_str("    .canvas { fill: #f8fafc; }\n");
    out.push_str("    .node { fill: #ffffff; stroke: #1f2937; stroke-width: 1.5; }\n");
    out.push_str("    .node.selected { fill: #dcfce7; stroke: #166534; stroke-width: 2; }\n");
    out.push_str("    .node.synthetic { fill: #fef3c7; stroke: #b45309; }\n");
    out.push_str("    .edge { fill: none; stroke: #475569; stroke-width: 1.6; }\n");
    out.push_str("    .edge.synthetic { stroke: #b45309; stroke-dasharray: 7 4; }\n");
    out.push_str("    .edge-label { fill: #334155; font: 12px 'Segoe UI', sans-serif; }\n");
    out.push_str("    .kind { fill: #64748b; font: 11px 'Segoe UI', sans-serif; text-transform: uppercase; letter-spacing: 0.08em; }\n");
    out.push_str(
        "    .name { fill: #0f172a; font: 14px 'Segoe UI', sans-serif; font-weight: 600; }\n",
    );
    out.push_str("    .badge { fill: #475569; font: 11px 'Segoe UI', sans-serif; }\n");
    out.push_str("  </style>\n");
    out.push_str(&format!(
        "  <rect class=\"canvas\" x=\"0\" y=\"0\" width=\"{width}\" height=\"{height}\" rx=\"18\" ry=\"18\"/>\n"
    ));

    for (idx, entry) in entries.iter().enumerate() {
        let Some(parent_index) = entry.parent_index else {
            continue;
        };
        let (parent_x, parent_y) = positions[parent_index];
        let (x, y) = positions[idx];
        let start_x = parent_x + BOX_WIDTH;
        let start_y = parent_y + BOX_HEIGHT / 2;
        let end_x = x;
        let end_y = y + BOX_HEIGHT / 2;
        let bend_x = start_x + EDGE_BEND;
        let edge_class = if entry.edge_synthetic {
            "edge synthetic"
        } else {
            "edge"
        };
        out.push_str(&format!(
            "  <path class=\"{edge_class}\" d=\"M {start_x} {start_y} L {bend_x} {start_y} L {bend_x} {end_y} L {end_x} {end_y}\" marker-end=\"url(#call-dataflow-arrow)\"/>\n"
        ));
        if let Some(label) = entry.edge_label.as_deref() {
            let label_y = if start_y == end_y {
                start_y.saturating_sub(10)
            } else {
                ((start_y + end_y) / 2).saturating_sub(6)
            };
            out.push_str(&format!(
                "  <text class=\"edge-label\" x=\"{}\" y=\"{}\">{}</text>\n",
                bend_x + 8,
                label_y,
                xml_text(label)
            ));
        }
    }

    for (idx, entry) in entries.iter().enumerate() {
        let (x, y) = positions[idx];
        let mut classes = vec!["node"];
        if entry.selected {
            classes.push("selected");
        }
        if entry.node_synthetic {
            classes.push("synthetic");
        }
        out.push_str(&format!("  <g transform=\"translate({x},{y})\">\n"));
        out.push_str(&format!(
            "    <rect class=\"{}\" width=\"{BOX_WIDTH}\" height=\"{BOX_HEIGHT}\" rx=\"12\" ry=\"12\"/>\n",
            classes.join(" ")
        ));
        out.push_str(&format!(
            "    <text class=\"kind\" x=\"16\" y=\"22\">{}</text>\n",
            xml_text(&entry.kind)
        ));
        out.push_str(&format!(
            "    <text class=\"name\" x=\"16\" y=\"44\">{}</text>\n",
            xml_text(&entry.name)
        ));
        let badge = call_dataflow_svg_badge(entry);
        if !badge.is_empty() {
            out.push_str(&format!(
                "    <text class=\"badge\" x=\"{}\" y=\"22\" text-anchor=\"end\">{}</text>\n",
                BOX_WIDTH.saturating_sub(14),
                xml_text(&badge)
            ));
        }
        out.push_str("  </g>\n");
    }

    out.push_str("</svg>\n");
    out
}

fn render_call_dataflow_ascii_children(
    node_id: &str,
    prefix: &str,
    outbound: &HashMap<&str, Vec<&abap_cache::CallDataflowLifecycleEdge>>,
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowLifecycleNode>,
    selected_target_id: Option<&str>,
    path: &mut BTreeSet<String>,
    out: &mut String,
) {
    let Some(edges) = outbound.get(node_id) else {
        return;
    };
    for (idx, edge) in edges.iter().enumerate() {
        let is_last = idx + 1 == edges.len();
        let branch = if is_last { "`-- " } else { "|-- " };
        let child_prefix = if is_last { "    " } else { "|   " };
        out.push_str(prefix);
        out.push_str(branch);
        out.push_str(&ascii_edge_label(edge, node_by_id, selected_target_id));
        out.push('\n');

        if edge.target.is_empty() || !path.insert(edge.target.clone()) {
            continue;
        }
        let next_prefix = format!("{prefix}{child_prefix}");
        render_call_dataflow_ascii_children(
            edge.target.as_str(),
            &next_prefix,
            outbound,
            node_by_id,
            selected_target_id,
            path,
            out,
        );
        path.remove(edge.target.as_str());
    }
}

fn ascii_edge_label(
    edge: &abap_cache::CallDataflowLifecycleEdge,
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowLifecycleNode>,
    selected_target_id: Option<&str>,
) -> String {
    let mut edge_desc = edge.kind.clone();
    if let Some(label) = edge.label.as_deref()
        && edge.kind != "selected_call"
    {
        edge_desc.push(' ');
        edge_desc.push_str(label);
    }

    let target_desc = node_by_id
        .get(edge.target.as_str())
        .map(|node| ascii_node_label(node, selected_target_id))
        .unwrap_or_else(|| "<unresolved>".to_string());
    let mut out = format!("{edge_desc} -> {target_desc}");
    if edge.synthetic {
        out.push_str(" [synthetic]");
    }
    out
}

fn ascii_node_label(
    node: &abap_cache::CallDataflowLifecycleNode,
    selected_target_id: Option<&str>,
) -> String {
    let mut label = format!("{} {}", node.kind, node.name);
    if selected_target_id == Some(node.id.as_str()) {
        label.push_str(" [selected]");
    }
    if node.synthetic {
        label.push_str(" [synthetic]");
    }
    label
}

fn ascii_edge_sort_key(
    edge: &abap_cache::CallDataflowLifecycleEdge,
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowLifecycleNode>,
) -> (String, String, usize, usize) {
    let target = node_by_id
        .get(edge.target.as_str())
        .map(|node| ascii_node_sort_key(node))
        .unwrap_or_else(|| ("zzzz".to_string(), String::new(), String::new()));
    (
        format!("{} {}", edge.kind, edge.label.as_deref().unwrap_or("")),
        format!("{} {}", target.0, target.1),
        edge.source_range
            .as_ref()
            .map(|range| range.start)
            .unwrap_or(0),
        edge.source_range
            .as_ref()
            .map(|range| range.end)
            .unwrap_or(0),
    )
}

fn ascii_node_sort_key(node: &abap_cache::CallDataflowLifecycleNode) -> (String, String, String) {
    (node.kind.clone(), node.name.clone(), node.unit_uri.clone())
}

fn build_call_dataflow_tree_entries(
    lifecycle: &CallDataflowLifecycle,
    selected: Option<&CallDataflowSelectedCall>,
) -> Vec<CallDataflowTreeEntry> {
    let index = build_call_dataflow_lifecycle_index(lifecycle);
    let selected_target_id = selected.and_then(|selected| selected.target_node_id.as_deref());
    let mut entries = Vec::new();
    for root in index.roots {
        let mut path = BTreeSet::new();
        path.insert(root.id.clone());
        let root_index = entries.len();
        entries.push(CallDataflowTreeEntry {
            kind: root.kind.clone(),
            name: root.name.clone(),
            edge_label: None,
            depth: 0,
            parent_index: None,
            node_synthetic: root.synthetic,
            edge_synthetic: false,
            selected: selected_target_id == Some(root.id.as_str()),
        });
        build_call_dataflow_tree_children(
            root.id.as_str(),
            0,
            root_index,
            &index.outbound,
            &index.node_by_id,
            selected_target_id,
            &mut path,
            &mut entries,
        );
    }
    entries
}

fn build_call_dataflow_tree_children(
    node_id: &str,
    depth: usize,
    parent_index: usize,
    outbound: &HashMap<&str, Vec<&abap_cache::CallDataflowLifecycleEdge>>,
    node_by_id: &HashMap<&str, &abap_cache::CallDataflowLifecycleNode>,
    selected_target_id: Option<&str>,
    path: &mut BTreeSet<String>,
    entries: &mut Vec<CallDataflowTreeEntry>,
) {
    let Some(edges) = outbound.get(node_id) else {
        return;
    };

    for edge in edges {
        let entry = if let Some(node) = node_by_id.get(edge.target.as_str()) {
            CallDataflowTreeEntry {
                kind: node.kind.clone(),
                name: node.name.clone(),
                edge_label: Some(call_dataflow_edge_display(edge)),
                depth: depth + 1,
                parent_index: Some(parent_index),
                node_synthetic: node.synthetic,
                edge_synthetic: edge.synthetic,
                selected: selected_target_id == Some(node.id.as_str()),
            }
        } else {
            CallDataflowTreeEntry {
                kind: "unresolved".to_string(),
                name: "<unresolved>".to_string(),
                edge_label: Some(call_dataflow_edge_display(edge)),
                depth: depth + 1,
                parent_index: Some(parent_index),
                node_synthetic: false,
                edge_synthetic: edge.synthetic,
                selected: false,
            }
        };
        let entry_index = entries.len();
        entries.push(entry);

        if edge.target.is_empty() {
            continue;
        }
        let can_descend = path.insert(edge.target.clone());
        if !can_descend {
            continue;
        }
        build_call_dataflow_tree_children(
            edge.target.as_str(),
            depth + 1,
            entry_index,
            outbound,
            node_by_id,
            selected_target_id,
            path,
            entries,
        );
        path.remove(edge.target.as_str());
    }
}

fn call_dataflow_edge_display(edge: &abap_cache::CallDataflowLifecycleEdge) -> String {
    let mut label = edge.kind.clone();
    if let Some(extra) = edge.label.as_deref()
        && edge.kind != "selected_call"
    {
        label.push(' ');
        label.push_str(extra);
    }
    label
}

fn call_dataflow_svg_badge(entry: &CallDataflowTreeEntry) -> String {
    match (entry.selected, entry.node_synthetic) {
        (true, true) => "selected, synthetic".to_string(),
        (true, false) => "selected".to_string(),
        (false, true) => "synthetic".to_string(),
        (false, false) => String::new(),
    }
}

fn render_call_dataflow_mermaid(lifecycle: &CallDataflowLifecycle) -> String {
    let mut out = String::new();
    out.push_str("flowchart TD\n");
    let active_node_ids = call_dataflow_active_node_ids(lifecycle);
    let node_ids: HashMap<_, _> = lifecycle
        .nodes
        .iter()
        .filter(|node| active_node_ids.contains(node.id.as_str()))
        .enumerate()
        .map(|(idx, node)| (node.id.as_str(), format!("n{idx}")))
        .collect();

    for node in &lifecycle.nodes {
        if !active_node_ids.contains(node.id.as_str()) {
            continue;
        }
        let Some(mermaid_id) = node_ids.get(node.id.as_str()) else {
            continue;
        };
        let mut label = format!("{}: {}", node.kind, node.name);
        if node.synthetic {
            label.push_str(" (synthetic)");
        }
        out.push_str(&format!(
            "  {}[\"{}\"]\n",
            mermaid_id,
            mermaid_node_label(&label)
        ));
    }
    for edge in &lifecycle.edges {
        let Some(source_id) = node_ids.get(edge.source.as_str()) else {
            continue;
        };
        let Some(target_id) = node_ids.get(edge.target.as_str()) else {
            continue;
        };
        let label = edge
            .label
            .as_deref()
            .unwrap_or(edge.kind.as_str())
            .to_string();
        out.push_str(&format!(
            "  {} -->|\"{}\"| {}\n",
            source_id,
            mermaid_label(&label),
            target_id
        ));
    }
    let synthetic_nodes: Vec<_> = lifecycle
        .nodes
        .iter()
        .filter(|node| node.synthetic)
        .filter_map(|node| node_ids.get(node.id.as_str()).cloned())
        .collect();
    if !synthetic_nodes.is_empty() {
        out.push_str("  classDef synthetic fill:#fff4cc,stroke:#b7791f,color:#5f370e;\n");
        out.push_str(&format!(
            "  class {} synthetic;\n",
            synthetic_nodes.join(",")
        ));
    }
    out
}

fn call_dataflow_active_node_ids(lifecycle: &CallDataflowLifecycle) -> HashSet<&str> {
    let mut active = HashSet::new();
    for edge in &lifecycle.edges {
        active.insert(edge.source.as_str());
        if !edge.target.is_empty() {
            active.insert(edge.target.as_str());
        }
    }
    active
}

fn call_dataflow_mapping_location(mapping: &abap_cache::CallDataflowFieldMapping) -> String {
    match (
        mapping.source_unit_uri.as_deref(),
        mapping.source_range.as_ref(),
    ) {
        (Some(unit_uri), Some(range)) => call_dataflow_display_location(unit_uri, range),
        (Some(unit_uri), None) => unit_uri.to_string(),
        (None, Some(range)) => format!("{}-{}", range.start, range.end),
        (None, None) => String::new(),
    }
}

fn call_dataflow_provenance_node_label(node: &abap_cache::CallDataflowProvenanceNode) -> String {
    let prefix = match node.kind.as_str() {
        "parameter" => "parameter",
        "target_value" | "target_field" | "target_table_row" | "target_table_field" => "target",
        "assignment" => "assignment",
        "append_row" => "append",
        "loop_binding" => "loop bind",
        "perform_binding" => "perform bind",
        "perform_write" => "perform write",
        "read_table_binding" => "read table",
        "field_symbol_binding" => "field-symbol bind",
        "sql_query" => "sql query",
        "sql_source" => "sql source",
        "sql_predicate" => "sql predicate",
        "sql_source_field" => "sql column",
        "sql_target_field" => "sql target",
        "call_output" => "call output",
        "constant" => "constant",
        "global_state" => "global",
        "symbol" => "symbol",
        "literal_or_expression" => "expression",
        "composite_expression" => "expression",
        other => other,
    };
    let max_len = match node.kind.as_str() {
        "sql_query" => 320,
        "sql_predicate" => 220,
        "sql_source" => 160,
        "sql_source_field" | "sql_target_field" => 180,
        _ => 96,
    };
    let mut label = format!("{prefix}: {}", truncate_display(&node.label, max_len));
    if let (Some(unit_uri), Some(range)) = (node.unit_uri.as_deref(), node.range.as_ref()) {
        let location = call_dataflow_short_location(unit_uri, range);
        if matches!(
            node.kind.as_str(),
            "sql_query" | "sql_predicate" | "sql_source"
        ) {
            label.push_str(&format!("\n@ {location}"));
        } else {
            label.push_str(&format!(" @ {location}"));
        }
    }
    label
}

fn call_dataflow_provenance_edge_label(edge: &abap_cache::CallDataflowProvenanceEdge) -> String {
    edge.label
        .clone()
        .unwrap_or_else(|| edge.kind.replace('_', " "))
}

fn markdown_inline_code(value: &str) -> String {
    value.replace('`', "'")
}

fn markdown_table_cell(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('|', "\\|")
        .replace('\n', "<br/>")
}

fn mermaid_node_label(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "'")
        .replace('\n', "<br/>")
}

fn mermaid_label(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "'")
        .replace('\n', " ")
}

fn short_unit_name(unit_uri: &str) -> String {
    unit_uri.rsplit('/').next().unwrap_or(unit_uri).to_string()
}

fn call_dataflow_display_location(
    unit_uri: &str,
    range: &abap_cache::CallDataflowByteRange,
) -> String {
    if let Some(line_range) = call_dataflow_line_range(unit_uri, range) {
        format!("{unit_uri}:{line_range}")
    } else {
        format!("{unit_uri}:{}-{}", range.start, range.end)
    }
}

fn call_dataflow_short_location(
    unit_uri: &str,
    range: &abap_cache::CallDataflowByteRange,
) -> String {
    if let Some(line_range) = call_dataflow_line_range(unit_uri, range) {
        format!("{}:{line_range}", short_unit_name(unit_uri))
    } else {
        format!(
            "{}:{}-{}",
            short_unit_name(unit_uri),
            range.start,
            range.end
        )
    }
}

fn call_dataflow_line_range(
    unit_uri: &str,
    range: &abap_cache::CallDataflowByteRange,
) -> Option<String> {
    let text = call_dataflow_source_text(unit_uri)?;
    let start_line = byte_offset_to_line(text.as_str(), range.start);
    let end_line = if range.end > range.start {
        byte_offset_to_line(text.as_str(), range.end.saturating_sub(1))
    } else {
        start_line
    };
    Some(if start_line == end_line {
        format!("L{start_line}")
    } else {
        format!("L{start_line}-L{end_line}")
    })
}

fn call_dataflow_source_text(unit_uri: &str) -> Option<Arc<String>> {
    static CACHE: OnceLock<Mutex<HashMap<String, Option<Arc<String>>>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));

    if let Some(cached) = cache
        .lock()
        .expect("call-dataflow source text cache lock")
        .get(unit_uri)
        .cloned()
    {
        return cached;
    }

    let loaded = file_uri_to_path(unit_uri)
        .and_then(|path| fs::read_to_string(path).ok())
        .map(Arc::new);
    cache
        .lock()
        .expect("call-dataflow source text cache lock")
        .insert(unit_uri.to_string(), loaded.clone());
    loaded
}

fn byte_offset_to_line(text: &str, offset: usize) -> usize {
    let clamped = offset.min(text.len());
    1 + text.as_bytes()[..clamped]
        .iter()
        .filter(|byte| **byte == b'\n')
        .count()
}

fn truncate_display(value: &str, max_len: usize) -> String {
    if value.chars().count() <= max_len {
        return value.to_string();
    }
    let truncated: String = value.chars().take(max_len.saturating_sub(3)).collect();
    format!("{truncated}...")
}

fn xml_text(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
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
    build_plan: SnapshotBuildPlan,
) -> Result<Arc<abap_cache::AnalysisSnapshot>, String> {
    let Some(target_path) = resolve_target_path(path)? else {
        let source = read_source(path)?;
        let store = DocumentStore::default();
        let snapshots = store.replace_all_with_build_plan(
            vec![DocumentInput {
                uri: Arc::from("file:///stdin.abap"),
                version: 1,
                text: Arc::from(source),
                is_dependency: false,
                object_name: None,
            }],
            build_plan,
        );
        return snapshots
            .get("file:///stdin.abap")
            .cloned()
            .ok_or_else(|| "stdin analysis did not materialize file:///stdin.abap".to_string());
    };

    let target_uri = path_to_file_uri(&target_path);
    let workspace_root = find_workspace_root(&target_path)?;
    let workspace_root_uri = path_to_file_uri(&workspace_root);
    let workspace = load_workspace_documents(&workspace_root_uri, &HashMap::new());
    let mut documents = workspace.documents.clone();

    if !documents
        .iter()
        .any(|document| document.uri.as_ref() == target_uri)
    {
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
        documents.push(abap_cache::WorkspaceDocument {
            uri: Arc::from(target_uri.as_str()),
            version: 1,
            text: source,
            is_dependency,
            object_name,
        });
    }

    let store = DocumentStore::default();
    let snapshots = replace_all_workspace_documents_with_local_exports_for_build_plan(
        &store,
        &workspace.root_path,
        &documents,
        build_plan,
        None,
    );
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
    let manifest_present = workspace.manifest.is_some();
    let mut documents = workspace.documents.clone();

    if !documents
        .iter()
        .any(|document| document.uri.as_ref() == target_uri)
    {
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
        documents.push(abap_cache::WorkspaceDocument {
            uri: Arc::from(target_uri.as_str()),
            version: 1,
            text: source,
            is_dependency,
            object_name,
        });
    }

    let store = DocumentStore::default();
    let snapshots = replace_all_workspace_documents_with_local_exports_for_build_plan(
        &store,
        &workspace.root_path,
        &documents,
        SnapshotBuildPlan::SEMANTIC_DOSSIER,
        None,
    );
    let snapshot = snapshots.get(target_uri.as_str()).cloned().ok_or_else(|| {
        format!(
            "workspace analysis did not include {}",
            target_path.display()
        )
    })?;
    let dependency_unit_count = snapshots
        .values()
        .filter(|snapshot| snapshot.is_dependency)
        .count();

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
        let snapshots = store.replace_all_with_build_plan(
            vec![DocumentInput {
                uri: Arc::from("file:///stdin.abap"),
                version: 1,
                text: Arc::from(source),
                is_dependency: false,
                object_name: None,
            }],
            SnapshotBuildPlan::EFFECTIVE_SOURCE,
        );
        let root = snapshots
            .get("file:///stdin.abap")
            .cloned()
            .ok_or_else(|| "stdin expansion did not materialize file:///stdin.abap".to_string())?;
        return Ok(ExpandSnapshotSet {
            root: Arc::clone(&root),
            snapshots,
        });
    };

    let target_uri = path_to_file_uri(&target_path);
    let workspace_root = find_workspace_root(&target_path)?;
    let workspace_root_uri = path_to_file_uri(&workspace_root);
    let workspace = load_workspace_documents(&workspace_root_uri, &HashMap::new());
    let mut documents = workspace.documents.clone();

    if !documents
        .iter()
        .any(|document| document.uri.as_ref() == target_uri)
    {
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
        documents.push(abap_cache::WorkspaceDocument {
            uri: Arc::from(target_uri.as_str()),
            version: 1,
            text: source,
            is_dependency,
            object_name,
        });
    }

    let store = DocumentStore::default();
    let snapshots = replace_all_workspace_documents_with_local_exports_for_build_plan(
        &store,
        &workspace.root_path,
        &documents,
        SnapshotBuildPlan::EFFECTIVE_SOURCE,
        None,
    );
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
    use super::{
        CallDataflowDiagramFormat, load_remote_candidate_workspace, parse_cli_args,
        path_to_file_uri, render_call_dataflow_diagram_block,
        render_call_dataflow_parameter_provenance_mermaid,
        render_call_dataflow_parameter_rich_mermaid, render_call_dataflow_report,
    };
    use abap_cache::{
        CallDataflowByteRange, CallDataflowLifecycle, CallDataflowLifecycleEdge,
        CallDataflowLifecycleNode, CallDataflowParameterTrace, CallDataflowProvenanceEdge,
        CallDataflowProvenanceGraph, CallDataflowProvenanceNode, CallDataflowQuery,
        CallDataflowSelectedCall, CallDataflowSummary, CallDataflowTrace,
    };
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
    fn parses_call_dataflow_command() {
        let cli = parse_cli_args(
            [
                "call-dataflow",
                "--target",
                "BAPI_PO_CREATE1",
                "--caller",
                "call_api",
                "--occurrence",
                "2",
                "--diagram",
                "svg",
                "--pretty",
                "main.abap",
            ]
            .into_iter()
            .map(str::to_string),
        )
        .expect("cli");

        assert_eq!(cli.command, super::Command::CallDataflow);
        assert_eq!(cli.call_dataflow_target.as_deref(), Some("BAPI_PO_CREATE1"));
        assert_eq!(cli.call_dataflow_caller.as_deref(), Some("call_api"));
        assert_eq!(cli.call_dataflow_occurrence, Some(2));
        assert_eq!(cli.call_dataflow_diagram, CallDataflowDiagramFormat::Svg);
        assert!(cli.pretty);
        assert_eq!(cli.path.as_deref(), Some("main.abap"));
    }

    #[test]
    fn parses_call_dataflow_rich_mermaid_diagram() {
        let cli = parse_cli_args(
            [
                "call-dataflow",
                "--target",
                "BAPI_PO_CREATE1",
                "--diagram",
                "rich-mermaid",
                "main.abap",
            ]
            .into_iter()
            .map(str::to_string),
        )
        .expect("cli");

        assert_eq!(
            cli.call_dataflow_diagram,
            CallDataflowDiagramFormat::RichMermaid
        );
    }

    #[test]
    fn call_dataflow_requires_target() {
        let err = parse_cli_args(["call-dataflow"].into_iter().map(str::to_string))
            .expect_err("missing target should fail");
        assert!(
            err.contains("call-dataflow requires --target NAME"),
            "{err}"
        );
    }

    #[test]
    fn call_dataflow_ascii_diagram_renders_tree() {
        let lifecycle = CallDataflowLifecycle {
            nodes: vec![
                CallDataflowLifecycleNode {
                    id: "event".to_string(),
                    kind: "event_block".to_string(),
                    name: "start-of-selection".to_string(),
                    unit_uri: "file:///main.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 0, end: 1 },
                    synthetic: false,
                },
                CallDataflowLifecycleNode {
                    id: "form".to_string(),
                    kind: "form".to_string(),
                    name: "create_sto".to_string(),
                    unit_uri: "file:///main.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 2, end: 3 },
                    synthetic: false,
                },
                CallDataflowLifecycleNode {
                    id: "target".to_string(),
                    kind: "function_module".to_string(),
                    name: "bapi_po_create1".to_string(),
                    unit_uri: "file:///bapi.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 4, end: 5 },
                    synthetic: false,
                },
            ],
            edges: vec![
                CallDataflowLifecycleEdge {
                    source: "event".to_string(),
                    target: "form".to_string(),
                    kind: "perform".to_string(),
                    label: None,
                    source_range: Some(CallDataflowByteRange { start: 10, end: 20 }),
                    synthetic: false,
                },
                CallDataflowLifecycleEdge {
                    source: "form".to_string(),
                    target: "target".to_string(),
                    kind: "selected_call".to_string(),
                    label: Some("bapi_po_create1".to_string()),
                    source_range: Some(CallDataflowByteRange { start: 30, end: 40 }),
                    synthetic: false,
                },
            ],
        };
        let selected = CallDataflowSelectedCall {
            occurrence: 1,
            target_kind: "function".to_string(),
            target_name: "bapi_po_create1".to_string(),
            unit_uri: "file:///main.abap".to_string(),
            call_range: CallDataflowByteRange { start: 30, end: 40 },
            caller_node_id: Some("form".to_string()),
            caller_kind: Some("form".to_string()),
            caller_name: Some("create_sto".to_string()),
            caller_unit_uri: Some("file:///main.abap".to_string()),
            target_node_id: Some("target".to_string()),
            argument_count: 1,
        };

        let rendered = render_call_dataflow_diagram_block(
            &lifecycle,
            Some(&selected),
            CallDataflowDiagramFormat::Ascii,
        );

        assert!(rendered.contains("```text"));
        assert!(rendered.contains("event_block start-of-selection"));
        assert!(rendered.contains("perform -> form create_sto"));
        assert!(rendered.contains("selected_call -> function_module bapi_po_create1 [selected]"));
    }

    #[test]
    fn call_dataflow_svg_diagram_renders_svg_markup() {
        let lifecycle = CallDataflowLifecycle {
            nodes: vec![
                CallDataflowLifecycleNode {
                    id: "event".to_string(),
                    kind: "event_block".to_string(),
                    name: "end-of-selection".to_string(),
                    unit_uri: "file:///main.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 0, end: 1 },
                    synthetic: false,
                },
                CallDataflowLifecycleNode {
                    id: "target".to_string(),
                    kind: "function_module".to_string(),
                    name: "bapi_po_create1".to_string(),
                    unit_uri: "file:///main.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 2, end: 3 },
                    synthetic: false,
                },
            ],
            edges: vec![CallDataflowLifecycleEdge {
                source: "event".to_string(),
                target: "target".to_string(),
                kind: "selected_call".to_string(),
                label: Some("bapi_po_create1".to_string()),
                source_range: Some(CallDataflowByteRange { start: 10, end: 20 }),
                synthetic: false,
            }],
        };
        let selected = CallDataflowSelectedCall {
            occurrence: 1,
            target_kind: "function".to_string(),
            target_name: "bapi_po_create1".to_string(),
            unit_uri: "file:///main.abap".to_string(),
            call_range: CallDataflowByteRange { start: 10, end: 20 },
            caller_node_id: Some("event".to_string()),
            caller_kind: Some("event_block".to_string()),
            caller_name: Some("end-of-selection".to_string()),
            caller_unit_uri: Some("file:///main.abap".to_string()),
            target_node_id: Some("target".to_string()),
            argument_count: 1,
        };

        let rendered = render_call_dataflow_diagram_block(
            &lifecycle,
            Some(&selected),
            CallDataflowDiagramFormat::Svg,
        );

        assert!(rendered.contains("<svg "));
        assert!(rendered.contains("call-dataflow-arrow"));
        assert!(rendered.contains("function_module"));
        assert!(rendered.contains("bapi_po_create1"));
        assert!(rendered.contains("selected"));
    }

    #[test]
    fn call_dataflow_mermaid_diagram_quotes_edge_labels() {
        let lifecycle = CallDataflowLifecycle {
            nodes: vec![
                CallDataflowLifecycleNode {
                    id: "event".to_string(),
                    kind: "event_block".to_string(),
                    name: "end-of-selection".to_string(),
                    unit_uri: "file:///main.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 0, end: 1 },
                    synthetic: false,
                },
                CallDataflowLifecycleNode {
                    id: "target".to_string(),
                    kind: "function_module".to_string(),
                    name: "bapi_po_create1".to_string(),
                    unit_uri: "file:///main.abap".to_string(),
                    decl_range: CallDataflowByteRange { start: 2, end: 3 },
                    synthetic: false,
                },
            ],
            edges: vec![CallDataflowLifecycleEdge {
                source: "event".to_string(),
                target: "target".to_string(),
                kind: "screen_dispatch".to_string(),
                label: Some("CALL SCREEN 9000 (input)".to_string()),
                source_range: Some(CallDataflowByteRange { start: 10, end: 20 }),
                synthetic: true,
            }],
        };

        let rendered = render_call_dataflow_diagram_block(
            &lifecycle,
            None,
            CallDataflowDiagramFormat::Mermaid,
        );

        assert!(rendered.contains("```mermaid"));
        assert!(rendered.contains("-->|\"CALL SCREEN 9000 (input)\"|"));
    }

    #[test]
    fn call_dataflow_mermaid_query_nodes_keep_clause_line_breaks() {
        let provenance = CallDataflowProvenanceGraph {
            nodes: vec![CallDataflowProvenanceNode {
                id: "p0".to_string(),
                kind: "sql_query".to_string(),
                label: "SELECT matnr, meins\nFROM mara\nWHERE matnr = p_matnr".to_string(),
                unit_uri: Some("file:///main.abap".to_string()),
                range: Some(CallDataflowByteRange { start: 10, end: 40 }),
                statement_text: None,
            }],
            edges: Vec::new(),
        };

        let rendered = render_call_dataflow_parameter_provenance_mermaid(&provenance);

        assert!(rendered.contains("SELECT matnr, meins<br/>FROM mara<br/>WHERE matnr = p_matnr"));
    }

    #[test]
    fn call_dataflow_rich_mermaid_parameter_omits_sql_helper_nodes() {
        let parameter = CallDataflowParameterTrace {
            parameter_name: Some("poheader".to_string()),
            section: Some("exporting".to_string()),
            direction: "input".to_string(),
            argument_text: "poheader = gs_header".to_string(),
            argument_range: CallDataflowByteRange { start: 30, end: 40 },
            argument_type: Some("bapimepoheader".to_string()),
            field_mappings: Vec::new(),
            provenance: CallDataflowProvenanceGraph {
                nodes: vec![
                    CallDataflowProvenanceNode {
                        id: "p0".to_string(),
                        kind: "parameter".to_string(),
                        label: "poheader [input / exporting] : bapimepoheader".to_string(),
                        unit_uri: None,
                        range: None,
                        statement_text: None,
                    },
                    CallDataflowProvenanceNode {
                        id: "p1".to_string(),
                        kind: "perform_write".to_string(),
                        label: "f_header writes cs_poheader".to_string(),
                        unit_uri: Some("file:///main.abap".to_string()),
                        range: Some(CallDataflowByteRange { start: 50, end: 60 }),
                        statement_text: Some("PERFORM f_header CHANGING cs_poheader.".to_string()),
                    },
                    CallDataflowProvenanceNode {
                        id: "p2".to_string(),
                        kind: "sql_query".to_string(),
                        label: "SELECT ekorg, bukrs\nFROM t024e\nINTO TABLE ct_t024e\nWHERE ekorg = lt_temp-ekorg".to_string(),
                        unit_uri: Some("file:///main.abap".to_string()),
                        range: Some(CallDataflowByteRange { start: 10, end: 40 }),
                        statement_text: None,
                    },
                    CallDataflowProvenanceNode {
                        id: "p3".to_string(),
                        kind: "sql_source".to_string(),
                        label: "FROM t024e".to_string(),
                        unit_uri: Some("file:///main.abap".to_string()),
                        range: Some(CallDataflowByteRange { start: 14, end: 19 }),
                        statement_text: None,
                    },
                    CallDataflowProvenanceNode {
                        id: "p4".to_string(),
                        kind: "sql_predicate".to_string(),
                        label: "WHERE ekorg = lt_temp-ekorg".to_string(),
                        unit_uri: Some("file:///main.abap".to_string()),
                        range: Some(CallDataflowByteRange { start: 20, end: 30 }),
                        statement_text: None,
                    },
                    CallDataflowProvenanceNode {
                        id: "p5".to_string(),
                        kind: "sql_target_field".to_string(),
                        label: "ct_t024e-bukrs".to_string(),
                        unit_uri: Some("file:///main.abap".to_string()),
                        range: Some(CallDataflowByteRange { start: 31, end: 35 }),
                        statement_text: None,
                    },
                    CallDataflowProvenanceNode {
                        id: "p6".to_string(),
                        kind: "symbol".to_string(),
                        label: "lt_temp-ekorg".to_string(),
                        unit_uri: Some("file:///main.abap".to_string()),
                        range: Some(CallDataflowByteRange { start: 24, end: 29 }),
                        statement_text: None,
                    },
                ],
                edges: vec![
                    CallDataflowProvenanceEdge {
                        source: "p6".to_string(),
                        target: "p4".to_string(),
                        kind: "uses".to_string(),
                        label: None,
                    },
                    CallDataflowProvenanceEdge {
                        source: "p4".to_string(),
                        target: "p2".to_string(),
                        kind: "filters".to_string(),
                        label: None,
                    },
                    CallDataflowProvenanceEdge {
                        source: "p3".to_string(),
                        target: "p2".to_string(),
                        kind: "reads_from".to_string(),
                        label: None,
                    },
                    CallDataflowProvenanceEdge {
                        source: "p2".to_string(),
                        target: "p5".to_string(),
                        kind: "selects_into".to_string(),
                        label: None,
                    },
                    CallDataflowProvenanceEdge {
                        source: "p5".to_string(),
                        target: "p1".to_string(),
                        kind: "flows_to".to_string(),
                        label: None,
                    },
                    CallDataflowProvenanceEdge {
                        source: "p1".to_string(),
                        target: "p0".to_string(),
                        kind: "writes".to_string(),
                        label: None,
                    },
                ],
            },
            notes: Vec::new(),
        };

        let rendered = render_call_dataflow_parameter_rich_mermaid(&parameter);

        assert!(rendered.contains("sql query: SELECT ekorg, bukrs"));
        assert!(!rendered.contains("HOSTS lt_temp-ekorg"));
        assert!(rendered.contains("p2 --> p0"));
        assert!(!rendered.contains("sql source:"));
        assert!(!rendered.contains("sql predicate:"));
        assert!(!rendered.contains("sql target:"));
        assert!(!rendered.contains("symbol: lt_temp-ekorg"));
        assert!(!rendered.contains("perform write:"));
    }

    #[test]
    fn call_dataflow_rich_mermaid_report_renders_parameter_scoped_diagram() {
        let trace = CallDataflowTrace {
            schema: "abap.call_dataflow_trace",
            schema_version: 1,
            query: CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
            selected_call: Some(CallDataflowSelectedCall {
                occurrence: 1,
                target_kind: "function_module".to_string(),
                target_name: "bapi_po_create1".to_string(),
                unit_uri: "file:///main.abap".to_string(),
                call_range: CallDataflowByteRange { start: 30, end: 40 },
                caller_node_id: Some("form".to_string()),
                caller_kind: Some("form".to_string()),
                caller_name: Some("call_api".to_string()),
                caller_unit_uri: Some("file:///main.abap".to_string()),
                target_node_id: Some("target".to_string()),
                argument_count: 1,
            }),
            matches: Vec::new(),
            lifecycle: CallDataflowLifecycle {
                nodes: vec![
                    CallDataflowLifecycleNode {
                        id: "form".to_string(),
                        kind: "form".to_string(),
                        name: "call_api".to_string(),
                        unit_uri: "file:///main.abap".to_string(),
                        decl_range: CallDataflowByteRange { start: 0, end: 1 },
                        synthetic: false,
                    },
                    CallDataflowLifecycleNode {
                        id: "target".to_string(),
                        kind: "function_module".to_string(),
                        name: "bapi_po_create1".to_string(),
                        unit_uri: "file:///bapi.abap".to_string(),
                        decl_range: CallDataflowByteRange { start: 2, end: 3 },
                        synthetic: false,
                    },
                ],
                edges: vec![CallDataflowLifecycleEdge {
                    source: "form".to_string(),
                    target: "target".to_string(),
                    kind: "selected_call".to_string(),
                    label: Some("bapi_po_create1".to_string()),
                    source_range: Some(CallDataflowByteRange { start: 30, end: 40 }),
                    synthetic: false,
                }],
            },
            parameter_traces: vec![CallDataflowParameterTrace {
                parameter_name: Some("poheader".to_string()),
                section: Some("exporting".to_string()),
                direction: "input".to_string(),
                argument_text: "poheader = gs_header".to_string(),
                argument_range: CallDataflowByteRange { start: 30, end: 40 },
                argument_type: Some("bapimepoheader".to_string()),
                field_mappings: Vec::new(),
                provenance: CallDataflowProvenanceGraph {
                    nodes: vec![
                        CallDataflowProvenanceNode {
                            id: "p0".to_string(),
                            kind: "parameter".to_string(),
                            label: "poheader [input / exporting] : bapimepoheader".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p1".to_string(),
                            kind: "assignment".to_string(),
                            label: "gs_header-doc_type = 'NB'.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 10, end: 20 }),
                            statement_text: Some("gs_header-doc_type = 'NB'.".to_string()),
                        },
                        CallDataflowProvenanceNode {
                            id: "p2".to_string(),
                            kind: "target_field".to_string(),
                            label: "poheader.doc_type".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                    ],
                    edges: vec![
                        CallDataflowProvenanceEdge {
                            source: "p1".to_string(),
                            target: "p2".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p2".to_string(),
                            target: "p0".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                    ],
                },
                notes: Vec::new(),
            }],
            summary: CallDataflowSummary {
                match_count: 1,
                ambiguous: false,
                lifecycle_node_count: 2,
                lifecycle_edge_count: 1,
                synthetic_edge_count: 0,
                parameter_count: 1,
                mapping_count: 0,
            },
        };

        let rendered = render_call_dataflow_report(&trace, CallDataflowDiagramFormat::RichMermaid);

        assert!(rendered.contains("## Parameters"));
        assert!(rendered.contains("#### Rich Diagram"));
        assert!(rendered.contains("```mermaid"));
        assert!(rendered.contains("poheader.doc_type"));
        assert!(!rendered.contains("## Diagram"));
        assert!(!rendered.contains("#### Detailed Provenance"));
    }

    #[test]
    fn call_dataflow_rich_mermaid_collapses_multi_field_row_build() {
        let trace = CallDataflowTrace {
            schema: "abap.call_dataflow_trace",
            schema_version: 1,
            query: CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("f_bapi_item_data".to_string()),
                occurrence: None,
            },
            selected_call: Some(CallDataflowSelectedCall {
                occurrence: 1,
                target_kind: "function_module".to_string(),
                target_name: "bapi_po_create1".to_string(),
                unit_uri: "file:///main.abap".to_string(),
                call_range: CallDataflowByteRange { start: 30, end: 40 },
                caller_node_id: Some("form".to_string()),
                caller_kind: Some("form".to_string()),
                caller_name: Some("f_bapi_item_data".to_string()),
                caller_unit_uri: Some("file:///main.abap".to_string()),
                target_node_id: Some("target".to_string()),
                argument_count: 1,
            }),
            matches: Vec::new(),
            lifecycle: CallDataflowLifecycle {
                nodes: vec![
                    CallDataflowLifecycleNode {
                        id: "form".to_string(),
                        kind: "form".to_string(),
                        name: "f_bapi_item_data".to_string(),
                        unit_uri: "file:///main.abap".to_string(),
                        decl_range: CallDataflowByteRange { start: 0, end: 1 },
                        synthetic: false,
                    },
                    CallDataflowLifecycleNode {
                        id: "target".to_string(),
                        kind: "function_module".to_string(),
                        name: "bapi_po_create1".to_string(),
                        unit_uri: "file:///bapi.abap".to_string(),
                        decl_range: CallDataflowByteRange { start: 2, end: 3 },
                        synthetic: false,
                    },
                ],
                edges: vec![CallDataflowLifecycleEdge {
                    source: "form".to_string(),
                    target: "target".to_string(),
                    kind: "selected_call".to_string(),
                    label: Some("bapi_po_create1".to_string()),
                    source_range: Some(CallDataflowByteRange { start: 30, end: 40 }),
                    synthetic: false,
                }],
            },
            parameter_traces: vec![CallDataflowParameterTrace {
                parameter_name: Some("poitem".to_string()),
                section: Some("tables".to_string()),
                direction: "in_out".to_string(),
                argument_text: "poitem = gt_poitem".to_string(),
                argument_range: CallDataflowByteRange { start: 30, end: 40 },
                argument_type: Some("typ_t_poitem".to_string()),
                field_mappings: Vec::new(),
                provenance: CallDataflowProvenanceGraph {
                    nodes: vec![
                        CallDataflowProvenanceNode {
                            id: "p0".to_string(),
                            kind: "parameter".to_string(),
                            label: "poitem [in_out / tables] : typ_t_poitem".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p1".to_string(),
                            kind: "target_table_row".to_string(),
                            label: "poitem[*]".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p2".to_string(),
                            kind: "perform_write".to_string(),
                            label: "f_bapi_item_data writes ct_poitem (append rows)".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange {
                                start: 80,
                                end: 120,
                            }),
                            statement_text: Some(
                                "PERFORM f_bapi_item_data CHANGING ct_poitem.".to_string(),
                            ),
                        },
                        CallDataflowProvenanceNode {
                            id: "p10".to_string(),
                            kind: "target_table_field".to_string(),
                            label: "poitem[*].material".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p11".to_string(),
                            kind: "assignment".to_string(),
                            label: "ls_poitem-material = ls_src-matnr.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 10, end: 20 }),
                            statement_text: Some("ls_poitem-material = ls_src-matnr.".to_string()),
                        },
                        CallDataflowProvenanceNode {
                            id: "p12".to_string(),
                            kind: "target_table_field".to_string(),
                            label: "poitem[*].plant".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p13".to_string(),
                            kind: "assignment".to_string(),
                            label: "ls_poitem-plant = ls_src-werks.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 21, end: 30 }),
                            statement_text: Some("ls_poitem-plant = ls_src-werks.".to_string()),
                        },
                        CallDataflowProvenanceNode {
                            id: "p14".to_string(),
                            kind: "target_table_field".to_string(),
                            label: "poitem[*].quantity".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p15".to_string(),
                            kind: "assignment".to_string(),
                            label: "ls_poitem-quantity = lv_qty.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 31, end: 40 }),
                            statement_text: Some("ls_poitem-quantity = lv_qty.".to_string()),
                        },
                        CallDataflowProvenanceNode {
                            id: "p16".to_string(),
                            kind: "target_table_field".to_string(),
                            label: "poitem[*].stge_loc".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p17".to_string(),
                            kind: "assignment".to_string(),
                            label: "ls_poitem-stge_loc = lv_lgort.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 41, end: 50 }),
                            statement_text: Some("ls_poitem-stge_loc = lv_lgort.".to_string()),
                        },
                    ],
                    edges: vec![
                        CallDataflowProvenanceEdge {
                            source: "p11".to_string(),
                            target: "p10".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p13".to_string(),
                            target: "p12".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p15".to_string(),
                            target: "p14".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p17".to_string(),
                            target: "p16".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p10".to_string(),
                            target: "p1".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p12".to_string(),
                            target: "p1".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p14".to_string(),
                            target: "p1".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p16".to_string(),
                            target: "p1".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p1".to_string(),
                            target: "p0".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p1".to_string(),
                            target: "p2".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                    ],
                },
                notes: Vec::new(),
            }],
            summary: CallDataflowSummary {
                match_count: 1,
                ambiguous: false,
                lifecycle_node_count: 2,
                lifecycle_edge_count: 1,
                synthetic_edge_count: 0,
                parameter_count: 1,
                mapping_count: 0,
            },
        };

        let rendered = render_call_dataflow_report(&trace, CallDataflowDiagramFormat::RichMermaid);

        assert!(rendered.contains("#### Rich Diagram"));
        assert!(rendered.contains("updates:"));
        assert!(rendered.contains("material=ls_src-matnr"));
        assert!(rendered.contains("plant=ls_src-werks"));
        assert!(rendered.contains("quantity=lv_qty"));
        assert!(rendered.contains("stge_loc=lv_lgort"));
        assert!(!rendered.contains("FORM f_bapi_item_data:"));
        assert!(!rendered.contains("perform write:"));
        assert!(!rendered.contains("perform write: f_bapi_item_data writes"));
        assert!(!rendered.contains("poitem[*].material"));
        assert!(!rendered.contains("ls_poitem-material = ls_src-matnr."));
    }

    #[test]
    fn call_dataflow_rich_mermaid_absorbs_constant_leaf_nodes_into_field_update_summary() {
        let trace = CallDataflowTrace {
            schema: "abap.call_dataflow_trace",
            schema_version: 1,
            query: CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("f_bapi_header_data".to_string()),
                occurrence: None,
            },
            selected_call: Some(CallDataflowSelectedCall {
                occurrence: 1,
                target_kind: "function_module".to_string(),
                target_name: "bapi_po_create1".to_string(),
                unit_uri: "file:///main.abap".to_string(),
                call_range: CallDataflowByteRange { start: 30, end: 40 },
                caller_node_id: Some("form".to_string()),
                caller_kind: Some("form".to_string()),
                caller_name: Some("f_bapi_header_data".to_string()),
                caller_unit_uri: Some("file:///main.abap".to_string()),
                target_node_id: Some("target".to_string()),
                argument_count: 1,
            }),
            matches: Vec::new(),
            lifecycle: CallDataflowLifecycle {
                nodes: vec![
                    CallDataflowLifecycleNode {
                        id: "form".to_string(),
                        kind: "form".to_string(),
                        name: "f_bapi_header_data".to_string(),
                        unit_uri: "file:///main.abap".to_string(),
                        decl_range: CallDataflowByteRange { start: 0, end: 1 },
                        synthetic: false,
                    },
                    CallDataflowLifecycleNode {
                        id: "target".to_string(),
                        kind: "function_module".to_string(),
                        name: "bapi_po_create1".to_string(),
                        unit_uri: "file:///bapi.abap".to_string(),
                        decl_range: CallDataflowByteRange { start: 2, end: 3 },
                        synthetic: false,
                    },
                ],
                edges: vec![CallDataflowLifecycleEdge {
                    source: "form".to_string(),
                    target: "target".to_string(),
                    kind: "selected_call".to_string(),
                    label: Some("bapi_po_create1".to_string()),
                    source_range: Some(CallDataflowByteRange { start: 30, end: 40 }),
                    synthetic: false,
                }],
            },
            parameter_traces: vec![CallDataflowParameterTrace {
                parameter_name: Some("poheaderx".to_string()),
                section: Some("exporting".to_string()),
                direction: "input".to_string(),
                argument_text: "poheaderx = gs_headerx".to_string(),
                argument_range: CallDataflowByteRange { start: 30, end: 40 },
                argument_type: Some("bapimepoheaderx".to_string()),
                field_mappings: Vec::new(),
                provenance: CallDataflowProvenanceGraph {
                    nodes: vec![
                        CallDataflowProvenanceNode {
                            id: "p0".to_string(),
                            kind: "parameter".to_string(),
                            label: "poheaderx [input / exporting] : bapimepoheaderx".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p1".to_string(),
                            kind: "perform_binding".to_string(),
                            label: "gs_headerx -> f_bapi_header_data.cs_po_headerx".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange {
                                start: 100,
                                end: 120,
                            }),
                            statement_text: Some(
                                "PERFORM f_bapi_header_data CHANGING gs_headerx.".to_string(),
                            ),
                        },
                        CallDataflowProvenanceNode {
                            id: "p2".to_string(),
                            kind: "perform_write".to_string(),
                            label: "f_bapi_header_data writes cs_po_headerx".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange {
                                start: 100,
                                end: 120,
                            }),
                            statement_text: Some(
                                "PERFORM f_bapi_header_data CHANGING gs_headerx.".to_string(),
                            ),
                        },
                        CallDataflowProvenanceNode {
                            id: "p3".to_string(),
                            kind: "target_field".to_string(),
                            label: "poheaderx.comp_code".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p4".to_string(),
                            kind: "assignment".to_string(),
                            label: "cs_po_headerx-comp_code = abap_true.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 10, end: 20 }),
                            statement_text: Some(
                                "cs_po_headerx-comp_code = abap_true.".to_string(),
                            ),
                        },
                        CallDataflowProvenanceNode {
                            id: "p5".to_string(),
                            kind: "constant".to_string(),
                            label: "abap_true".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 18, end: 27 }),
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p6".to_string(),
                            kind: "target_field".to_string(),
                            label: "poheaderx.doc_date".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p7".to_string(),
                            kind: "assignment".to_string(),
                            label: "cs_po_headerx-doc_date = abap_true.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 21, end: 31 }),
                            statement_text: Some("cs_po_headerx-doc_date = abap_true.".to_string()),
                        },
                        CallDataflowProvenanceNode {
                            id: "p8".to_string(),
                            kind: "constant".to_string(),
                            label: "abap_true".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 29, end: 38 }),
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p9".to_string(),
                            kind: "target_field".to_string(),
                            label: "poheaderx.purch_org".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p10".to_string(),
                            kind: "assignment".to_string(),
                            label: "cs_po_headerx-purch_org = abap_true.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 32, end: 42 }),
                            statement_text: Some(
                                "cs_po_headerx-purch_org = abap_true.".to_string(),
                            ),
                        },
                        CallDataflowProvenanceNode {
                            id: "p11".to_string(),
                            kind: "constant".to_string(),
                            label: "abap_true".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 40, end: 49 }),
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p12".to_string(),
                            kind: "target_field".to_string(),
                            label: "poheaderx.ref_1".to_string(),
                            unit_uri: None,
                            range: None,
                            statement_text: None,
                        },
                        CallDataflowProvenanceNode {
                            id: "p13".to_string(),
                            kind: "assignment".to_string(),
                            label: "cs_po_headerx-ref_1 = abap_true.".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 43, end: 53 }),
                            statement_text: Some("cs_po_headerx-ref_1 = abap_true.".to_string()),
                        },
                        CallDataflowProvenanceNode {
                            id: "p14".to_string(),
                            kind: "constant".to_string(),
                            label: "abap_true".to_string(),
                            unit_uri: Some("file:///main.abap".to_string()),
                            range: Some(CallDataflowByteRange { start: 51, end: 60 }),
                            statement_text: None,
                        },
                    ],
                    edges: vec![
                        CallDataflowProvenanceEdge {
                            source: "p1".to_string(),
                            target: "p2".to_string(),
                            kind: "binds_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p2".to_string(),
                            target: "p0".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p3".to_string(),
                            target: "p0".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p3".to_string(),
                            target: "p2".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p4".to_string(),
                            target: "p3".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p5".to_string(),
                            target: "p4".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p6".to_string(),
                            target: "p0".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p6".to_string(),
                            target: "p2".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p7".to_string(),
                            target: "p6".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p8".to_string(),
                            target: "p7".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p9".to_string(),
                            target: "p0".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p9".to_string(),
                            target: "p2".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p10".to_string(),
                            target: "p9".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p11".to_string(),
                            target: "p10".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p12".to_string(),
                            target: "p0".to_string(),
                            kind: "populates".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p12".to_string(),
                            target: "p2".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p13".to_string(),
                            target: "p12".to_string(),
                            kind: "writes".to_string(),
                            label: None,
                        },
                        CallDataflowProvenanceEdge {
                            source: "p14".to_string(),
                            target: "p13".to_string(),
                            kind: "flows_to".to_string(),
                            label: None,
                        },
                    ],
                },
                notes: Vec::new(),
            }],
            summary: CallDataflowSummary {
                match_count: 1,
                ambiguous: false,
                lifecycle_node_count: 2,
                lifecycle_edge_count: 1,
                synthetic_edge_count: 0,
                parameter_count: 1,
                mapping_count: 0,
            },
        };

        let rendered = render_call_dataflow_report(&trace, CallDataflowDiagramFormat::RichMermaid);

        assert!(rendered.contains("#### Rich Diagram"));
        assert!(rendered.contains("updates:"));
        assert!(rendered.contains("comp_code=abap_true"));
        assert!(rendered.contains("doc_date=abap_true"));
        assert!(rendered.contains("purch_org=abap_true"));
        assert!(rendered.contains("ref_1=abap_true"));
        assert!(!rendered.contains("FORM f_bapi_header_data:"));
        assert!(!rendered.contains("perform bind:"));
        assert!(!rendered.contains("perform write:"));
        assert!(!rendered.contains("perform write: f_bapi_header_data writes cs_po_headerx"));
        assert!(!rendered.contains("constant: abap_true"));
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

    #[test]
    fn remote_candidates_skip_local_exported_dependencies_from_unit_sidecars() {
        let root = std::env::temp_dir().join("abap-cli-remote-candidates-local-export");
        let export_root = std::env::temp_dir().join("abap-cli-remote-candidates-local-export-d04");
        let _ = fs::remove_dir_all(&root);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(root.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZFIC/ddic-data-element"))
            .expect("export dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"
"#,
        )
        .expect("manifest");
        fs::write(root.join("src/reports/ZREP/ZREP.abap"), "REPORT zrep.").expect("report");
        fs::write(
            root.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lv_status TYPE zzf_status_code.\nDATA lv_missing TYPE zzf_missing.\n",
        )
        .expect("top include");
        fs::write(
            root.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("unit sidecar");
        fs::write(
            export_root.join("packages/ZFIC/ddic-data-element/ZZF_STATUS_CODE.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><dataElement />"#,
        )
        .expect("exported ddic");

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

        assert!(!names.contains(&"zzf_status_code"), "{names:?}");
        assert!(names.contains(&"zzf_missing"), "{names:?}");
        assert!(
            !source_candidates.contains(&"zzf_status_code"),
            "{source_candidates:?}"
        );
        assert!(
            source_candidates.contains(&"zzf_missing"),
            "{source_candidates:?}"
        );

        let _ = fs::remove_dir_all(&root);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn remote_candidates_try_local_export_before_remote_for_local_style_types() {
        let root = std::env::temp_dir().join("abap-cli-remote-candidates-local-style-type");
        let export_root =
            std::env::temp_dir().join("abap-cli-remote-candidates-local-style-type-d04");
        let _ = fs::remove_dir_all(&root);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(root.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZWM/ddic-table-type")).expect("export dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"
"#,
        )
        .expect("manifest");
        fs::write(root.join("src/reports/ZREP/ZREP.abap"), "REPORT zrep.").expect("report");
        fs::write(
            root.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lt_exported TYPE tt_ltap_vb.\nDATA lt_missing TYPE tt_missing_vb.\n",
        )
        .expect("top include");
        fs::write(
            root.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("unit sidecar");
        fs::write(
            export_root.join("packages/ZWM/ddic-table-type/TT_LTAP_VB.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><tableType />"#,
        )
        .expect("exported ddic table type");

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

        assert!(!names.contains(&"tt_ltap_vb"), "{names:?}");
        assert!(names.contains(&"tt_missing_vb"), "{names:?}");
        assert!(
            !source_candidates.contains(&"tt_ltap_vb"),
            "{source_candidates:?}"
        );
        assert!(
            source_candidates.contains(&"tt_missing_vb"),
            "{source_candidates:?}"
        );

        let _ = fs::remove_dir_all(&root);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn remote_candidates_include_transitive_local_export_dependency_superclass() {
        let root =
            std::env::temp_dir().join("abap-cli-remote-candidates-local-export-public-superclass");
        let export_root = std::env::temp_dir()
            .join("abap-cli-remote-candidates-local-export-public-superclass-d04");
        let _ = fs::remove_dir_all(&root);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(root.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/global-class")).expect("export dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"
"#,
        )
        .expect("manifest");
        fs::write(
            root.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.\nDATA lo_doc TYPE REF TO zcl_document.\nSTART-OF-SELECTION.\n  lo_doc->add_text( ).\n",
        )
        .expect("report");
        fs::write(
            root.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("unit sidecar");
        fs::write(
            export_root.join("packages/ZPKG/global-class/ZCL_DOCUMENT.abap"),
            "CLASS zcl_document DEFINITION PUBLIC INHERITING FROM zcl_area CREATE PUBLIC.\n  PUBLIC SECTION.\n    METHODS display_document.\nENDCLASS.\nCLASS zcl_document IMPLEMENTATION.\n  METHOD display_document.\n  ENDMETHOD.\nENDCLASS.\n",
        )
        .expect("exported class");

        let workspace = load_remote_candidate_workspace(Some(root.to_string_lossy().as_ref()))
            .expect("remote candidates");
        let names: Vec<_> = workspace
            .candidates
            .iter()
            .map(|candidate| candidate.name.as_str())
            .collect();
        let report_uri = path_to_file_uri(&root.join("src/reports/ZREP/ZREP.abap"));
        let source_candidates = workspace
            .source_candidates
            .get(report_uri.as_str())
            .expect("report candidates")
            .iter()
            .map(|candidate| candidate.name.as_str())
            .collect::<Vec<_>>();

        assert!(names.contains(&"zcl_area"), "{names:?}");
        assert!(!names.contains(&"zcl_document"), "{names:?}");
        assert!(
            source_candidates.contains(&"zcl_area"),
            "{source_candidates:?}"
        );

        let _ = fs::remove_dir_all(&root);
        let _ = fs::remove_dir_all(&export_root);
    }
}
