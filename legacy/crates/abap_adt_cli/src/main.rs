//! CLI for querying SAP ADT objects from scripts, shells, and LLMs.

use std::env;

use abap_adt::{
    AdtClient, AdtObjectRef, ChildKind, ConnectionConfig, ConnectionOverrides, DdicKind,
    SourceKind, load_dotenv_defaults,
};
use serde_json::json;

type AppResult<T> = Result<T, String>;

#[derive(Debug, Clone)]
struct Cli {
    raw_output: bool,
    connection: ConnectionOverrides,
    command: Command,
}

#[derive(Debug, Clone)]
enum Command {
    Search {
        query: String,
        max_results: usize,
    },
    GetSource {
        kind: SourceKind,
        name: String,
        function_group: Option<String>,
    },
    GetDdic {
        kind: DdicKind,
        name: String,
    },
    Children {
        kind: ChildKind,
        name: String,
    },
}

#[derive(Debug)]
struct ArgCursor {
    args: Vec<String>,
    index: usize,
}

impl ArgCursor {
    fn new(args: Vec<String>) -> Self {
        Self { args, index: 0 }
    }

    fn next(&mut self) -> Option<String> {
        let value = self.args.get(self.index).cloned();
        if value.is_some() {
            self.index += 1;
        }
        value
    }

    fn next_required(&mut self, flag: &str) -> AppResult<String> {
        self.next()
            .ok_or_else(|| format!("expected value after {flag}\n{}", usage()))
    }
}

fn main() {
    if let Err(error) = run() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

fn run() -> AppResult<()> {
    let cli = parse_cli_args(env::args().skip(1))?;
    let dotenv = load_dotenv_defaults(None)?;
    let connection = ConnectionConfig::from_sources(&cli.connection, &dotenv)?;
    let mut client = AdtClient::new(connection)?;

    match cli.command {
        Command::Search { query, max_results } => {
            let results = client.search_repository_objects(&query, max_results)?;
            print_json(&json!({
                "command": "search",
                "query": query,
                "max_results": max_results,
                "results": results.iter().map(object_ref_json).collect::<Vec<_>>(),
            }))
        }
        Command::GetSource {
            kind,
            name,
            function_group,
        } => {
            let fetched = client.fetch_source(kind, &name, function_group.as_deref())?;
            if cli.raw_output {
                print!("{}", fetched.body);
                return Ok(());
            }
            print_json(&json!({
                "command": "get-source",
                "kind": kind.as_str(),
                "name": name,
                "function_group": function_group,
                "resolved_by": fetched.resolved_by,
                "object_uri": fetched.object_uri,
                "request_url": fetched.request_url,
                "source": fetched.body,
            }))
        }
        Command::GetDdic { kind, name } => {
            let fetched = client.fetch_ddic(kind, &name)?;
            if cli.raw_output {
                print!("{}", fetched.body);
                return Ok(());
            }
            print_json(&json!({
                "command": "get-ddic",
                "kind": kind.as_str(),
                "name": name,
                "request_url": fetched.request_url,
                "xml": fetched.body,
            }))
        }
        Command::Children { kind, name } => {
            let (structure, children) = client.list_children(kind, &name)?;
            print_json(&json!({
                "command": "children",
                "kind": kind.as_str(),
                "name": name,
                "categories": structure.categories.iter().map(|entry| json!({
                    "category": entry.category,
                    "label": entry.label,
                })).collect::<Vec<_>>(),
                "object_types": structure.object_types.iter().map(|entry| json!({
                    "object_type": entry.object_type,
                    "category_tag": entry.category_tag,
                    "label": entry.label,
                    "node_id": entry.node_id,
                })).collect::<Vec<_>>(),
                "children": children.iter().map(|entry| json!({
                    "category_tag": entry.category_tag,
                    "object_type_label": entry.object_type_label,
                    "object_type": entry.object_type,
                    "name": entry.name,
                    "uri": entry.uri,
                    "vit_uri": entry.vit_uri,
                    "expandable": entry.expandable,
                })).collect::<Vec<_>>(),
            }))
        }
    }
}

fn object_ref_json(entry: &AdtObjectRef) -> serde_json::Value {
    json!({
        "uri": entry.uri,
        "type": entry.object_type,
        "name": entry.name,
        "package_name": entry.package_name,
        "description": entry.description,
    })
}

fn parse_cli_args(it: impl Iterator<Item = String>) -> AppResult<Cli> {
    let args = it.collect::<Vec<_>>();
    if args.is_empty() {
        return Err(usage());
    }

    let mut cursor = ArgCursor::new(args);
    let mut raw_output = false;
    let mut connection = ConnectionOverrides::default();
    let command_word = next_non_common(&mut cursor, &mut raw_output, &mut connection)?;

    let command = match command_word.as_str() {
        "search" => parse_search_command(&mut cursor, &mut raw_output, &mut connection)?,
        "get" => parse_get_command(&mut cursor, &mut raw_output, &mut connection)?,
        "children" => parse_children_command(&mut cursor, &mut raw_output, &mut connection)?,
        "help" | "-h" | "--help" => return Err(usage()),
        other => return Err(format!("unknown command {other:?}\n{}", usage())),
    };

    if raw_output && !matches!(command, Command::GetSource { .. } | Command::GetDdic { .. }) {
        return Err(format!(
            "--raw only applies to `get source` and `get ddic`\n{}",
            usage()
        ));
    }

    Ok(Cli {
        raw_output,
        connection,
        command,
    })
}

fn parse_search_command(
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<Command> {
    let mut query = None;
    let mut max_results = 51usize;

    while let Some(arg) = cursor.next() {
        if handle_common_arg(arg.as_str(), cursor, raw_output, connection)? {
            continue;
        }
        if arg == "--max-results" {
            max_results = parse_usize(&cursor.next_required("--max-results")?, "--max-results")?;
            continue;
        }
        if let Some(value) = arg.strip_prefix("--max-results=") {
            max_results = parse_usize(value, "--max-results")?;
            continue;
        }
        if arg.starts_with('-') {
            return Err(format!("unknown option {arg:?}\n{}", usage()));
        }
        if query.replace(arg).is_some() {
            return Err(format!("unexpected extra argument\n{}", usage()));
        }
    }

    Ok(Command::Search {
        query: query.ok_or_else(|| format!("search requires a query\n{}", usage()))?,
        max_results,
    })
}

fn parse_get_command(
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<Command> {
    let family = next_non_common(cursor, raw_output, connection)?;
    match family.as_str() {
        "source" => parse_get_source_command(cursor, raw_output, connection),
        "ddic" => parse_get_ddic_command(cursor, raw_output, connection),
        other => Err(format!(
            "unknown `get` family {other:?}; expected `source` or `ddic`\n{}",
            usage()
        )),
    }
}

fn parse_get_source_command(
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<Command> {
    let kind_token = next_non_common(cursor, raw_output, connection)?;
    let kind = SourceKind::parse(&kind_token).ok_or_else(|| {
        format!(
            "unknown source kind {kind_token:?}; expected report/include/class/function-group/function-module/interface\n{}",
            usage()
        )
    })?;

    let mut name = None;
    let mut function_group = None;
    while let Some(arg) = cursor.next() {
        if handle_common_arg(arg.as_str(), cursor, raw_output, connection)? {
            continue;
        }
        if arg == "--group" {
            function_group = Some(cursor.next_required("--group")?);
            continue;
        }
        if let Some(value) = arg.strip_prefix("--group=") {
            function_group = Some(value.to_string());
            continue;
        }
        if arg.starts_with('-') {
            return Err(format!("unknown option {arg:?}\n{}", usage()));
        }
        if name.replace(arg).is_some() {
            return Err(format!("unexpected extra argument\n{}", usage()));
        }
    }

    Ok(Command::GetSource {
        kind,
        name: name.ok_or_else(|| format!("get source requires an object name\n{}", usage()))?,
        function_group,
    })
}

fn parse_get_ddic_command(
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<Command> {
    let kind_token = next_non_common(cursor, raw_output, connection)?;
    let kind = DdicKind::parse(&kind_token).ok_or_else(|| {
        format!(
            "unknown DDIC kind {kind_token:?}; expected data-element/table-type/structure/view/table\n{}",
            usage()
        )
    })?;

    let mut name = None;
    while let Some(arg) = cursor.next() {
        if handle_common_arg(arg.as_str(), cursor, raw_output, connection)? {
            continue;
        }
        if arg.starts_with('-') {
            return Err(format!("unknown option {arg:?}\n{}", usage()));
        }
        if name.replace(arg).is_some() {
            return Err(format!("unexpected extra argument\n{}", usage()));
        }
    }

    Ok(Command::GetDdic {
        kind,
        name: name.ok_or_else(|| format!("get ddic requires an object name\n{}", usage()))?,
    })
}

fn parse_children_command(
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<Command> {
    let kind_token = next_non_common(cursor, raw_output, connection)?;
    let kind = ChildKind::parse(&kind_token).ok_or_else(|| {
        format!(
            "unknown children kind {kind_token:?}; expected package/report/function-group\n{}",
            usage()
        )
    })?;

    let mut name = None;
    while let Some(arg) = cursor.next() {
        if handle_common_arg(arg.as_str(), cursor, raw_output, connection)? {
            continue;
        }
        if arg.starts_with('-') {
            return Err(format!("unknown option {arg:?}\n{}", usage()));
        }
        if name.replace(arg).is_some() {
            return Err(format!("unexpected extra argument\n{}", usage()));
        }
    }

    Ok(Command::Children {
        kind,
        name: name.ok_or_else(|| format!("children requires an object name\n{}", usage()))?,
    })
}

fn next_non_common(
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<String> {
    while let Some(arg) = cursor.next() {
        if handle_common_arg(arg.as_str(), cursor, raw_output, connection)? {
            continue;
        }
        return Ok(arg);
    }
    Err(usage())
}

fn handle_common_arg(
    arg: &str,
    cursor: &mut ArgCursor,
    raw_output: &mut bool,
    connection: &mut ConnectionOverrides,
) -> AppResult<bool> {
    match arg {
        "-h" | "--help" | "help" => Err(usage()),
        "--raw" => {
            *raw_output = true;
            Ok(true)
        }
        "--url" => {
            connection.base_url = Some(cursor.next_required("--url")?);
            Ok(true)
        }
        "--user" => {
            connection.username = Some(cursor.next_required("--user")?);
            Ok(true)
        }
        "--password" => {
            connection.password = Some(cursor.next_required("--password")?);
            Ok(true)
        }
        "--sap-client" => {
            connection.sap_client = Some(cursor.next_required("--sap-client")?);
            Ok(true)
        }
        _ => {
            if let Some(value) = arg.strip_prefix("--url=") {
                connection.base_url = Some(value.to_string());
                return Ok(true);
            }
            if let Some(value) = arg.strip_prefix("--user=") {
                connection.username = Some(value.to_string());
                return Ok(true);
            }
            if let Some(value) = arg.strip_prefix("--password=") {
                connection.password = Some(value.to_string());
                return Ok(true);
            }
            if let Some(value) = arg.strip_prefix("--sap-client=") {
                connection.sap_client = Some(value.to_string());
                return Ok(true);
            }
            Ok(false)
        }
    }
}

fn parse_usize(value: &str, flag: &str) -> AppResult<usize> {
    value
        .parse::<usize>()
        .map_err(|_| format!("invalid value for {flag}: {value:?}"))
}

fn print_json(value: &serde_json::Value) -> AppResult<()> {
    let rendered = serde_json::to_string_pretty(value)
        .map_err(|e| format!("failed to serialize JSON: {e}"))?;
    println!("{rendered}");
    Ok(())
}

fn usage() -> String {
    r#"ABAP ADT query CLI.

Usage:
  abap-adt [connection options] search <query> [--max-results N]
  abap-adt [connection options] get source <kind> <name> [--group <function-group>] [--raw]
  abap-adt [connection options] get ddic <kind> <name> [--raw]
  abap-adt [connection options] children <kind> <name>

Connection options:
  --url <URL>              SAP host root or full ADT root
  --user <USER>            SAP username
  --password <PASSWORD>    SAP password
  --sap-client <CLIENT>    Optional SAP client, also read from ABAP_ADT_CLIENT / SAPCLIENT

The same connection values can come from:
  ABAP_ADT_URL / ABAP_ADT_BASE_URL / SAPBASE_URL
  ABAP_ADT_USER / ABAP_ADT_USERNAME / SAPUSER
  ABAP_ADT_PASSWORD / SAPPASS

Commands emit JSON by default. Use `--raw` on `get source` or `get ddic`
to print only the fetched source/XML.
"#
    .to_string()
}
