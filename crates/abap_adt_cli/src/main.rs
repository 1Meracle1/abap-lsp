//! CLI for querying SAP ADT objects from scripts, shells, and LLMs.
//!
//! By default, commands emit JSON so downstream tooling can consume results
//! without having to scrape table output. Use `--raw` on `get` commands to emit
//! the fetched source or XML payload directly.

use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::time::Duration;

use reqwest::Method;
use reqwest::Url;
use reqwest::blocking::Client;
use reqwest::header::{ACCEPT, CACHE_CONTROL, CONTENT_TYPE};
use serde_json::json;

type AppResult<T> = Result<T, String>;

#[derive(Debug, Default, Clone)]
struct ConnectionOverrides {
    base_url: Option<String>,
    username: Option<String>,
    password: Option<String>,
    sap_client: Option<String>,
}

#[derive(Debug, Clone)]
struct ConnectionConfig {
    base_url: String,
    username: String,
    password: String,
    sap_client: Option<String>,
}

#[derive(Debug, Default, Clone)]
struct DotenvDefaults {
    values: HashMap<String, String>,
}

impl ConnectionConfig {
    fn from_sources(overrides: &ConnectionOverrides, dotenv: &DotenvDefaults) -> AppResult<Self> {
        let base_url = overrides
            .base_url
            .clone()
            .or_else(|| {
                first_config_value(&["ABAP_ADT_URL", "ABAP_ADT_BASE_URL", "SAPBASE_URL"], dotenv)
            })
            .ok_or_else(|| {
                "missing SAP ADT base URL. Set `--url`, `ABAP_ADT_URL`, `ABAP_ADT_BASE_URL`, or `SAPBASE_URL`.".to_string()
            })?;
        let username = overrides
            .username
            .clone()
            .or_else(|| {
                first_config_value(&["ABAP_ADT_USER", "ABAP_ADT_USERNAME", "SAPUSER"], dotenv)
            })
            .ok_or_else(|| {
                "missing SAP username. Set `--user`, `ABAP_ADT_USER`, `ABAP_ADT_USERNAME`, or `SAPUSER`.".to_string()
            })?;
        let password = overrides
            .password
            .clone()
            .or_else(|| first_config_value(&["ABAP_ADT_PASSWORD", "SAPPASS"], dotenv))
            .ok_or_else(|| {
                "missing SAP password. Set `--password`, `ABAP_ADT_PASSWORD`, or `SAPPASS`."
                    .to_string()
            })?;
        let sap_client = overrides
            .sap_client
            .clone()
            .or_else(|| first_config_value(&["ABAP_ADT_CLIENT", "SAPCLIENT"], dotenv));

        Ok(Self {
            base_url: normalize_base_url(&base_url),
            username,
            password,
            sap_client,
        })
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SourceKind {
    Report,
    Include,
    Class,
    FunctionGroup,
    FunctionModule,
    Interface,
}

impl SourceKind {
    fn parse(raw: &str) -> Option<Self> {
        match raw.to_ascii_lowercase().as_str() {
            "report" | "prog" | "program" => Some(Self::Report),
            "include" => Some(Self::Include),
            "class" => Some(Self::Class),
            "function-group" | "functiongroup" | "fugr" => Some(Self::FunctionGroup),
            "function-module" | "functionmodule" | "fmodule" | "fm" => Some(Self::FunctionModule),
            "interface" | "intf" => Some(Self::Interface),
            _ => None,
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::Report => "report",
            Self::Include => "include",
            Self::Class => "class",
            Self::FunctionGroup => "function-group",
            Self::FunctionModule => "function-module",
            Self::Interface => "interface",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DdicKind {
    DataElement,
    TableType,
    Structure,
    View,
    Table,
}

impl DdicKind {
    fn parse(raw: &str) -> Option<Self> {
        match raw.to_ascii_lowercase().as_str() {
            "data-element" | "dataelement" | "dtel" => Some(Self::DataElement),
            "table-type" | "tabletype" | "ttyp" => Some(Self::TableType),
            "structure" | "struct" => Some(Self::Structure),
            "view" => Some(Self::View),
            "table" | "database-table" | "db-table" => Some(Self::Table),
            _ => None,
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::DataElement => "data-element",
            Self::TableType => "table-type",
            Self::Structure => "structure",
            Self::View => "view",
            Self::Table => "table",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChildKind {
    Package,
    Report,
    FunctionGroup,
}

impl ChildKind {
    fn parse(raw: &str) -> Option<Self> {
        match raw.to_ascii_lowercase().as_str() {
            "package" | "devclass" => Some(Self::Package),
            "report" | "prog" | "program" => Some(Self::Report),
            "function-group" | "functiongroup" | "fugr" => Some(Self::FunctionGroup),
            _ => None,
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::Package => "package",
            Self::Report => "report",
            Self::FunctionGroup => "function-group",
        }
    }

    fn parent_type(self) -> &'static str {
        match self {
            Self::Package => "DEVC/K",
            Self::Report => "PROG/P",
            Self::FunctionGroup => "FUGR/F",
        }
    }
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

#[derive(Debug, Clone)]
struct AdtObjectRef {
    uri: String,
    object_type: String,
    name: String,
    package_name: String,
    description: String,
}

#[derive(Debug, Clone)]
struct CategoryInfo {
    category: String,
    label: String,
}

#[derive(Debug, Clone)]
struct ObjectTypeInfo {
    object_type: String,
    category_tag: String,
    label: String,
    node_id: String,
}

#[derive(Debug, Clone)]
struct TreeNode {
    object_type: String,
    object_name: String,
    object_uri: String,
    object_vit_uri: String,
    expandable: bool,
}

#[derive(Debug, Clone, Default)]
struct RepositoryNodeStructure {
    tree_content: Vec<TreeNode>,
    categories: Vec<CategoryInfo>,
    object_types: Vec<ObjectTypeInfo>,
}

#[derive(Debug, Clone)]
struct SourceFetch {
    request_url: String,
    object_uri: Option<String>,
    resolved_by: &'static str,
    body: String,
}

#[derive(Debug, Clone)]
struct DdicFetch {
    request_url: String,
    body: String,
}

#[derive(Debug, Clone)]
struct ChildEntry {
    category_tag: String,
    object_type_label: String,
    object_type: String,
    name: String,
    uri: String,
    vit_uri: String,
    expandable: bool,
}

struct AdtClient {
    http: Client,
    connection: ConnectionConfig,
    csrf_token: Option<String>,
}

impl AdtClient {
    fn new(connection: ConnectionConfig) -> AppResult<Self> {
        let http = Client::builder()
            .cookie_store(true)
            .timeout(Duration::from_secs(60))
            .build()
            .map_err(|e| format!("failed to build HTTP client: {e}"))?;
        Ok(Self {
            http,
            connection,
            csrf_token: None,
        })
    }

    fn search_repository_objects(
        &mut self,
        query: &str,
        max_results: usize,
    ) -> AppResult<Vec<AdtObjectRef>> {
        let mut url = self.base_endpoint("/repository/informationsystem/search")?;
        {
            let mut pairs = url.query_pairs_mut();
            pairs.append_pair("operation", "quickSearch");
            pairs.append_pair("query", query);
            pairs.append_pair("maxResults", &max_results.to_string());
        }
        let body = self.send_text(Method::GET, url, "application/xml", None)?;
        Ok(parse_object_references(&body))
    }

    fn fetch_source(
        &mut self,
        kind: SourceKind,
        name: &str,
        function_group: Option<&str>,
    ) -> AppResult<SourceFetch> {
        match kind {
            SourceKind::Report => self
                .fetch_source_by_path(format!("/programs/programs/{}", encode_path_segment(name))),
            SourceKind::Include => self
                .fetch_source_by_path(format!("/programs/includes/{}", encode_path_segment(name))),
            SourceKind::Class => {
                self.fetch_source_by_path(format!("/oo/classes/{}", encode_path_segment(name)))
            }
            SourceKind::Interface => {
                self.fetch_source_by_path(format!("/oo/interfaces/{}", encode_path_segment(name)))
            }
            SourceKind::FunctionGroup => self
                .fetch_source_by_path(format!("/functions/groups/{}", encode_path_segment(name))),
            SourceKind::FunctionModule => {
                if let Some(group) = function_group {
                    self.fetch_source_by_path(format!(
                        "/functions/groups/{}/fmodules/{}",
                        encode_path_segment(group),
                        encode_path_segment(name)
                    ))
                } else {
                    let object = self.resolve_exact_object(name, matches_source_kind(kind))?;
                    self.fetch_source_by_uri(&object.uri, "search")
                }
            }
        }
    }

    fn fetch_ddic(&mut self, kind: DdicKind, name: &str) -> AppResult<DdicFetch> {
        match kind {
            DdicKind::DataElement => {
                let path = format!("/ddic/dataelements/{}", encode_path_segment(name));
                let url = self.base_endpoint(&path)?;
                let request_url = url.to_string();
                let body = self.send_text(
                    Method::GET,
                    url,
                    "application/vnd.sap.adt.dataelements.v1+xml, application/vnd.sap.adt.dataelements.v2+xml",
                    None,
                )?;
                Ok(DdicFetch { request_url, body })
            }
            DdicKind::TableType | DdicKind::Structure | DdicKind::View | DdicKind::Table => {
                let mut url = self.base_endpoint("/ddic/elementinfo")?;
                url.query_pairs_mut().append_pair("path", name);
                let request_url = url.to_string();
                let body = self.send_text(
                    Method::GET,
                    url,
                    "application/vnd.sap.adt.elementinfo+xml",
                    None,
                )?;
                Ok(DdicFetch { request_url, body })
            }
        }
    }

    fn list_children(
        &mut self,
        kind: ChildKind,
        name: &str,
    ) -> AppResult<(RepositoryNodeStructure, Vec<ChildEntry>)> {
        let root = self.fetch_repository_node_structure(name, kind.parent_type(), &[])?;
        let mut children = Vec::new();

        if root.object_types.is_empty() {
            children.extend(root.tree_content.iter().map(|node| ChildEntry {
                category_tag: String::new(),
                object_type_label: String::new(),
                object_type: node.object_type.clone(),
                name: node.object_name.clone(),
                uri: node.object_uri.clone(),
                vit_uri: node.object_vit_uri.clone(),
                expandable: node.expandable,
            }));
            return Ok((root, children));
        }

        for object_type in &root.object_types {
            if object_type.node_id.is_empty() {
                continue;
            }
            let branch = self.fetch_repository_node_structure(
                name,
                kind.parent_type(),
                &[object_type.node_id.as_str()],
            )?;
            for node in branch.tree_content {
                children.push(ChildEntry {
                    category_tag: object_type.category_tag.clone(),
                    object_type_label: object_type.label.clone(),
                    object_type: node.object_type,
                    name: node.object_name,
                    uri: node.object_uri,
                    vit_uri: node.object_vit_uri,
                    expandable: node.expandable,
                });
            }
        }

        Ok((root, children))
    }

    fn fetch_repository_node_structure(
        &mut self,
        parent_name: &str,
        parent_type: &str,
        node_keys: &[&str],
    ) -> AppResult<RepositoryNodeStructure> {
        let mut url = self.base_endpoint("/repository/nodestructure")?;
        {
            let mut pairs = url.query_pairs_mut();
            pairs.append_pair("parent_name", parent_name);
            pairs.append_pair("parent_tech_name", parent_name);
            pairs.append_pair("parent_type", parent_type);
            pairs.append_pair("withShortDescriptions", "true");
        }
        let body = build_node_structure_request_body(node_keys);
        let xml = self.send_text(
            Method::POST,
            url,
            "application/vnd.sap.as+xml;charset=UTF-8;dataname=com.sap.adt.RepositoryObjectTreeContent",
            Some((
                "application/vnd.sap.as+xml; charset=UTF-8; dataname=null",
                body,
            )),
        )?;
        Ok(parse_repository_node_structure(&xml))
    }

    fn fetch_source_by_path(&mut self, object_path: String) -> AppResult<SourceFetch> {
        self.fetch_source_by_uri(&object_path, "direct")
    }

    fn fetch_source_by_uri(
        &mut self,
        object_uri: &str,
        resolved_by: &'static str,
    ) -> AppResult<SourceFetch> {
        let source_path = if object_uri.ends_with("/source/main") {
            object_uri.to_string()
        } else {
            format!("{object_uri}/source/main")
        };
        let url = self.absolute_url(&source_path)?;
        let request_url = url.to_string();
        let body = self.send_text(Method::GET, url, "text/plain", None)?;
        Ok(SourceFetch {
            request_url,
            object_uri: Some(object_uri.to_string()),
            resolved_by,
            body,
        })
    }

    fn resolve_exact_object(
        &mut self,
        query: &str,
        matcher: fn(&AdtObjectRef) -> bool,
    ) -> AppResult<AdtObjectRef> {
        let normalized = query.trim().to_ascii_lowercase();
        let results = self.search_repository_objects(query, 50)?;
        let exact = results
            .iter()
            .find(|candidate| {
                matcher(candidate) && candidate.name.trim().eq_ignore_ascii_case(&normalized)
            })
            .cloned();
        if let Some(object) = exact {
            return Ok(object);
        }
        let fallback = results.into_iter().find(matcher);
        fallback.ok_or_else(|| format!("no matching ADT object found for {query:?}"))
    }

    fn ensure_session(&mut self) -> AppResult<()> {
        if self.csrf_token.is_some() {
            return Ok(());
        }

        let url = self.base_endpoint("/runtime/systemmessages")?;
        let response = self
            .http
            .request(Method::GET, url)
            .basic_auth(&self.connection.username, Some(&self.connection.password))
            .header(CACHE_CONTROL, "no-cache")
            .header(ACCEPT, "application/xml")
            .header("x-csrf-token", "Fetch")
            .send()
            .map_err(|e| format!("failed to establish ADT session: {e}"))?;

        let status = response.status();
        let headers = response.headers().clone();
        let body = response
            .bytes()
            .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
            .map_err(|e| format!("failed to read ADT session response: {e}"))?;

        if !status.is_success() {
            return Err(format!(
                "failed to establish ADT session ({}): {}",
                status.as_u16(),
                body
            ));
        }

        let token = headers
            .get("x-csrf-token")
            .and_then(|value| value.to_str().ok())
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .map(str::to_string)
            .ok_or_else(|| "SAP ADT did not return a CSRF token.".to_string())?;
        self.csrf_token = Some(token);
        Ok(())
    }

    fn send_text(
        &mut self,
        method: Method,
        url: Url,
        accept: &'static str,
        body: Option<(&'static str, String)>,
    ) -> AppResult<String> {
        self.ensure_session()?;

        let mut request = self
            .http
            .request(method, url)
            .basic_auth(&self.connection.username, Some(&self.connection.password))
            .header(CACHE_CONTROL, "no-cache")
            .header(ACCEPT, accept);
        if let Some(token) = &self.csrf_token {
            request = request.header("x-csrf-token", token);
        }
        if let Some((content_type, payload)) = body {
            request = request.header(CONTENT_TYPE, content_type).body(payload);
        }

        let response = request
            .send()
            .map_err(|e| format!("ADT request failed: {e}"))?;
        let status = response.status();
        let body = response
            .bytes()
            .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
            .map_err(|e| format!("failed to read ADT response body: {e}"))?;

        if !status.is_success() {
            return Err(format!(
                "ADT request failed ({}): {}",
                status.as_u16(),
                body
            ));
        }
        Ok(body)
    }

    fn base_endpoint(&self, relative_path: &str) -> AppResult<Url> {
        self.absolute_url(relative_path)
    }

    fn absolute_url(&self, path_or_url: &str) -> AppResult<Url> {
        let mut url = if path_or_url.starts_with("http://") || path_or_url.starts_with("https://") {
            Url::parse(path_or_url).map_err(|e| format!("invalid URL {path_or_url:?}: {e}"))?
        } else {
            let separator = if path_or_url.starts_with('/') {
                ""
            } else {
                "/"
            };
            Url::parse(&format!(
                "{}{}{}",
                self.connection.base_url, separator, path_or_url
            ))
            .map_err(|e| format!("invalid ADT endpoint {path_or_url:?}: {e}"))?
        };

        if let Some(client) = &self.connection.sap_client {
            let has_sap_client = url.query_pairs().any(|(key, _)| key == "sap-client");
            if !has_sap_client {
                url.query_pairs_mut().append_pair("sap-client", client);
            }
        }

        Ok(url)
    }
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }
}

fn run() -> AppResult<()> {
    let dotenv = load_dotenv_defaults()?;
    let cli = parse_cli_args(env::args().skip(1))?;
    let connection = ConnectionConfig::from_sources(&cli.connection, &dotenv)?;
    let mut client = AdtClient::new(connection)?;

    match cli.command {
        Command::Search { query, max_results } => {
            let results = client.search_repository_objects(&query, max_results)?;
            print_json(&json!({
                "command": "search",
                "query": query,
                "max_results": max_results,
                "results": results.iter().map(|entry| json!({
                    "uri": entry.uri,
                    "type": entry.object_type,
                    "name": entry.name,
                    "package_name": entry.package_name,
                    "description": entry.description,
                })).collect::<Vec<_>>(),
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

fn parse_cli_args(it: impl Iterator<Item = String>) -> AppResult<Cli> {
    let args: Vec<String> = it.collect();
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
        other => {
            return Err(format!("unknown command {other:?}\n{}", usage()));
        }
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
            let value = cursor.next_required("--max-results")?;
            max_results = value
                .parse::<usize>()
                .map_err(|_| format!("invalid value for --max-results: {value:?}"))?;
            continue;
        }
        if let Some(value) = arg.strip_prefix("--max-results=") {
            max_results = value
                .parse::<usize>()
                .map_err(|_| format!("invalid value for --max-results: {value:?}"))?;
            continue;
        }
        if arg.starts_with('-') {
            return Err(format!("unknown option {arg:?}\n{}", usage()));
        }
        if query.is_some() {
            return Err(format!("unexpected extra argument {arg:?}\n{}", usage()));
        }
        query = Some(arg);
    }

    let query = query.ok_or_else(|| format!("search requires a query\n{}", usage()))?;
    Ok(Command::Search { query, max_results })
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
        if name.is_some() {
            return Err(format!("unexpected extra argument {arg:?}\n{}", usage()));
        }
        name = Some(arg);
    }

    let name = name.ok_or_else(|| format!("get source requires an object name\n{}", usage()))?;
    Ok(Command::GetSource {
        kind,
        name,
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
        if name.is_some() {
            return Err(format!("unexpected extra argument {arg:?}\n{}", usage()));
        }
        name = Some(arg);
    }

    let name = name.ok_or_else(|| format!("get ddic requires an object name\n{}", usage()))?;
    Ok(Command::GetDdic { kind, name })
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
        if name.is_some() {
            return Err(format!("unexpected extra argument {arg:?}\n{}", usage()));
        }
        name = Some(arg);
    }

    let name = name.ok_or_else(|| format!("children requires an object name\n{}", usage()))?;
    Ok(Command::Children { kind, name })
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

fn matches_source_kind(kind: SourceKind) -> fn(&AdtObjectRef) -> bool {
    match kind {
        SourceKind::Report => |entry| entry.object_type == "PROG/P",
        SourceKind::Include => |entry| entry.object_type == "PROG/I",
        SourceKind::Class => |entry| entry.object_type.starts_with("CLAS/"),
        SourceKind::FunctionGroup => |entry| entry.object_type == "FUGR/F",
        SourceKind::FunctionModule => |entry| entry.object_type == "FUGR/FF",
        SourceKind::Interface => |entry| entry.object_type.starts_with("INTF/"),
    }
}

fn first_config_value(keys: &[&str], dotenv: &DotenvDefaults) -> Option<String> {
    first_env(keys).or_else(|| first_dotenv_value(keys, dotenv))
}

fn first_env(keys: &[&str]) -> Option<String> {
    keys.iter()
        .find_map(|key| normalized_non_empty(env::var(key).ok().as_deref()))
}

fn first_dotenv_value(keys: &[&str], dotenv: &DotenvDefaults) -> Option<String> {
    keys.iter()
        .find_map(|key| normalized_non_empty(dotenv.values.get(*key).map(String::as_str)))
}

fn normalized_non_empty(value: Option<&str>) -> Option<String> {
    value
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
}

fn load_dotenv_defaults() -> AppResult<DotenvDefaults> {
    let current_dir =
        env::current_dir().map_err(|e| format!("failed to determine current directory: {e}"))?;

    if let Some(repo_root) = current_dir
        .ancestors()
        .find(|dir| dir.join(".git").exists())
    {
        let repo_env = repo_root.join(".env");
        if repo_env.is_file() {
            return parse_dotenv_file(&repo_env);
        }
        return Ok(DotenvDefaults::default());
    }

    if let Some(fallback) = current_dir
        .ancestors()
        .map(|dir| dir.join(".env"))
        .find(|path| path.is_file())
    {
        return parse_dotenv_file(&fallback);
    }

    Ok(DotenvDefaults::default())
}

fn parse_dotenv_file(path: &Path) -> AppResult<DotenvDefaults> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    parse_dotenv_contents(&content).map_err(|e| format!("failed to parse {}: {e}", path.display()))
}

fn parse_dotenv_contents(content: &str) -> AppResult<DotenvDefaults> {
    let mut values = HashMap::new();

    for (index, raw_line) in content.lines().enumerate() {
        let line_nr = index + 1;
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let line = if let Some(rest) = line.strip_prefix("export ") {
            rest.trim_start()
        } else {
            line
        };

        let Some((raw_key, raw_value)) = line.split_once('=') else {
            return Err(format!("line {line_nr}: expected KEY=VALUE"));
        };

        let key = raw_key.trim();
        if key.is_empty() {
            return Err(format!("line {line_nr}: missing variable name"));
        }

        let value = parse_dotenv_value(raw_value.trim())
            .map_err(|message| format!("line {line_nr}: {message}"))?;
        values.insert(key.to_string(), value);
    }

    Ok(DotenvDefaults { values })
}

fn parse_dotenv_value(raw: &str) -> AppResult<String> {
    if raw.len() >= 2 {
        let first = raw.as_bytes()[0];
        let last = raw.as_bytes()[raw.len() - 1];
        if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
            return Ok(raw[1..raw.len() - 1].to_string());
        }
        if first == b'"' || first == b'\'' {
            return Err("unterminated quoted value".to_string());
        }
    } else if raw == "\"" || raw == "'" {
        return Err("unterminated quoted value".to_string());
    }

    let value = raw
        .split_once(" #")
        .map_or(raw, |(prefix, _)| prefix)
        .trim_end();
    Ok(value.to_string())
}

fn normalize_base_url(raw: &str) -> String {
    let trimmed = raw.trim().trim_end_matches('/');
    if trimmed.to_ascii_lowercase().contains("/sap/bc/adt") {
        trimmed.to_string()
    } else {
        format!("{trimmed}/sap/bc/adt")
    }
}

fn encode_path_segment(value: &str) -> String {
    let mut out = String::new();
    for byte in value.as_bytes() {
        let byte = *byte;
        if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.' | b'~') {
            out.push(byte as char);
        } else {
            out.push('%');
            out.push(hex_upper(byte >> 4));
            out.push(hex_upper(byte & 0x0F));
        }
    }
    out
}

fn hex_upper(nibble: u8) -> char {
    match nibble {
        0..=9 => (b'0' + nibble) as char,
        10..=15 => (b'A' + (nibble - 10)) as char,
        _ => unreachable!(),
    }
}

fn build_node_structure_request_body(node_keys: &[&str]) -> String {
    let mut body = String::from(
        r#"<?xml version="1.0" encoding="UTF-8" ?>
<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">
<asx:values>
<DATA>
"#,
    );

    if node_keys.is_empty() {
        body.push_str("<TV_NODEKEY>000000</TV_NODEKEY>\n");
    } else {
        for key in node_keys {
            body.push_str("<TV_NODEKEY>");
            body.push_str(key);
            body.push_str("</TV_NODEKEY>\n");
        }
    }

    body.push_str(
        r#"</DATA>
</asx:values>
</asx:abap>"#,
    );
    body
}

fn parse_object_references(xml: &str) -> Vec<AdtObjectRef> {
    let mut out = Vec::new();
    let needle = "<adtcore:objectReference";
    let mut search_from = 0usize;

    while let Some(rel_start) = xml[search_from..].find(needle) {
        let start = search_from + rel_start + needle.len();
        let Some(rel_end) = xml[start..].find('>') else {
            break;
        };
        let attrs = &xml[start..start + rel_end];
        out.push(AdtObjectRef {
            uri: decode_xml_entities(&read_attr(attrs, "adtcore:uri")),
            object_type: decode_xml_entities(&read_attr(attrs, "adtcore:type")),
            name: decode_xml_entities(&read_attr(attrs, "adtcore:name")),
            package_name: decode_xml_entities(&read_attr(attrs, "adtcore:packageName")),
            description: decode_xml_entities(&read_attr(attrs, "adtcore:description")),
        });
        search_from = start + rel_end + 1;
    }

    out.into_iter()
        .filter(|entry| !entry.uri.is_empty() && !entry.name.is_empty())
        .collect()
}

fn parse_repository_node_structure(xml: &str) -> RepositoryNodeStructure {
    let mut structure = RepositoryNodeStructure::default();

    for block in collect_blocks(xml, "SEU_ADT_REPOSITORY_OBJ_NODE") {
        structure.tree_content.push(TreeNode {
            object_type: read_tag_text(&block, "OBJECT_TYPE"),
            object_name: read_tag_text(&block, "OBJECT_NAME"),
            object_uri: read_tag_text(&block, "OBJECT_URI"),
            object_vit_uri: read_tag_text(&block, "OBJECT_VIT_URI"),
            expandable: read_tag_text(&block, "EXPANDABLE").eq_ignore_ascii_case("X"),
        });
    }

    for block in collect_blocks(xml, "SEU_ADT_OBJECT_CATEGORY_INFO") {
        structure.categories.push(CategoryInfo {
            category: read_tag_text(&block, "CATEGORY"),
            label: read_tag_text(&block, "CATEGORY_LABEL"),
        });
    }

    for block in collect_blocks(xml, "SEU_ADT_OBJECT_TYPE_INFO") {
        structure.object_types.push(ObjectTypeInfo {
            object_type: read_tag_text(&block, "OBJECT_TYPE"),
            category_tag: read_tag_text(&block, "CATEGORY_TAG"),
            label: read_tag_text(&block, "OBJECT_TYPE_LABEL"),
            node_id: read_tag_text(&block, "NODE_ID"),
        });
    }

    structure
}

fn collect_blocks(xml: &str, tag: &str) -> Vec<String> {
    let open = format!("<{tag}>");
    let close = format!("</{tag}>");
    let mut out = Vec::new();
    let mut index = 0usize;

    while let Some(rel_start) = xml[index..].find(&open) {
        let start = index + rel_start + open.len();
        let Some(rel_end) = xml[start..].find(&close) else {
            break;
        };
        let end = start + rel_end;
        out.push(xml[start..end].to_string());
        index = end + close.len();
    }

    out
}

fn read_attr(attrs: &str, name: &str) -> String {
    let needle = format!(r#"{name}=""#);
    let Some(start) = attrs.find(&needle) else {
        return String::new();
    };
    let value_start = start + needle.len();
    let Some(end) = attrs[value_start..].find('"') else {
        return String::new();
    };
    attrs[value_start..value_start + end].to_string()
}

fn read_tag_text(block: &str, tag: &str) -> String {
    let open = format!("<{tag}>");
    let close = format!("</{tag}>");
    let Some(start) = block.find(&open) else {
        return String::new();
    };
    let value_start = start + open.len();
    let Some(end) = block[value_start..].find(&close) else {
        return String::new();
    };
    decode_xml_entities(&block[value_start..value_start + end])
}

fn decode_xml_entities(value: &str) -> String {
    value
        .replace("&quot;", "\"")
        .replace("&apos;", "'")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&amp;", "&")
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

Startup:
  `abap-adt` automatically loads `.env` from the git repo root when run inside
  this repository. Existing environment variables and CLI flags still take precedence.

Source kinds:
  report, include, class, function-group, function-module, interface

DDIC kinds:
  data-element, table-type, structure, view, table

Children kinds:
  package, report, function-group

Notes:
  Commands emit JSON by default.
  Use `--raw` on `get source` or `get ddic` to print only the fetched source/XML.
  If `--url` does not already end in `/sap/bc/adt`, that suffix is added automatically.

Examples:
  abap-adt search "MARA"
  abap-adt get ddic table mara --raw
  abap-adt get source class zcl_demo
  abap-adt get source function-module bapi_user_get_detail
  abap-adt get source function-module bapi_user_get_detail --group suser
  abap-adt children report zmy_report
"#
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalizes_plain_host_to_adt_root() {
        assert_eq!(
            normalize_base_url("https://host.example.com"),
            "https://host.example.com/sap/bc/adt"
        );
        assert_eq!(
            normalize_base_url("https://host.example.com/sap/bc/adt/"),
            "https://host.example.com/sap/bc/adt"
        );
    }

    #[test]
    fn encodes_namespaced_object_names() {
        assert_eq!(encode_path_segment("/STTP/DEMO"), "%2FSTTP%2FDEMO");
        assert_eq!(encode_path_segment("ZCL_DEMO"), "ZCL_DEMO");
    }

    #[test]
    fn parses_search_object_references() {
        let xml = r#"
<feed>
  <adtcore:objectReference adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO" adtcore:packageName="ZPKG" adtcore:description="Demo class"/>
  <adtcore:objectReference adtcore:uri="/sap/bc/adt/programs/includes/zinc_demo" adtcore:type="PROG/I" adtcore:name="ZINC_DEMO" adtcore:packageName="ZPKG" adtcore:description="Demo include"/>
</feed>
"#;
        let refs = parse_object_references(xml);
        assert_eq!(refs.len(), 2);
        assert_eq!(refs[0].name, "ZCL_DEMO");
        assert_eq!(refs[1].object_type, "PROG/I");
    }

    #[test]
    fn parses_repository_node_structure() {
        let xml = r#"
<asx:values>
<DATA>
<TREE_CONTENT>
<SEU_ADT_REPOSITORY_OBJ_NODE>
<OBJECT_TYPE>PROG/I</OBJECT_TYPE>
<OBJECT_NAME>ZINC_ONE</OBJECT_NAME>
<OBJECT_URI>/sap/bc/adt/programs/includes/zinc_one</OBJECT_URI>
<OBJECT_VIT_URI></OBJECT_VIT_URI>
<EXPANDABLE></EXPANDABLE>
</SEU_ADT_REPOSITORY_OBJ_NODE>
</TREE_CONTENT>
<CATEGORIES>
<SEU_ADT_OBJECT_CATEGORY_INFO>
<CATEGORY>source_library</CATEGORY>
<CATEGORY_LABEL>Source Library</CATEGORY_LABEL>
</SEU_ADT_OBJECT_CATEGORY_INFO>
</CATEGORIES>
<OBJECT_TYPES>
<SEU_ADT_OBJECT_TYPE_INFO>
<OBJECT_TYPE>PROG/I</OBJECT_TYPE>
<CATEGORY_TAG>source_library</CATEGORY_TAG>
<OBJECT_TYPE_LABEL>Includes</OBJECT_TYPE_LABEL>
<NODE_ID>000123</NODE_ID>
</SEU_ADT_OBJECT_TYPE_INFO>
</OBJECT_TYPES>
</DATA>
</asx:values>
"#;
        let structure = parse_repository_node_structure(xml);
        assert_eq!(structure.tree_content.len(), 1);
        assert_eq!(structure.categories.len(), 1);
        assert_eq!(structure.object_types.len(), 1);
        assert_eq!(structure.tree_content[0].object_name, "ZINC_ONE");
        assert_eq!(structure.object_types[0].label, "Includes");
    }

    #[test]
    fn parses_get_source_cli_with_group() {
        let cli = parse_cli_args(
            [
                "get",
                "source",
                "function-module",
                "BAPI_USER_GET_DETAIL",
                "--group",
                "SUSR",
            ]
            .into_iter()
            .map(str::to_string),
        )
        .expect("cli");
        match cli.command {
            Command::GetSource {
                kind,
                name,
                function_group,
            } => {
                assert_eq!(kind, SourceKind::FunctionModule);
                assert_eq!(name, "BAPI_USER_GET_DETAIL");
                assert_eq!(function_group.as_deref(), Some("SUSR"));
            }
            other => panic!("unexpected command: {other:?}"),
        }
    }

    #[test]
    fn parses_dotenv_contents_with_comments_and_quotes() {
        let dotenv = parse_dotenv_contents(
            r#"
# comment
ABAP_ADT_URL=https://sap.example.com/sap/bc/adt
ABAP_ADT_USER="DEMO_USER"
ABAP_ADT_PASSWORD='secret value'
ABAP_ADT_CLIENT=100 # inline comment
"#,
        )
        .expect("dotenv");

        assert_eq!(
            dotenv.values.get("ABAP_ADT_URL").map(String::as_str),
            Some("https://sap.example.com/sap/bc/adt")
        );
        assert_eq!(
            dotenv.values.get("ABAP_ADT_USER").map(String::as_str),
            Some("DEMO_USER")
        );
        assert_eq!(
            dotenv.values.get("ABAP_ADT_PASSWORD").map(String::as_str),
            Some("secret value")
        );
        assert_eq!(
            dotenv.values.get("ABAP_ADT_CLIENT").map(String::as_str),
            Some("100")
        );
    }

    #[test]
    fn dotenv_values_are_used_when_env_is_missing() {
        let mut dotenv = DotenvDefaults::default();
        dotenv.values.insert(
            "ABAP_ADT_URL".to_string(),
            "https://sap.example.com".to_string(),
        );
        dotenv
            .values
            .insert("ABAP_ADT_USER".to_string(), "demo".to_string());
        dotenv
            .values
            .insert("ABAP_ADT_PASSWORD".to_string(), "secret".to_string());

        let config = ConnectionConfig::from_sources(&ConnectionOverrides::default(), &dotenv)
            .expect("config");
        assert_eq!(config.base_url, "https://sap.example.com/sap/bc/adt");
        assert_eq!(config.username, "demo");
        assert_eq!(config.password, "secret");
    }
}
