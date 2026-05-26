use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

use reqwest::Client as AsyncHttpClient;
use reqwest::blocking::Client as BlockingClient;
use reqwest::header::{ACCEPT, CACHE_CONTROL, CONTENT_TYPE};
use reqwest::{Method, Url};
use serde::{Deserialize, Serialize};

pub type AdtResult<T> = Result<T, String>;

const SESSION_BOOTSTRAP_ACCEPT: &str = "application/atom+xml;type=feed, application/xml";

#[derive(Debug, Default, Clone)]
pub struct ConnectionOverrides {
    pub base_url: Option<String>,
    pub username: Option<String>,
    pub password: Option<String>,
    pub sap_client: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ConnectionConfig {
    pub base_url: String,
    pub username: String,
    pub password: String,
    pub sap_client: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub struct DotenvDefaults {
    values: HashMap<String, String>,
}

impl ConnectionConfig {
    pub fn from_env_and_dotenv(start_dir: Option<&Path>) -> AdtResult<Self> {
        Self::from_sources(
            &ConnectionOverrides::default(),
            &load_dotenv_defaults(start_dir)?,
        )
    }

    pub fn from_sources(
        overrides: &ConnectionOverrides,
        dotenv: &DotenvDefaults,
    ) -> AdtResult<Self> {
        let base_url = overrides
            .base_url
            .clone()
            .or_else(|| first_config_value(&["ABAP_ADT_URL", "ABAP_ADT_BASE_URL", "SAPBASE_URL"], dotenv))
            .ok_or_else(|| {
                "missing SAP ADT base URL. Set `ABAP_ADT_URL`, `ABAP_ADT_BASE_URL`, or `SAPBASE_URL`.".to_string()
            })?;
        let username = overrides
            .username
            .clone()
            .or_else(|| {
                first_config_value(&["ABAP_ADT_USER", "ABAP_ADT_USERNAME", "SAPUSER"], dotenv)
            })
            .ok_or_else(|| {
                "missing SAP username. Set `ABAP_ADT_USER`, `ABAP_ADT_USERNAME`, or `SAPUSER`."
                    .to_string()
            })?;
        let password = overrides
            .password
            .clone()
            .or_else(|| first_config_value(&["ABAP_ADT_PASSWORD", "SAPPASS"], dotenv))
            .ok_or_else(|| {
                "missing SAP password. Set `ABAP_ADT_PASSWORD` or `SAPPASS`.".to_string()
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

    pub fn connection_key(&self) -> String {
        match self.sap_client.as_deref() {
            Some(client) if !client.trim().is_empty() => {
                format!("{}?sap-client={}", self.base_url, client.trim())
            }
            _ => self.base_url.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    Report,
    Include,
    Class,
    FunctionGroup,
    FunctionModule,
    Interface,
}

impl SourceKind {
    pub fn parse(raw: &str) -> Option<Self> {
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

    pub fn as_str(self) -> &'static str {
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
pub enum DdicKind {
    DataElement,
    TableType,
    Structure,
    View,
    Table,
}

impl DdicKind {
    pub fn parse(raw: &str) -> Option<Self> {
        match raw.to_ascii_lowercase().as_str() {
            "data-element" | "dataelement" | "dtel" => Some(Self::DataElement),
            "table-type" | "tabletype" | "ttyp" => Some(Self::TableType),
            "structure" | "struct" => Some(Self::Structure),
            "view" => Some(Self::View),
            "table" | "database-table" | "db-table" => Some(Self::Table),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
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
pub enum ChildKind {
    Package,
    Report,
    FunctionGroup,
}

impl ChildKind {
    pub fn parse(raw: &str) -> Option<Self> {
        match raw.to_ascii_lowercase().as_str() {
            "package" | "devclass" => Some(Self::Package),
            "report" | "prog" | "program" => Some(Self::Report),
            "function-group" | "functiongroup" | "fugr" => Some(Self::FunctionGroup),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AdtObjectRef {
    pub uri: String,
    #[serde(rename = "type", alias = "objectType")]
    pub object_type: String,
    pub name: String,
    #[serde(default, alias = "package_name")]
    pub package_name: String,
    #[serde(default)]
    pub description: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AdtDependencyFetchResult {
    pub body: String,
    pub file_extension: String,
    pub manifest_kind: String,
    #[serde(default)]
    pub shared_dependencies: Vec<AdtDependencyArtifact>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AdtDependencyArtifact {
    pub object_ref: AdtObjectRef,
    pub body: String,
    pub file_extension: String,
    pub manifest_kind: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AdtRepositoryChild {
    pub object_ref: AdtObjectRef,
    pub category_tag: String,
    pub object_type_label: String,
    pub expandable: bool,
}

#[derive(Debug, Clone)]
pub struct SourceFetch {
    pub request_url: String,
    pub object_uri: Option<String>,
    pub resolved_by: &'static str,
    pub body: String,
}

#[derive(Debug, Clone)]
pub struct DdicFetch {
    pub request_url: String,
    pub body: String,
}

#[derive(Debug, Clone)]
pub struct CategoryInfo {
    pub category: String,
    pub label: String,
}

#[derive(Debug, Clone)]
pub struct ObjectTypeInfo {
    pub object_type: String,
    pub category_tag: String,
    pub label: String,
    pub node_id: String,
}

#[derive(Debug, Clone)]
pub struct TreeNode {
    pub object_type: String,
    pub object_name: String,
    pub object_uri: String,
    pub object_vit_uri: String,
    pub expandable: bool,
}

#[derive(Debug, Clone, Default)]
pub struct RepositoryNodeStructure {
    pub tree_content: Vec<TreeNode>,
    pub categories: Vec<CategoryInfo>,
    pub object_types: Vec<ObjectTypeInfo>,
}

#[derive(Debug, Clone)]
pub struct ChildEntry {
    pub category_tag: String,
    pub object_type_label: String,
    pub object_type: String,
    pub name: String,
    pub uri: String,
    pub vit_uri: String,
    pub expandable: bool,
}

pub struct AdtClient {
    http: BlockingClient,
    connection: ConnectionConfig,
    csrf_token: Option<String>,
}

impl AdtClient {
    pub fn new(connection: ConnectionConfig) -> AdtResult<Self> {
        let http = BlockingClient::builder()
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

    pub fn connection_key(&self) -> String {
        self.connection.connection_key()
    }

    pub fn search_repository_objects(
        &mut self,
        query: &str,
        max_results: usize,
    ) -> AdtResult<Vec<AdtObjectRef>> {
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

    pub fn fetch_source(
        &mut self,
        kind: SourceKind,
        name: &str,
        function_group: Option<&str>,
    ) -> AdtResult<SourceFetch> {
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

    pub fn fetch_object_source(&mut self, object_uri: &str) -> AdtResult<String> {
        self.fetch_source_by_uri(object_uri, "direct")
            .map(|fetched| fetched.body)
    }

    pub fn fetch_dependency_object(
        &mut self,
        object_ref: &AdtObjectRef,
    ) -> AdtResult<AdtDependencyFetchResult> {
        if is_message_class_dependency_object(object_ref) {
            return Ok(AdtDependencyFetchResult {
                body: self.fetch_message_class(&object_ref.name)?,
                file_extension: "xml".to_string(),
                manifest_kind: "message-class".to_string(),
                shared_dependencies: Vec::new(),
            });
        }
        if is_fetchable_ddic_dependency_object(object_ref) {
            let kind = infer_ddic_manifest_kind(object_ref);
            if kind == "ddic-domain" {
                return Err(format!(
                    "DDIC domain {} is not fetchable through ADT",
                    object_ref.name
                ));
            }
            return Ok(AdtDependencyFetchResult {
                body: self.fetch_ddic_object(&kind, &object_ref.name)?,
                file_extension: "xml".to_string(),
                manifest_kind: kind,
                shared_dependencies: Vec::new(),
            });
        }
        if is_function_module_object(object_ref) {
            return self.fetch_function_module_dependency_source(object_ref);
        }

        Ok(AdtDependencyFetchResult {
            body: self.fetch_object_source(&object_ref.uri)?,
            file_extension: "abap".to_string(),
            manifest_kind: infer_repository_manifest_kind(object_ref),
            shared_dependencies: Vec::new(),
        })
    }

    pub fn fetch_ddic(&mut self, kind: DdicKind, name: &str) -> AdtResult<DdicFetch> {
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

    pub fn fetch_ddic_object(&mut self, kind: &str, name: &str) -> AdtResult<String> {
        let kind = match kind.trim().to_ascii_lowercase().as_str() {
            "ddic-data-element" => DdicKind::DataElement,
            "ddic-table-type" => DdicKind::TableType,
            "ddic-table" => DdicKind::Table,
            "ddic-view" => DdicKind::View,
            _ => DdicKind::Structure,
        };
        self.fetch_ddic(kind, name)
            .map(|fetched| format_ddic_xml(&fetched.body))
    }

    pub fn fetch_message_class(&mut self, name: &str) -> AdtResult<String> {
        let path = format!("/messageclass/{}", encode_path_segment(name));
        let url = self.base_endpoint(&path)?;
        let body = self.send_text(
            Method::GET,
            url,
            "application/vnd.sap.adt.elementinfo+xml",
            None,
        )?;
        Ok(format_ddic_xml(&body))
    }

    pub fn list_children(
        &mut self,
        kind: ChildKind,
        name: &str,
    ) -> AdtResult<(RepositoryNodeStructure, Vec<ChildEntry>)> {
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

    pub fn list_function_group_children(
        &mut self,
        function_group_name: &str,
    ) -> AdtResult<Vec<AdtRepositoryChild>> {
        let (_, children) = self.list_children(ChildKind::FunctionGroup, function_group_name)?;
        Ok(children
            .into_iter()
            .map(|child| AdtRepositoryChild {
                object_ref: AdtObjectRef {
                    uri: child.uri,
                    object_type: child.object_type,
                    name: child.name,
                    package_name: String::new(),
                    description: String::new(),
                },
                category_tag: child.category_tag,
                object_type_label: child.object_type_label,
                expandable: child.expandable,
            })
            .collect())
    }

    fn fetch_function_module_dependency_source(
        &mut self,
        object_ref: &AdtObjectRef,
    ) -> AdtResult<AdtDependencyFetchResult> {
        let function_module_source = self.fetch_object_source(&object_ref.uri)?;
        let Some(function_group_uri) = infer_function_group_uri(object_ref) else {
            return Ok(AdtDependencyFetchResult {
                body: function_module_source,
                file_extension: "abap".to_string(),
                manifest_kind: "function-module".to_string(),
                shared_dependencies: Vec::new(),
            });
        };

        let Ok(function_group_source) = self.fetch_object_source(&function_group_uri) else {
            return Ok(AdtDependencyFetchResult {
                body: function_module_source,
                file_extension: "abap".to_string(),
                manifest_kind: "function-module".to_string(),
                shared_dependencies: Vec::new(),
            });
        };

        let mut shared_dependencies = Vec::new();
        for include_name in extract_active_top_level_include_names(&function_group_source) {
            if is_function_group_dispatcher_include(&include_name) {
                continue;
            }
            if let Ok(body) = self.fetch_object_source(&format!(
                "/programs/includes/{}",
                encode_path_segment(&include_name)
            )) {
                shared_dependencies.push(AdtDependencyArtifact {
                    object_ref: build_include_object_ref(&include_name, &object_ref.package_name),
                    body,
                    file_extension: "abap".to_string(),
                    manifest_kind: "include".to_string(),
                });
            }
        }
        shared_dependencies.sort_by(|left, right| left.object_ref.name.cmp(&right.object_ref.name));
        Ok(AdtDependencyFetchResult {
            body: build_function_module_dependency_source(
                &function_group_source,
                &function_module_source,
            ),
            file_extension: "abap".to_string(),
            manifest_kind: "function-module".to_string(),
            shared_dependencies,
        })
    }

    fn fetch_repository_node_structure(
        &mut self,
        parent_name: &str,
        parent_type: &str,
        node_keys: &[&str],
    ) -> AdtResult<RepositoryNodeStructure> {
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

    fn fetch_source_by_path(&mut self, object_path: String) -> AdtResult<SourceFetch> {
        self.fetch_source_by_uri(&object_path, "direct")
    }

    fn fetch_source_by_uri(
        &mut self,
        object_uri: &str,
        resolved_by: &'static str,
    ) -> AdtResult<SourceFetch> {
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
    ) -> AdtResult<AdtObjectRef> {
        let normalized = query.trim();
        let results = self.search_repository_objects(query, 50)?;
        if let Some(object) = results
            .iter()
            .find(|candidate| {
                matcher(candidate) && candidate.name.trim().eq_ignore_ascii_case(normalized)
            })
            .cloned()
        {
            return Ok(object);
        }
        results
            .into_iter()
            .find(matcher)
            .ok_or_else(|| format!("no matching ADT object found for {query:?}"))
    }

    fn ensure_session(&mut self) -> AdtResult<()> {
        if self.csrf_token.is_some() {
            return Ok(());
        }

        let url = self.base_endpoint("/runtime/systemmessages")?;
        let response = self
            .http
            .request(Method::GET, url)
            .basic_auth(&self.connection.username, Some(&self.connection.password))
            .header(CACHE_CONTROL, "no-cache")
            .header(ACCEPT, SESSION_BOOTSTRAP_ACCEPT)
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
    ) -> AdtResult<String> {
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

    fn base_endpoint(&self, relative_path: &str) -> AdtResult<Url> {
        self.absolute_url(relative_path)
    }

    fn absolute_url(&self, path_or_url: &str) -> AdtResult<Url> {
        let mut normalized_path = path_or_url.to_string();
        if !path_or_url.starts_with("http://")
            && !path_or_url.starts_with("https://")
            && self
                .connection
                .base_url
                .to_ascii_lowercase()
                .contains("/sap/bc/adt")
            && path_or_url.to_ascii_lowercase().starts_with("/sap/bc/adt/")
        {
            normalized_path = path_or_url["/sap/bc/adt".len()..].to_string();
        }

        let mut url =
            if normalized_path.starts_with("http://") || normalized_path.starts_with("https://") {
                Url::parse(&normalized_path)
                    .map_err(|e| format!("invalid URL {normalized_path:?}: {e}"))?
            } else {
                let separator = if normalized_path.starts_with('/') {
                    ""
                } else {
                    "/"
                };
                Url::parse(&format!(
                    "{}{}{}",
                    self.connection.base_url, separator, normalized_path
                ))
                .map_err(|e| format!("invalid ADT endpoint {normalized_path:?}: {e}"))?
            };

        if let Some(client) = &self.connection.sap_client {
            if !url.query_pairs().any(|(key, _)| key == "sap-client") {
                url.query_pairs_mut().append_pair("sap-client", client);
            }
        }

        Ok(url)
    }
}

pub struct AsyncAdtClient {
    http: AsyncHttpClient,
    connection: ConnectionConfig,
    csrf_token: Option<String>,
}

impl AsyncAdtClient {
    pub fn new(connection: ConnectionConfig) -> AdtResult<Self> {
        let http = AsyncHttpClient::builder()
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

    pub async fn search_repository_objects(
        &mut self,
        query: &str,
        max_results: usize,
    ) -> AdtResult<Vec<AdtObjectRef>> {
        let mut url = self.base_endpoint("/repository/informationsystem/search")?;
        {
            let mut pairs = url.query_pairs_mut();
            pairs.append_pair("operation", "quickSearch");
            pairs.append_pair("query", query);
            pairs.append_pair("maxResults", &max_results.to_string());
        }
        let body = self
            .send_text(Method::GET, url, "application/xml", None)
            .await?;
        Ok(parse_object_references(&body))
    }

    pub async fn fetch_dependency_object(
        &mut self,
        object_ref: &AdtObjectRef,
    ) -> AdtResult<AdtDependencyFetchResult> {
        if is_message_class_dependency_object(object_ref) {
            return Ok(AdtDependencyFetchResult {
                body: self.fetch_message_class(&object_ref.name).await?,
                file_extension: "xml".to_string(),
                manifest_kind: "message-class".to_string(),
                shared_dependencies: Vec::new(),
            });
        }
        if is_fetchable_ddic_dependency_object(object_ref) {
            let kind = infer_ddic_manifest_kind(object_ref);
            if kind == "ddic-domain" {
                return Err(format!(
                    "DDIC domain {} is not fetchable through ADT",
                    object_ref.name
                ));
            }
            return Ok(AdtDependencyFetchResult {
                body: self.fetch_ddic_object(&kind, &object_ref.name).await?,
                file_extension: "xml".to_string(),
                manifest_kind: kind,
                shared_dependencies: Vec::new(),
            });
        }
        if is_function_module_object(object_ref) {
            return self
                .fetch_function_module_dependency_source(object_ref)
                .await;
        }

        Ok(AdtDependencyFetchResult {
            body: self.fetch_object_source(&object_ref.uri).await?,
            file_extension: "abap".to_string(),
            manifest_kind: infer_repository_manifest_kind(object_ref),
            shared_dependencies: Vec::new(),
        })
    }

    pub async fn fetch_ddic(&mut self, kind: DdicKind, name: &str) -> AdtResult<DdicFetch> {
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
                ).await?;
                Ok(DdicFetch { request_url, body })
            }
            DdicKind::TableType | DdicKind::Structure | DdicKind::View | DdicKind::Table => {
                let mut url = self.base_endpoint("/ddic/elementinfo")?;
                url.query_pairs_mut().append_pair("path", name);
                let request_url = url.to_string();
                let body = self
                    .send_text(
                        Method::GET,
                        url,
                        "application/vnd.sap.adt.elementinfo+xml",
                        None,
                    )
                    .await?;
                Ok(DdicFetch { request_url, body })
            }
        }
    }

    pub async fn fetch_ddic_object(&mut self, kind: &str, name: &str) -> AdtResult<String> {
        let kind = match kind.trim().to_ascii_lowercase().as_str() {
            "ddic-data-element" => DdicKind::DataElement,
            "ddic-table-type" => DdicKind::TableType,
            "ddic-table" => DdicKind::Table,
            "ddic-view" => DdicKind::View,
            _ => DdicKind::Structure,
        };
        self.fetch_ddic(kind, name)
            .await
            .map(|fetched| format_ddic_xml(&fetched.body))
    }

    pub async fn fetch_message_class(&mut self, name: &str) -> AdtResult<String> {
        let path = format!("/messageclass/{}", encode_path_segment(name));
        let url = self.base_endpoint(&path)?;
        let body = self
            .send_text(
                Method::GET,
                url,
                "application/vnd.sap.adt.elementinfo+xml",
                None,
            )
            .await?;
        Ok(format_ddic_xml(&body))
    }

    async fn fetch_function_module_dependency_source(
        &mut self,
        object_ref: &AdtObjectRef,
    ) -> AdtResult<AdtDependencyFetchResult> {
        let function_module_source = self.fetch_object_source(&object_ref.uri).await?;
        let Some(function_group_uri) = infer_function_group_uri(object_ref) else {
            return Ok(AdtDependencyFetchResult {
                body: function_module_source,
                file_extension: "abap".to_string(),
                manifest_kind: "function-module".to_string(),
                shared_dependencies: Vec::new(),
            });
        };

        let Ok(function_group_source) = self.fetch_object_source(&function_group_uri).await else {
            return Ok(AdtDependencyFetchResult {
                body: function_module_source,
                file_extension: "abap".to_string(),
                manifest_kind: "function-module".to_string(),
                shared_dependencies: Vec::new(),
            });
        };

        let mut shared_dependencies = Vec::new();
        for include_name in extract_active_top_level_include_names(&function_group_source) {
            if is_function_group_dispatcher_include(&include_name) {
                continue;
            }
            if let Ok(body) = self
                .fetch_object_source(&format!(
                    "/programs/includes/{}",
                    encode_path_segment(&include_name)
                ))
                .await
            {
                shared_dependencies.push(AdtDependencyArtifact {
                    object_ref: build_include_object_ref(&include_name, &object_ref.package_name),
                    body,
                    file_extension: "abap".to_string(),
                    manifest_kind: "include".to_string(),
                });
            }
        }
        shared_dependencies.sort_by(|left, right| left.object_ref.name.cmp(&right.object_ref.name));
        Ok(AdtDependencyFetchResult {
            body: build_function_module_dependency_source(
                &function_group_source,
                &function_module_source,
            ),
            file_extension: "abap".to_string(),
            manifest_kind: "function-module".to_string(),
            shared_dependencies,
        })
    }

    async fn fetch_object_source(&mut self, object_uri: &str) -> AdtResult<String> {
        let source_path = if object_uri.ends_with("/source/main") {
            object_uri.to_string()
        } else {
            format!("{object_uri}/source/main")
        };
        let url = self.absolute_url(&source_path)?;
        self.send_text(Method::GET, url, "text/plain", None).await
    }

    async fn ensure_session(&mut self) -> AdtResult<()> {
        if self.csrf_token.is_some() {
            return Ok(());
        }

        let url = self.base_endpoint("/runtime/systemmessages")?;
        let response = self
            .http
            .request(Method::GET, url)
            .basic_auth(&self.connection.username, Some(&self.connection.password))
            .header(CACHE_CONTROL, "no-cache")
            .header(ACCEPT, SESSION_BOOTSTRAP_ACCEPT)
            .header("x-csrf-token", "Fetch")
            .send()
            .await
            .map_err(|e| format!("failed to establish ADT session: {e}"))?;

        let status = response.status();
        let headers = response.headers().clone();
        let body = response
            .bytes()
            .await
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

    async fn send_text(
        &mut self,
        method: Method,
        url: Url,
        accept: &'static str,
        body: Option<(&'static str, String)>,
    ) -> AdtResult<String> {
        self.ensure_session().await?;

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
            .await
            .map_err(|e| format!("ADT request failed: {e}"))?;
        let status = response.status();
        let body = response
            .bytes()
            .await
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

    fn base_endpoint(&self, relative_path: &str) -> AdtResult<Url> {
        self.absolute_url(relative_path)
    }

    fn absolute_url(&self, path_or_url: &str) -> AdtResult<Url> {
        let mut normalized_path = path_or_url.to_string();
        if !path_or_url.starts_with("http://")
            && !path_or_url.starts_with("https://")
            && self
                .connection
                .base_url
                .to_ascii_lowercase()
                .contains("/sap/bc/adt")
            && path_or_url.to_ascii_lowercase().starts_with("/sap/bc/adt/")
        {
            normalized_path = path_or_url["/sap/bc/adt".len()..].to_string();
        }

        let mut url =
            if normalized_path.starts_with("http://") || normalized_path.starts_with("https://") {
                Url::parse(&normalized_path)
                    .map_err(|e| format!("invalid URL {normalized_path:?}: {e}"))?
            } else {
                let separator = if normalized_path.starts_with('/') {
                    ""
                } else {
                    "/"
                };
                Url::parse(&format!(
                    "{}{}{}",
                    self.connection.base_url, separator, normalized_path
                ))
                .map_err(|e| format!("invalid ADT endpoint {normalized_path:?}: {e}"))?
            };

        if let Some(client) = &self.connection.sap_client {
            if !url.query_pairs().any(|(key, _)| key == "sap-client") {
                url.query_pairs_mut().append_pair("sap-client", client);
            }
        }

        Ok(url)
    }
}

pub fn select_dependency_objects(
    query: &str,
    objects: &[AdtObjectRef],
    kind_hint: Option<&str>,
) -> Vec<AdtObjectRef> {
    let normalized_query = query.trim();
    if normalized_query.is_empty() {
        return Vec::new();
    }

    let supported_exact = dedupe_dependency_objects(
        objects
            .iter()
            .filter(|object_ref| {
                object_ref
                    .name
                    .trim()
                    .eq_ignore_ascii_case(normalized_query)
                    && is_supported_dependency_object(object_ref, None)
            })
            .cloned()
            .collect(),
    );
    if !supported_exact.is_empty() {
        return drop_shadowed_ddic_domain_objects(supported_exact);
    }

    let supported_by_hint = objects
        .iter()
        .filter(|object_ref| is_supported_dependency_object(object_ref, kind_hint))
        .cloned()
        .collect::<Vec<_>>();
    let fallback_supported = if supported_by_hint.is_empty() {
        objects
            .iter()
            .filter(|object_ref| is_supported_dependency_object(object_ref, None))
            .cloned()
            .collect::<Vec<_>>()
    } else {
        supported_by_hint
    };
    if fallback_supported.is_empty() {
        return Vec::new();
    }

    pick_best_dependency_object(query, &fallback_supported, kind_hint)
        .map(|object| vec![object])
        .unwrap_or_else(|| vec![fallback_supported[0].clone()])
}

pub fn direct_dependency_object_refs(name: &str, kind_hint: Option<&str>) -> Vec<AdtObjectRef> {
    match kind_hint
        .unwrap_or_default()
        .trim()
        .to_ascii_lowercase()
        .as_str()
    {
        "message-class" => vec![build_message_class_object_ref(name)],
        "include" => vec![build_include_object_ref(name, "")],
        "report" => vec![build_report_object_ref(name, "")],
        "static" => direct_class_interface_object_refs(name, true),
        "type" => direct_class_interface_object_refs(name, false),
        _ => Vec::new(),
    }
}

pub fn is_supported_dependency_object(object_ref: &AdtObjectRef, kind_hint: Option<&str>) -> bool {
    let object_type = object_ref.object_type.to_ascii_uppercase();
    let uri = object_ref.uri.to_ascii_lowercase();
    match kind_hint
        .unwrap_or_default()
        .trim()
        .to_ascii_lowercase()
        .as_str()
    {
        "message-class" => return is_message_class_dependency_object(object_ref),
        "include" => return uri.contains("/programs/includes/") || object_type == "PROG/I",
        "report" => return uri.contains("/programs/programs/") || object_type == "PROG/P",
        "function" => {
            return uri.contains("/functions/groups/")
                || object_type == "FUGR/F"
                || object_type == "FUGR/FF";
        }
        "static" => {
            return uri.contains("/oo/classes/")
                || uri.contains("/oo/interfaces/")
                || object_type.starts_with("CLAS/")
                || object_type.starts_with("INTF/");
        }
        "type" => {
            return is_fetchable_ddic_dependency_object(object_ref)
                || uri.contains("/oo/classes/")
                || uri.contains("/oo/interfaces/")
                || object_type.starts_with("CLAS/")
                || object_type.starts_with("INTF/");
        }
        _ => {}
    }
    uri.contains("/programs/includes/")
        || uri.contains("/programs/programs/")
        || uri.contains("/oo/classes/")
        || uri.contains("/oo/interfaces/")
        || uri.contains("/functions/groups/")
        || is_message_class_dependency_object(object_ref)
        || is_fetchable_ddic_dependency_object(object_ref)
        || object_type == "PROG/I"
        || object_type == "PROG/P"
        || object_type.starts_with("CLAS/")
        || object_type.starts_with("INTF/")
}

pub fn is_ddic_dependency_object(object_ref: &AdtObjectRef) -> bool {
    let object_type = object_ref.object_type.to_ascii_uppercase();
    object_type == "DTEL/DE"
        || is_ddic_domain_object(object_ref)
        || object_type == "TABL/DS"
        || object_type == "TABL/DT"
        || object_type == "TABL/DA"
        || object_type == "TTYP/DA"
        || object_type == "VIEW/DV"
}

pub fn is_message_class_dependency_object(object_ref: &AdtObjectRef) -> bool {
    object_ref.object_type.to_ascii_uppercase() == "MSAG/N"
        || object_ref
            .uri
            .to_ascii_lowercase()
            .contains("/sap/bc/adt/messageclass/")
}

pub fn is_function_module_object(object_ref: &AdtObjectRef) -> bool {
    object_ref.object_type.to_ascii_uppercase() == "FUGR/FF"
        || (object_ref
            .uri
            .to_ascii_lowercase()
            .contains("/functions/groups/")
            && object_ref.uri.to_ascii_lowercase().contains("/fmodules/"))
}

pub fn infer_function_group_uri(object_ref: &AdtObjectRef) -> Option<String> {
    let marker = "/functions/groups/";
    let lower = object_ref.uri.to_ascii_lowercase();
    let start = lower.find(marker)?;
    let tail = &object_ref.uri[start + marker.len()..];
    let group_end = tail.find('/').unwrap_or(tail.len());
    Some(object_ref.uri[..start + marker.len() + group_end].to_string())
}

pub fn infer_ddic_manifest_kind(object_ref: &AdtObjectRef) -> String {
    let object_type = object_ref.object_type.to_ascii_uppercase();
    if object_type.starts_with("DOMA/") {
        return "ddic-domain".to_string();
    }
    match object_type.as_str() {
        "DTEL/DE" => "ddic-data-element",
        "TABL/DS" => "ddic-structure",
        "TABL/DT" => "ddic-table",
        "TABL/DA" | "TTYP/DA" => "ddic-table-type",
        "VIEW/DV" => "ddic-view",
        _ => "ddic-structure",
    }
    .to_string()
}

pub fn infer_repository_manifest_kind(object_ref: &AdtObjectRef) -> String {
    let uri = object_ref.uri.to_ascii_lowercase();
    let object_type = object_ref.object_type.to_ascii_uppercase();
    if uri.contains("/programs/includes/") || object_type == "PROG/I" {
        return "include".to_string();
    }
    if uri.contains("/oo/classes/") || object_type.starts_with("CLAS/") {
        return "global-class".to_string();
    }
    if uri.contains("/oo/interfaces/") || object_type.starts_with("INTF/") {
        return "global-interface".to_string();
    }
    if uri.contains("/functions/groups/") {
        return "function-group".to_string();
    }
    "report".to_string()
}

pub fn build_message_class_object_ref(name: &str) -> AdtObjectRef {
    let name = name.trim().to_ascii_uppercase();
    AdtObjectRef {
        uri: format!("/sap/bc/adt/messageclass/{}", encode_path_segment(&name)),
        object_type: "MSAG/N".to_string(),
        name,
        package_name: String::new(),
        description: "Message class".to_string(),
    }
}

pub fn build_include_object_ref(name: &str, package_name: &str) -> AdtObjectRef {
    let name = name.trim().to_ascii_uppercase();
    AdtObjectRef {
        uri: format!(
            "/sap/bc/adt/programs/includes/{}",
            encode_path_segment(&name)
        ),
        object_type: "PROG/I".to_string(),
        name,
        package_name: package_name.to_string(),
        description: "Include".to_string(),
    }
}

pub fn build_report_object_ref(name: &str, package_name: &str) -> AdtObjectRef {
    let name = name.trim().to_ascii_uppercase();
    AdtObjectRef {
        uri: format!(
            "/sap/bc/adt/programs/programs/{}",
            encode_path_segment(&name)
        ),
        object_type: "PROG/P".to_string(),
        name,
        package_name: package_name.to_string(),
        description: "Report".to_string(),
    }
}

pub fn build_class_object_ref(name: &str, package_name: &str) -> AdtObjectRef {
    let name = name.trim().to_ascii_uppercase();
    AdtObjectRef {
        uri: format!("/sap/bc/adt/oo/classes/{}", encode_path_segment(&name)),
        object_type: "CLAS/OC".to_string(),
        name,
        package_name: package_name.to_string(),
        description: "Global class".to_string(),
    }
}

pub fn build_interface_object_ref(name: &str, package_name: &str) -> AdtObjectRef {
    let name = name.trim().to_ascii_uppercase();
    AdtObjectRef {
        uri: format!("/sap/bc/adt/oo/interfaces/{}", encode_path_segment(&name)),
        object_type: "INTF/OI".to_string(),
        name,
        package_name: package_name.to_string(),
        description: "Global interface".to_string(),
    }
}

pub fn extract_active_top_level_include_names(source: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for line in normalize_abap_source(source).lines() {
        let Some(include_name) = active_include_name_from_line(line) else {
            continue;
        };
        if seen.insert(include_name.clone()) {
            out.push(include_name);
        }
    }
    out
}

pub fn build_function_module_dependency_source(
    function_group_source: &str,
    function_module_source: &str,
) -> String {
    let rendered_group = normalize_abap_source(function_group_source)
        .lines()
        .map(|line| {
            let Some(include_name) = active_include_name_from_line(line) else {
                return line.to_string();
            };
            if is_function_group_dispatcher_include(&include_name) {
                format!(
                    "* INCLUDE {include_name}. Omitted in dependency cache; function module stays in its own unit."
                )
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "{}\n\n{}\n",
        trim_trailing_whitespace(&rendered_group),
        trim_trailing_whitespace(&normalize_abap_source(function_module_source))
    )
}

pub fn format_ddic_xml(xml: &str) -> String {
    let trimmed = xml.trim();
    if !trimmed.starts_with('<') {
        return xml.to_string();
    }

    let tokens = trimmed
        .replace("> <", "><")
        .replace(">\n<", "><")
        .split_inclusive('>')
        .flat_map(|part| {
            let mut out = Vec::new();
            for segment in part.split('<') {
                let segment = segment.trim();
                if segment.is_empty() {
                    continue;
                }
                if segment.ends_with('>') {
                    out.push(format!("<{segment}"));
                } else {
                    out.push(segment.to_string());
                }
            }
            out
        })
        .collect::<Vec<_>>();

    let mut lines = Vec::new();
    let mut indent = 0usize;
    for token in tokens {
        if !token.starts_with('<') {
            lines.push(format!("{}{}", "  ".repeat(indent), token));
            continue;
        }
        if token.starts_with("</") {
            indent = indent.saturating_sub(1);
            lines.push(format!("{}{}", "  ".repeat(indent), token));
            continue;
        }
        lines.push(format!("{}{}", "  ".repeat(indent), token));
        if !token.starts_with("<?") && !token.starts_with("<!") && !token.ends_with("/>") {
            indent += 1;
        }
    }
    format!("{}\n", lines.join("\n"))
}

pub fn parse_object_references(xml: &str) -> Vec<AdtObjectRef> {
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

pub fn parse_repository_node_structure(xml: &str) -> RepositoryNodeStructure {
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

pub fn normalize_base_url(raw: &str) -> String {
    let trimmed = raw.trim().trim_end_matches('/');
    if trimmed.to_ascii_lowercase().contains("/sap/bc/adt") {
        trimmed.to_string()
    } else {
        format!("{trimmed}/sap/bc/adt")
    }
}

pub fn encode_path_segment(value: &str) -> String {
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

pub fn load_dotenv_defaults(start_dir: Option<&Path>) -> AdtResult<DotenvDefaults> {
    for base in dotenv_search_bases(start_dir)? {
        if let Some(repo_root) = base.ancestors().find(|dir| dir.join(".git").exists()) {
            let repo_env = repo_root.join(".env");
            if repo_env.is_file() {
                return parse_dotenv_file(&repo_env);
            }
        }
        if let Some(fallback) = base
            .ancestors()
            .map(|dir| dir.join(".env"))
            .find(|path| path.is_file())
        {
            return parse_dotenv_file(&fallback);
        }
    }
    Ok(DotenvDefaults::default())
}

pub fn parse_dotenv_contents(content: &str) -> AdtResult<DotenvDefaults> {
    let mut values = HashMap::new();

    for (index, raw_line) in content.lines().enumerate() {
        let line_nr = index + 1;
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let line = line
            .strip_prefix("export ")
            .map(str::trim_start)
            .unwrap_or(line);
        let Some((raw_key, raw_value)) = line.split_once('=') else {
            return Err(format!("line {line_nr}: expected KEY=VALUE"));
        };
        let key = raw_key.trim();
        if key.is_empty() {
            return Err(format!("line {line_nr}: missing variable name"));
        }
        values.insert(
            key.to_string(),
            parse_dotenv_value(raw_value.trim())
                .map_err(|message| format!("line {line_nr}: {message}"))?,
        );
    }

    Ok(DotenvDefaults { values })
}

fn is_fetchable_ddic_dependency_object(object_ref: &AdtObjectRef) -> bool {
    is_ddic_dependency_object(object_ref) && !is_ddic_domain_object(object_ref)
}

fn is_ddic_domain_object(object_ref: &AdtObjectRef) -> bool {
    object_ref
        .object_type
        .to_ascii_uppercase()
        .starts_with("DOMA/")
}

fn pick_best_dependency_object(
    query: &str,
    objects: &[AdtObjectRef],
    kind_hint: Option<&str>,
) -> Option<AdtObjectRef> {
    let normalized_query = query.trim();
    if normalized_query.is_empty() {
        return None;
    }
    let supported = objects
        .iter()
        .filter(|object_ref| is_supported_dependency_object(object_ref, kind_hint))
        .cloned()
        .collect::<Vec<_>>();
    if supported.is_empty() {
        return None;
    }
    let exact = supported
        .iter()
        .filter(|object_ref| {
            object_ref
                .name
                .trim()
                .eq_ignore_ascii_case(normalized_query)
        })
        .cloned()
        .collect::<Vec<_>>();
    if !exact.is_empty() {
        return pick_preferred_dependency_object(&exact, kind_hint)
            .or_else(|| exact.first().cloned());
    }
    pick_preferred_dependency_object(&supported, kind_hint).or_else(|| supported.first().cloned())
}

fn pick_preferred_dependency_object(
    objects: &[AdtObjectRef],
    kind_hint: Option<&str>,
) -> Option<AdtObjectRef> {
    match kind_hint
        .unwrap_or_default()
        .trim()
        .to_ascii_lowercase()
        .as_str()
    {
        "report" => objects
            .iter()
            .find(|object_ref| object_ref.object_type.to_ascii_uppercase() == "PROG/P"),
        "function" => objects
            .iter()
            .find(|object_ref| object_ref.object_type.to_ascii_uppercase() == "FUGR/FF")
            .or_else(|| {
                objects
                    .iter()
                    .find(|object_ref| object_ref.object_type.to_ascii_uppercase() == "FUGR/F")
            }),
        "static" => objects
            .iter()
            .find(|object_ref| {
                object_ref
                    .object_type
                    .to_ascii_uppercase()
                    .starts_with("CLAS/")
            })
            .or_else(|| {
                objects.iter().find(|object_ref| {
                    object_ref
                        .object_type
                        .to_ascii_uppercase()
                        .starts_with("INTF/")
                })
            }),
        "type" => objects
            .iter()
            .find(|object_ref| {
                is_ddic_dependency_object(object_ref) && !is_ddic_domain_object(object_ref)
            })
            .or_else(|| {
                objects
                    .iter()
                    .find(|object_ref| is_ddic_domain_object(object_ref))
            })
            .or_else(|| {
                objects.iter().find(|object_ref| {
                    object_ref
                        .object_type
                        .to_ascii_uppercase()
                        .starts_with("CLAS/")
                })
            })
            .or_else(|| {
                objects.iter().find(|object_ref| {
                    object_ref
                        .object_type
                        .to_ascii_uppercase()
                        .starts_with("INTF/")
                })
            }),
        _ => None,
    }
    .cloned()
}

fn dedupe_dependency_objects(objects: Vec<AdtObjectRef>) -> Vec<AdtObjectRef> {
    let mut deduped = HashMap::<String, AdtObjectRef>::new();
    for object_ref in objects {
        let key = format!(
            "{}::{}",
            object_ref.object_type.to_ascii_uppercase(),
            object_ref.uri.to_ascii_lowercase()
        );
        deduped.entry(key).or_insert(object_ref);
    }
    let mut out = deduped.into_values().collect::<Vec<_>>();
    out.sort_by(|left, right| {
        left.object_type
            .cmp(&right.object_type)
            .then_with(|| left.uri.cmp(&right.uri))
    });
    out
}

fn drop_shadowed_ddic_domain_objects(objects: Vec<AdtObjectRef>) -> Vec<AdtObjectRef> {
    if objects.iter().any(|object_ref| {
        is_ddic_dependency_object(object_ref) && !is_ddic_domain_object(object_ref)
    }) {
        objects
            .into_iter()
            .filter(|object_ref| !is_ddic_domain_object(object_ref))
            .collect()
    } else {
        objects
    }
}

fn direct_class_interface_object_refs(name: &str, fallback_both_kinds: bool) -> Vec<AdtObjectRef> {
    if looks_like_global_interface_name(name) {
        return vec![build_interface_object_ref(name, "")];
    }
    if looks_like_global_class_name(name) {
        return vec![build_class_object_ref(name, "")];
    }
    if fallback_both_kinds {
        return vec![
            build_class_object_ref(name, ""),
            build_interface_object_ref(name, ""),
        ];
    }
    Vec::new()
}

fn looks_like_global_class_name(name: &str) -> bool {
    let local = local_object_name(name);
    local.starts_with("CL_")
        || local.starts_with("ZCL_")
        || local.starts_with("YCL_")
        || local.starts_with("CX_")
        || local.starts_with("ZCX_")
        || local.starts_with("YCX_")
}

fn looks_like_global_interface_name(name: &str) -> bool {
    let local = local_object_name(name);
    local.starts_with("IF_") || local.starts_with("ZIF_") || local.starts_with("YIF_")
}

fn local_object_name(name: &str) -> String {
    let normalized = name.trim().to_ascii_uppercase();
    if !normalized.starts_with('/') {
        return normalized;
    }
    normalized
        .split('/')
        .next_back()
        .filter(|value| !value.is_empty())
        .unwrap_or(&normalized)
        .to_string()
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

fn dotenv_search_bases(start_dir: Option<&Path>) -> AdtResult<Vec<PathBuf>> {
    let mut bases = Vec::new();
    if let Some(path) = start_dir {
        bases.push(path.to_path_buf());
    }
    bases.push(
        env::current_dir().map_err(|e| format!("failed to determine current directory: {e}"))?,
    );
    Ok(bases)
}

fn parse_dotenv_file(path: &Path) -> AdtResult<DotenvDefaults> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    parse_dotenv_contents(&content).map_err(|e| format!("failed to parse {}: {e}", path.display()))
}

fn parse_dotenv_value(raw: &str) -> AdtResult<String> {
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

    Ok(raw
        .split_once(" #")
        .map_or(raw, |(prefix, _)| prefix)
        .trim_end()
        .to_string())
}

fn build_node_structure_request_body(node_keys: &[&str]) -> String {
    let values = if node_keys.is_empty() {
        vec!["000000"]
    } else {
        node_keys.to_vec()
    };
    format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<asx:abap version=\"1.0\" xmlns:asx=\"http://www.sap.com/abapxml\">\n<asx:values>\n<DATA>\n{}\n</DATA>\n</asx:values>\n</asx:abap>",
        values
            .iter()
            .map(|value| format!("<TV_NODEKEY>{}</TV_NODEKEY>", escape_xml_text(value)))
            .collect::<Vec<_>>()
            .join("\n")
    )
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

fn escape_xml_text(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn active_include_name_from_line(line: &str) -> Option<String> {
    let trimmed = line.trim_start();
    if trimmed.starts_with('*') {
        return None;
    }
    let without_comment = line.split_once('"').map_or(line, |(prefix, _)| prefix);
    let words = without_comment
        .trim()
        .trim_end_matches('.')
        .split_whitespace()
        .collect::<Vec<_>>();
    if words.len() == 2 && words[0].eq_ignore_ascii_case("include") {
        Some(words[1].trim().to_ascii_uppercase())
    } else {
        None
    }
}

fn is_function_group_dispatcher_include(include_name: &str) -> bool {
    include_name.trim().to_ascii_uppercase().ends_with("UXX")
}

fn normalize_abap_source(source: &str) -> String {
    source.replace("\r\n", "\n")
}

fn trim_trailing_whitespace(source: &str) -> String {
    source.trim_end().to_string()
}

fn hex_upper(nibble: u8) -> char {
    match nibble {
        0..=9 => (b'0' + nibble) as char,
        10..=15 => (b'A' + (nibble - 10)) as char,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalizes_base_url_to_adt_root() {
        assert_eq!(
            normalize_base_url("https://host.example.com/"),
            "https://host.example.com/sap/bc/adt"
        );
        assert_eq!(
            normalize_base_url("https://host.example.com/sap/bc/adt/"),
            "https://host.example.com/sap/bc/adt"
        );
    }

    #[test]
    fn encodes_adt_path_segment() {
        assert_eq!(encode_path_segment("/STTP/DEMO"), "%2FSTTP%2FDEMO");
        assert_eq!(encode_path_segment("ZCL_DEMO"), "ZCL_DEMO");
    }

    #[test]
    fn parses_search_object_references() {
        let xml = r#"<feed xmlns:adtcore="http://www.sap.com/adt/core">
  <adtcore:objectReference adtcore:uri="/sap/bc/adt/oo/classes/ZCL_DEMO" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO" adtcore:packageName="ZPKG" adtcore:description="Demo"/>
</feed>"#;
        let refs = parse_object_references(xml);
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].name, "ZCL_DEMO");
        assert_eq!(refs[0].object_type, "CLAS/OC");
    }

    #[test]
    fn parses_repository_node_structure() {
        let xml = r#"<asx:values>
<SEU_ADT_OBJECT_TYPE_INFO><OBJECT_TYPE>FUGR/FF</OBJECT_TYPE><CATEGORY_TAG>FUNC</CATEGORY_TAG><OBJECT_TYPE_LABEL>Function Modules</OBJECT_TYPE_LABEL><NODE_ID>000001</NODE_ID></SEU_ADT_OBJECT_TYPE_INFO>
<SEU_ADT_REPOSITORY_OBJ_NODE><OBJECT_TYPE>FUGR/FF</OBJECT_TYPE><OBJECT_NAME>ZFM</OBJECT_NAME><OBJECT_URI>/sap/bc/adt/functions/groups/ZFG/fmodules/ZFM</OBJECT_URI><OBJECT_VIT_URI>vit</OBJECT_VIT_URI><EXPANDABLE>X</EXPANDABLE></SEU_ADT_REPOSITORY_OBJ_NODE>
</asx:values>"#;
        let structure = parse_repository_node_structure(xml);
        assert_eq!(structure.object_types.len(), 1);
        assert_eq!(structure.tree_content[0].object_name, "ZFM");
        assert!(structure.tree_content[0].expandable);
    }
}
