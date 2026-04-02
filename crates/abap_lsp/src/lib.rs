use abap_cache::DocumentStore;
use lsp_types::{
    CompletionOptions, HoverProviderCapability, InitializeResult, OneOf, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};
use serde::{Deserialize, Serialize};

pub const RESOLVE_REMOTE_DEPENDENCIES: &str = "abapls/resolveRemoteDependencies";
pub const REMOTE_DEPENDENCIES_UPDATED: &str = "abapls/remoteDependenciesUpdated";
pub const WORKSPACE_MANIFEST_UPDATED: &str = "abapls/workspaceManifestUpdated";
pub const DEPENDENCY_CACHE_CLEARED: &str = "abapls/dependencyCacheCleared";

#[derive(Debug)]
pub struct ServerState {
    pub cache: DocumentStore,
    pub shutdown_requested: bool,
}

impl Default for ServerState {
    fn default() -> Self {
        Self {
            cache: DocumentStore::default(),
            shutdown_requested: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub name: &'static str,
    pub version: &'static str,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            name: "abap-lsp-rs",
            version: env!("CARGO_PKG_VERSION"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependencyCandidate {
    pub name: String,
    pub kind: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependencyResolveParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "sourceUri")]
    pub source_uri: String,
    #[serde(rename = "unknownSymbolMode", default)]
    pub unknown_symbol_mode: Option<String>,
    #[serde(rename = "remoteRequestParallelism", default)]
    pub remote_request_parallelism: Option<usize>,
    #[serde(rename = "remoteRequestsPerSecond", default)]
    pub remote_requests_per_second: Option<usize>,
    pub candidates: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependenciesUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "sourceUri")]
    pub source_uri: String,
    pub fetched: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkspaceManifestUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
}

pub fn initialize_result(config: &ServerConfig) -> InitializeResult {
    InitializeResult {
        server_info: Some(lsp_types::ServerInfo {
            name: config.name.to_owned(),
            version: Some(config.version.to_owned()),
        }),
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions::default()),
            definition_provider: Some(OneOf::Left(false)),
            ..ServerCapabilities::default()
        },
    }
}

#[cfg(test)]
mod tests {
    use super::{
        DEPENDENCY_CACHE_CLEARED, REMOTE_DEPENDENCIES_UPDATED, RESOLVE_REMOTE_DEPENDENCIES,
        WORKSPACE_MANIFEST_UPDATED, initialize_result,
    };

    #[test]
    fn initialize_result_exposes_server_capabilities() {
        let result = initialize_result(&Default::default());

        assert!(result.capabilities.text_document_sync.is_some());
        assert!(result.server_info.is_some());
    }

    #[test]
    fn custom_notification_names_are_stable() {
        assert_eq!(RESOLVE_REMOTE_DEPENDENCIES, "abapls/resolveRemoteDependencies");
        assert_eq!(REMOTE_DEPENDENCIES_UPDATED, "abapls/remoteDependenciesUpdated");
        assert_eq!(WORKSPACE_MANIFEST_UPDATED, "abapls/workspaceManifestUpdated");
        assert_eq!(DEPENDENCY_CACHE_CLEARED, "abapls/dependencyCacheCleared");
    }
}
