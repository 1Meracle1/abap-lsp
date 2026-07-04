use zed_extension_api::{self as zed, settings::LspSettings, LanguageServerId, Result};

const ENV_SERVER_PATH: &str = "__ABAP_LSP_SERVER_PATH";

struct AbapExtension;

#[derive(Default)]
struct ServerSettings {
    path: Option<String>,
    args: Vec<String>,
    env: Vec<(String, String)>,
}

struct ServerBinary {
    path: String,
    args: Vec<String>,
    env: Vec<(String, String)>,
}

impl AbapExtension {
    fn server_settings(
        &self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> ServerSettings {
        let Ok(settings) = LspSettings::for_worktree(language_server_id.as_ref(), worktree) else {
            return ServerSettings::default();
        };

        let Some(binary) = settings.binary else {
            return ServerSettings::default();
        };

        ServerSettings {
            path: binary.path,
            args: binary.arguments.unwrap_or_default(),
            env: binary.env.unwrap_or_default().into_iter().collect(),
        }
    }

    fn language_server_binary(
        &self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<ServerBinary> {
        let settings = self.server_settings(language_server_id, worktree);

        let path = if let Some(path) = settings.path.clone().and_then(non_empty_string) {
            self.command_path_for_worktree(worktree, path)
        } else if let Some(path) = self.shell_env_var(worktree, ENV_SERVER_PATH) {
            self.command_path_for_worktree(worktree, path)
        } else if let Some(path) = worktree.which(binary_name()) {
            path
        } else {
            return Err(format!(
                "could not find {binary}; configure lsp.{server}.binary.path, set {env_var}, or add it to PATH",
                binary = binary_name(),
                server = language_server_id.as_ref(),
                env_var = ENV_SERVER_PATH
            ));
        };

        Ok(ServerBinary {
            path,
            args: settings.args,
            env: settings.env,
        })
    }

    fn shell_env_var(&self, worktree: &zed::Worktree, name: &str) -> Option<String> {
        worktree
            .shell_env()
            .into_iter()
            .find_map(|(key, value)| (key == name).then_some(value))
            .and_then(non_empty_string)
    }

    fn command_path_for_worktree(&self, worktree: &zed::Worktree, path: String) -> String {
        if path_is_absolute(&path) || !path_has_separator(&path) {
            path
        } else {
            worktree_relative_path(worktree, &path)
        }
    }
}

impl zed::Extension for AbapExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let binary = self.language_server_binary(language_server_id, worktree)?;
        Ok(zed::Command {
            command: binary.path,
            args: binary.args,
            env: binary.env,
        })
    }

    fn language_server_initialization_options(
        &mut self,
        _language_server_id: &LanguageServerId,
        _worktree: &zed::Worktree,
    ) -> Result<Option<zed::serde_json::Value>> {
        Ok(Some(zed::serde_json::json!({
            "materializeDependencyDocuments": true,
        })))
    }
}

fn binary_name() -> &'static str {
    let (os, _) = zed::current_platform();
    match os {
        zed::Os::Windows => "abap_language_server.exe",
        _ => "abap_language_server",
    }
}

fn non_empty_string(value: String) -> Option<String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

fn path_is_absolute(path: &str) -> bool {
    let bytes = path.as_bytes();
    path.starts_with('/')
        || path.starts_with("\\\\")
        || (bytes.len() >= 3 && bytes[1] == b':' && (bytes[2] == b'/' || bytes[2] == b'\\'))
}

fn path_has_separator(path: &str) -> bool {
    path.contains('/') || path.contains('\\')
}

fn worktree_relative_path(worktree: &zed::Worktree, path: &str) -> String {
    let root = worktree.root_path();
    let path = path.trim_start_matches(|ch| ch == '/' || ch == '\\');
    if root.ends_with('/') || root.ends_with('\\') {
        format!("{root}{path}")
    } else {
        format!("{root}/{path}")
    }
}

zed::register_extension!(AbapExtension);
