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

        let path = if let Some(path) = settings.path.clone() {
            path
        } else if let Some(path) = self.shell_env_var(worktree, ENV_SERVER_PATH) {
            path
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
}

fn binary_name() -> &'static str {
    let (os, _) = zed::current_platform();
    match os {
        zed::Os::Windows => "abap_language_server.exe",
        _ => "abap_language_server",
    }
}

zed::register_extension!(AbapExtension);
