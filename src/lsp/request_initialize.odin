package abap_frontend_lsp

import json "core:encoding/json"
import "core:mem"
import "core:strings"

handle_initialize :: proc(ctx: ^Request_Context, params: json.Value) {
	state := ctx.state
	if object, ok := params.(json.Object); ok {
		if init_options, init_ok := object_object(object, "initializationOptions"); init_ok {
			if path, path_ok := object_string(init_options, "dependencyCachePath"); path_ok {
				state.options.dependency_store_path = strings.clone(path, state.allocator)
			}
		}
		opened := false
		if folders, folders_ok := object_array(object, "workspaceFolders");
		   folders_ok && len(folders) > 0 {
			for value in folders {
				folder, folder_ok := value.(json.Object)
				if !folder_ok {
					continue
				}
				uri, uri_ok := object_string(folder, "uri")
				if !uri_ok {
					continue
				}
				opened = open_workspace_for_uri(state, uri) || opened
			}
		}
		if !opened {
			uri, uri_ok := object_string(object, "rootUri")
			if uri_ok {
				open_workspace_for_uri(state, uri)
			}
		}
	}
	state.initialized = true
	send_success(ctx.output, ctx.id, initialize_result(state.allocator), state.allocator)
}

initialize_result :: proc(allocator: mem.Allocator) -> Initialize_Result {
	token_modifiers := make([]string, 2, allocator)
	token_modifiers[0] = "declaration"
	token_modifiers[1] = "readonly"

	trigger_characters := make([]string, 3, allocator)
	trigger_characters[0] = "-"
	trigger_characters[1] = ">"
	trigger_characters[2] = "~"

	file_operation_filters := make([]Workspace_File_Operation_Filter, 1, allocator)
	file_operation_filters[0] = Workspace_File_Operation_Filter {
		scheme = "file",
		pattern = Workspace_File_Operation_Pattern{glob = "**/*"},
	}
	file_operation_options := Workspace_File_Operation_Registration_Options {
		filters = file_operation_filters,
	}

	return Initialize_Result {
		capabilities = Server_Capabilities {
			text_document_sync = TEXT_DOCUMENT_SYNC_FULL,
			hover_provider = true,
			definition_provider = true,
			references_provider = true,
			rename_provider = Rename_Options{prepare_provider = true},
			completion_provider = Completion_Options{trigger_characters = trigger_characters},
			semantic_tokens_provider = Semantic_Tokens_Options {
				legend = Semantic_Tokens_Legend {
					token_types = initialize_semantic_token_types(allocator),
					token_modifiers = token_modifiers,
				},
				full = true,
			},
			folding_range_provider = true,
			workspace = Workspace_Server_Capabilities {
				workspace_folders = Workspace_Folders_Server_Capability {
					supported = true,
					change_notifications = true,
				},
				file_operations = Workspace_File_Operation_Server_Capabilities {
					did_create = file_operation_options,
					did_rename = file_operation_options,
					did_delete = file_operation_options,
				},
			},
		},
		server_info = Server_Info{name = "abap-lsp-odin", version = "0.1.0"},
	}
}

initialize_semantic_token_types :: proc(allocator: mem.Allocator) -> []string {
	out := make([]string, 11, allocator)
	out[0] = "type"
	out[1] = "class"
	out[2] = "interface"
	out[3] = "parameter"
	out[4] = "variable"
	out[5] = "property"
	out[6] = "function"
	out[7] = "method"
	out[8] = "event"
	out[9] = "namespace"
	out[10] = "enumMember"
	return out
}

Initialize_Result :: struct {
	capabilities: Server_Capabilities `json:"capabilities"`,
	server_info:  Server_Info `json:"serverInfo"`,
}

Server_Info :: struct {
	name:    string `json:"name"`,
	version: string `json:"version"`,
}

Server_Capabilities :: struct {
	text_document_sync:       int `json:"textDocumentSync"`,
	hover_provider:           bool `json:"hoverProvider"`,
	definition_provider:      bool `json:"definitionProvider"`,
	references_provider:      bool `json:"referencesProvider"`,
	rename_provider:          Rename_Options `json:"renameProvider"`,
	completion_provider:      Completion_Options `json:"completionProvider"`,
	semantic_tokens_provider: Semantic_Tokens_Options `json:"semanticTokensProvider"`,
	folding_range_provider:   bool `json:"foldingRangeProvider"`,
	workspace:                Workspace_Server_Capabilities `json:"workspace"`,
}

Rename_Options :: struct {
	prepare_provider: bool `json:"prepareProvider"`,
}

Completion_Options :: struct {
	trigger_characters: []string `json:"triggerCharacters"`,
}

Workspace_Server_Capabilities :: struct {
	workspace_folders: Workspace_Folders_Server_Capability `json:"workspaceFolders"`,
	file_operations:   Workspace_File_Operation_Server_Capabilities `json:"fileOperations"`,
}

Workspace_Folders_Server_Capability :: struct {
	supported:            bool `json:"supported"`,
	change_notifications: bool `json:"changeNotifications"`,
}

Workspace_File_Operation_Server_Capabilities :: struct {
	did_create: Workspace_File_Operation_Registration_Options `json:"didCreate"`,
	did_rename: Workspace_File_Operation_Registration_Options `json:"didRename"`,
	did_delete: Workspace_File_Operation_Registration_Options `json:"didDelete"`,
}

Workspace_File_Operation_Registration_Options :: struct {
	filters: []Workspace_File_Operation_Filter `json:"filters"`,
}

Workspace_File_Operation_Filter :: struct {
	scheme:  string `json:"scheme"`,
	pattern: Workspace_File_Operation_Pattern `json:"pattern"`,
}

Workspace_File_Operation_Pattern :: struct {
	glob: string `json:"glob"`,
}

Semantic_Tokens_Options :: struct {
	legend: Semantic_Tokens_Legend `json:"legend"`,
	full:   bool `json:"full"`,
}

Semantic_Tokens_Legend :: struct {
	token_types:     []string `json:"tokenTypes"`,
	token_modifiers: []string `json:"tokenModifiers"`,
}
