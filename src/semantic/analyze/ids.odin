package abap_frontend_semantic_analyze

Source_File_Id :: distinct u32
Scope_Id :: distinct u32
Symbol_Id :: distinct u32
Reference_Id :: distinct u32
Structure_Id :: distinct u32
Decl_Info_Id :: distinct u32
Type_Id :: distinct u32
Provider_Id :: distinct u32

Entity_Id :: Symbol_Id

INVALID_SOURCE_FILE_ID :: Source_File_Id(0xffffffff)
INVALID_SCOPE_ID :: Scope_Id(0xffffffff)
INVALID_SYMBOL_ID :: Symbol_Id(0xffffffff)
INVALID_REFERENCE_ID :: Reference_Id(0xffffffff)
INVALID_STRUCTURE_ID :: Structure_Id(0xffffffff)
INVALID_DECL_INFO_ID :: Decl_Info_Id(0xffffffff)
INVALID_TYPE_ID :: Type_Id(0xffffffff)
INVALID_PROVIDER_ID :: Provider_Id(0xffffffff)
UNKNOWN_TYPE_ID :: Type_Id(0)
BUILTIN_PROVIDER_ID :: Provider_Id(0)

Provider_Kind :: enum {
	Invalid,
	Builtin,
	File,
	Summary_Provider,
}

Provider_Handle :: struct {
	kind:     Provider_Kind,
	id:       Provider_Id,
	revision: u64,
}

Entity_Handle :: struct {
	provider: Provider_Handle,
	id:       Entity_Id,
}

Scope_Handle :: struct {
	provider: Provider_Handle,
	id:       Scope_Id,
}

Decl_Handle :: struct {
	provider: Provider_Handle,
	id:       Decl_Info_Id,
}

Type_Handle :: struct {
	provider: Provider_Handle,
	id:       Type_Id,
}

Symbol_Link :: struct {
	unit:   Source_File_Id,
	symbol: Symbol_Id,
}

builtin_provider_handle :: #force_inline proc "contextless" () -> Provider_Handle {
	return Provider_Handle{kind = .Builtin, id = BUILTIN_PROVIDER_ID}
}

provider_handle_for_source_file :: proc "contextless" (source_file_id: Source_File_Id) -> Provider_Handle {
	if source_file_id == INVALID_SOURCE_FILE_ID {
		return Provider_Handle{}
	}
	return Provider_Handle{kind = .File, id = Provider_Id(u32(source_file_id))}
}

source_file_provider_handle :: proc "contextless" (source_file: ^Source_File_Provider) -> Provider_Handle {
	if source_file == nil {
		return Provider_Handle{}
	}
	return provider_handle_for_source_file(source_file.source_file_id)
}

provider_handle_for_dependency_summary :: proc "contextless" (
	id: Provider_Id,
	revision := u64(0),
) -> Provider_Handle {
	if id == INVALID_PROVIDER_ID {
		return Provider_Handle{}
	}
	return Provider_Handle{kind = .Summary_Provider, id = id, revision = revision}
}

provider_handle_is_valid :: #force_inline proc "contextless" (provider: Provider_Handle) -> bool {
	return provider.kind != .Invalid
}

provider_handle_source_file_id :: proc "contextless" (provider: Provider_Handle) -> (Source_File_Id, bool) {
	if provider.kind != .File {
		return INVALID_SOURCE_FILE_ID, false
	}
	return Source_File_Id(u32(provider.id)), true
}

scope_handle_for_source_file :: proc "contextless" (source_file_id: Source_File_Id, scope_id: Scope_Id) -> Scope_Handle {
	if source_file_id == INVALID_SOURCE_FILE_ID || scope_id == INVALID_SCOPE_ID {
		return Scope_Handle{}
	}
	return Scope_Handle{provider = provider_handle_for_source_file(source_file_id), id = scope_id}
}

decl_handle_for_source_file :: proc "contextless" (source_file_id: Source_File_Id, decl_id: Decl_Info_Id) -> Decl_Handle {
	if source_file_id == INVALID_SOURCE_FILE_ID || decl_id == INVALID_DECL_INFO_ID {
		return Decl_Handle{}
	}
	return Decl_Handle{provider = provider_handle_for_source_file(source_file_id), id = decl_id}
}

type_handle_for_source_file :: proc "contextless" (source_file_id: Source_File_Id, type_id: Type_Id) -> Type_Handle {
	if source_file_id == INVALID_SOURCE_FILE_ID || type_id == INVALID_TYPE_ID {
		return Type_Handle{}
	}
	return Type_Handle{provider = provider_handle_for_source_file(source_file_id), id = type_id}
}

type_handle_from_source_file_fact :: proc(unit: ^Source_File_Provider, fact: Type_Fact_Data) -> Type_Handle {
	if fact.type_id == INVALID_TYPE_ID {
		return Type_Handle{}
	}
	if !type_id_is_known(fact.type_id) {
		return Type_Handle{id = UNKNOWN_TYPE_ID}
	}
	if unit != nil && (fact.type_unit == INVALID_SOURCE_FILE_ID || fact.type_unit == unit.source_file_id) {
		if t := type_data(unit, fact.type_id); t != nil && t.kind == .Builtin {
			return Type_Handle{provider = builtin_provider_handle(), id = fact.type_id}
		}
	}
	return type_handle_for_source_file(fact.type_unit, fact.type_id)
}

entity_handle_from_source_file_symbol :: proc(
	unit: ^Source_File_Provider,
	symbol_id: Symbol_Id,
) -> (
	Entity_Handle,
	bool,
) {
	if unit == nil {
		return {}, false
	}
	s := symbol(unit, symbol_id)
	if s == nil {
		return {}, false
	}
	provider := source_file_provider_handle(unit)
	if symbol_kind_is_builtin(s.kind) {
		provider = builtin_provider_handle()
	}
	return Entity_Handle{provider = provider, id = s.id}, true
}

entity_handle_from_source_file_symbol_handle :: proc(
	unit: ^Source_File_Provider,
	handle: Symbol_Link,
) -> (
	Entity_Handle,
	bool,
) {
	if handle.unit == INVALID_SOURCE_FILE_ID || handle.symbol == INVALID_SYMBOL_ID {
		return {}, false
	}
	if unit != nil && handle.unit == unit.source_file_id {
		return entity_handle_from_source_file_symbol(unit, handle.symbol)
	}
	return Entity_Handle {
			provider = provider_handle_for_source_file(handle.unit),
			id = Entity_Id(handle.symbol),
		},
		true
}

entity_handle_from_symbol_handle :: proc(
	project: ^Project_Analysis,
	handle: Symbol_Link,
) -> (
	Entity_Handle,
	bool,
) {
	if project == nil {
		return entity_handle_from_source_file_symbol_handle(nil, handle)
	}
	unit := project_source_file_by_id(project, handle.unit)
	return entity_handle_from_source_file_symbol_handle(unit, handle)
}

symbol_handle_from_entity_handle :: proc(
	project: ^Project_Analysis,
	handle: Entity_Handle,
	legacy_unit := INVALID_SOURCE_FILE_ID,
) -> (
	Symbol_Link,
	bool,
) {
	if !provider_handle_is_valid(handle.provider) || handle.id == INVALID_SYMBOL_ID {
		return {}, false
	}
	#partial switch handle.provider.kind {
	case .File, .Summary_Provider:
		source_file_id := Source_File_Id(u32(handle.provider.id))
		symbol_id := Symbol_Id(handle.id)
		if project != nil {
			unit := project_source_file_by_id(project, source_file_id)
			if unit == nil || symbol(unit, symbol_id) == nil {
				return {}, false
			}
		}
		return Symbol_Link{unit = source_file_id, symbol = symbol_id}, true
	case .Builtin:
		source_file_id := legacy_unit
		if source_file_id == INVALID_SOURCE_FILE_ID && project != nil && len(project.providers.source_files) > 0 {
			source_file_id = project.providers.source_files[0].source_file_id
		}
		if source_file_id == INVALID_SOURCE_FILE_ID {
			return {}, false
		}
		symbol_id := Symbol_Id(handle.id)
		builtin := shared_builtin_provider()
		s := symbol(builtin, symbol_id)
		if s == nil || !symbol_kind_is_builtin(s.kind) {
			return {}, false
		}
		return Symbol_Link{unit = source_file_id, symbol = symbol_id}, true
	}
	return {}, false
}

provider_entity_lookup :: proc(
	project: ^Project_Analysis,
	provider: Provider_Handle,
	namespace: Namespace,
	name: string,
) -> (
	Entity_Handle,
	bool,
) {
	#partial switch provider.kind {
	case .Builtin:
		return builtin_entity_handle(namespace, name)
	case .File:
		if project == nil {
			return {}, false
		}
		source_file_id, ok := provider_handle_source_file_id(provider)
		if !ok {
			return {}, false
		}
		unit := project_source_file_by_id(project, source_file_id)
		if unit == nil {
			return {}, false
		}
		if symbol_id, found := scope_lookup_declaration(unit, unit.root_scope, namespace, name); found {
			return entity_handle_from_source_file_symbol(unit, symbol_id)
		}
		if entity, found := builtin_entity_handle(namespace, name); found {
			return entity, true
		}
	case .Summary_Provider:
		if project == nil {
			return {}, false
		}
		summary, summary_ok := summary_provider_slot(project, provider)
		if !summary_ok {
			return {}, false
		}
		if entity_id, found := summary_provider_entity_lookup(summary, namespace, name); found {
			return Entity_Handle{provider = provider, id = entity_id}, true
		}
	}
	return {}, false
}

project_source_file_by_id :: proc(project: ^Project_Analysis, source_file_id: Source_File_Id) -> ^Source_File_Provider {
	if project == nil {
		return nil
	}
	source_file_index := source_file_id_index(source_file_id)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return nil
	}
	if project.providers.source_files[source_file_index].source_file_id != source_file_id {
		return nil
	}
	return &project.providers.source_files[source_file_index]
}

source_file_id_index :: #force_inline proc(id: Source_File_Id) -> int {
	return int(id)
}

scope_id_index :: #force_inline proc(id: Scope_Id) -> int {
	return int(id)
}

symbol_id_index :: #force_inline proc(id: Symbol_Id) -> int {
	return int(id)
}

reference_id_index :: #force_inline proc(id: Reference_Id) -> int {
	return int(id)
}

structure_id_index :: #force_inline proc(id: Structure_Id) -> int {
	return int(id)
}

decl_info_id_index :: #force_inline proc(id: Decl_Info_Id) -> int {
	return int(id)
}

type_id_index :: #force_inline proc(id: Type_Id) -> int {
	return int(id)
}
