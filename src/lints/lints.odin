package abap_frontend_lints

import "src:semantic"
import "src:utils"

import "core:mem"
import "core:strings"

PROFILE_RECOMMENDED :: "recommended"
PROFILE_STRICT :: "strict"
PROFILE_ALL :: "all"
PROFILE_NONE :: "none"

UNREACHABLE_CODE :: "abap-lsp.unreachable-code"
USE_BEFORE_DEFINITE_ASSIGNMENT :: "abap-lsp.use-before-definite-assignment"
POSSIBLY_UNBOUND_FIELD_SYMBOL :: "abap-lsp.possibly-unbound-field-symbol"
DEAD_STORE :: "abap-lsp.dead-store"
UNSORTED_READ_TABLE_BINARY_SEARCH :: "abap-lsp.unsorted-read-table-binary-search"
SELECT_STAR :: "abap-lsp.select-star"
SELECT_IN_LOOP :: "abap-lsp.select-in-loop"
SELECT_SINGLE_WITHOUT_FULL_KEY :: "abap-lsp.select-single-without-full-key"
SELECT_INTO_FIELD_NAME_MISMATCH :: "abap-lsp.select-into-field-name-mismatch"
SELECT_INTO_FIELD_LENGTH_NARROWING :: "abap-lsp.select-into-field-length-narrowing"
FOR_ALL_ENTRIES_WITHOUT_GUARD :: "abap-lsp.for-all-entries-without-guard"
DYNAMIC_OPEN_SQL :: "abap-lsp.dynamic-open-sql"
IGNORED_AUTHORITY_CHECK :: "abap-lsp.ignored-authority-check"
IGNORED_CALL_FUNCTION_RESULT :: "abap-lsp.ignored-call-function-result"
EPC_UNVERIFIED_OPEN_SQL_SOURCE :: "epc.unverified-open-sql-source"
EPC_INVALID_OPEN_SQL_INTO_TARGET :: "epc.invalid-open-sql-into-target"
EPC_MISSING_TABLES_DECLARATION :: "epc.missing-tables-declaration"

Group :: enum {
	Correctness,
	Performance,
	Security,
	Style,
	Modernization,
	Package,
	Experimental,
}

Origin :: enum {
	Abap_Lsp,
	Sap_Extended_Program_Check,
	Sap_Code_Inspector,
	Sap_Atc,
}

Level :: enum {
	Allow,
	Info,
	Warn,
	Deny,
}

Diagnostic_Severity :: enum {
	Error,
	Warning,
	Information,
	Hint,
}

Suppression_Kind :: enum {
	Pragma,
	Pseudo_Comment,
	Abap_Lsp_Allow,
	Config,
}

Suppression :: struct {
	kind:  Suppression_Kind,
	range: semantic.Range,
	token: string,
}

Suppression_Target_Kind :: enum {
	Statement,
	File,
}

Suppression_Selector_Kind :: enum {
	Id,
	Sap_Alias,
}

Suppression_Selector :: struct {
	kind:  Suppression_Selector_Kind,
	value: string,
}

Suppression_Entry :: struct {
	target:    Suppression_Target_Kind,
	range:     semantic.Range,
	kind:      Suppression_Kind,
	token:     string,
	selectors: [dynamic]Suppression_Selector,
}

Metadata :: struct {
	id:              string,
	group:           Group,
	origin:          Origin,
	default_level:   Level,
	summary:         string,
	tags:            [4]string,
	tag_count:       int,
	sap_aliases:     [4]string,
	sap_alias_count: int,
}

Rule_Level :: struct {
	id:    string,
	level: Level,
}

Group_Level :: struct {
	group: Group,
	level: Level,
}

Config_Diagnostic_Kind :: enum {
	Unknown_Group,
	Unknown_Native_Rule,
	Unknown_Rule,
}

Config_Diagnostic :: struct {
	kind:    Config_Diagnostic_Kind,
	section: string,
	key:     string,
	message: string,
}

Sap_Atc_Mode :: enum {
	Off,
	Manual,
	On_Save,
}

Sap_Atc_Config :: struct {
	mode:          Sap_Atc_Mode,
	check_variant: string,
	configuration: string,
}

Config :: struct {
	profile:           string,
	report_suppressed: bool,
	sap_atc:           Sap_Atc_Config,
	groups:            [dynamic]Group_Level,
	rules:             [dynamic]Rule_Level,
	diagnostics:       [dynamic]Config_Diagnostic,
}

Policy :: struct {
	profile:           string,
	report_suppressed: bool,
	levels:            [dynamic]Rule_Level,
}

Diagnostic :: struct {
	id:              string,
	level:           Level,
	severity:        Diagnostic_Severity,
	group:           Group,
	origin:          Origin,
	tags:            [5]string,
	tag_count:       int,
	sap_aliases:     [4]string,
	sap_alias_count: int,
	suppressed:      bool,
	suppression:     Suppression,
	has_suppression: bool,
	range:           semantic.Range,
	message:         string,
	file:            ^semantic.Project_File,
}

REGISTRY :: [?]Metadata {
	{
		id = UNREACHABLE_CODE,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Warn,
		summary = "statement can never be executed",
		tags = [4]string{"control-flow", "unreachable", "", ""},
		tag_count = 2,
	},
	{
		id = USE_BEFORE_DEFINITE_ASSIGNMENT,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Warn,
		summary = "variable may be read before it is definitely assigned",
		tags = [4]string{"data-flow", "", "", ""},
		tag_count = 1,
	},
	{
		id = POSSIBLY_UNBOUND_FIELD_SYMBOL,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Warn,
		summary = "field symbol may be used before it is assigned",
		tags = [4]string{"data-flow", "field-symbol", "", ""},
		tag_count = 2,
	},
	{
		id = DEAD_STORE,
		group = .Style,
		origin = .Abap_Lsp,
		default_level = .Info,
		summary = "assigned value is overwritten or unused before it is read",
		tags = [4]string{"data-flow", "unused", "", ""},
		tag_count = 2,
		sap_aliases = [4]string{"NEEDED", "", "", ""},
		sap_alias_count = 1,
	},
	{
		id = UNSORTED_READ_TABLE_BINARY_SEARCH,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Info,
		summary = "READ TABLE ... BINARY SEARCH is used on a table not known to be sorted",
		tags = [4]string{"internal-table", "", "", ""},
		tag_count = 1,
	},
	{
		id = SELECT_STAR,
		group = .Performance,
		origin = .Sap_Code_Inspector,
		default_level = .Info,
		summary = "Open SQL SELECT * reads all columns instead of an explicit projection",
		tags = [4]string{"open-sql", "projection", "", ""},
		tag_count = 2,
		sap_aliases = [4]string{"CI_ALL_FIELDS_NEEDED", "", "", ""},
		sap_alias_count = 1,
	},
	{
		id = SELECT_IN_LOOP,
		group = .Performance,
		origin = .Sap_Code_Inspector,
		default_level = .Info,
		summary = "Open SQL SELECT runs inside a LOOP, DO, or WHILE body",
		tags = [4]string{"open-sql", "loop", "", ""},
		tag_count = 2,
		sap_aliases = [4]string{"CI_SEL_NESTED", "", "", ""},
		sap_alias_count = 1,
	},
	{
		id = SELECT_SINGLE_WITHOUT_FULL_KEY,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Info,
		summary = "Open SQL SELECT SINGLE does not restrict all known primary-key fields",
		tags = [4]string{"open-sql", "primary-key", "", ""},
		tag_count = 2,
	},
	{
		id = SELECT_INTO_FIELD_NAME_MISMATCH,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Info,
		summary = "Open SQL SELECT field name differs from the positional target field",
		tags = [4]string{"open-sql", "projection", "", ""},
		tag_count = 2,
	},
	{
		id = SELECT_INTO_FIELD_LENGTH_NARROWING,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Info,
		summary = "Open SQL SELECT field has a longer backing length than the positional target field",
		tags = [4]string{"open-sql", "projection", "", ""},
		tag_count = 2,
	},
	{
		id = FOR_ALL_ENTRIES_WITHOUT_GUARD,
		group = .Correctness,
		origin = .Sap_Code_Inspector,
		default_level = .Info,
		summary = "FOR ALL ENTRIES is used without an enclosing initial-table guard",
		tags = [4]string{"open-sql", "for-all-entries", "", ""},
		tag_count = 2,
		sap_aliases = [4]string{"CI_FAE_LINES_ENSURED", "", "", ""},
		sap_alias_count = 1,
	},
	{
		id = DYNAMIC_OPEN_SQL,
		group = .Security,
		origin = .Sap_Code_Inspector,
		default_level = .Info,
		summary = "Open SQL contains a dynamic source, projection, or WHERE fragment",
		tags = [4]string{"open-sql", "dynamic", "experimental", ""},
		tag_count = 3,
	},
	{
		id = IGNORED_AUTHORITY_CHECK,
		group = .Security,
		origin = .Sap_Atc,
		default_level = .Info,
		summary = "AUTHORITY-CHECK result is not checked before sy-subrc is overwritten",
		tags = [4]string{"authorization", "sy-subrc", "", ""},
		tag_count = 2,
	},
	{
		id = IGNORED_CALL_FUNCTION_RESULT,
		group = .Correctness,
		origin = .Abap_Lsp,
		default_level = .Info,
		summary = "CALL FUNCTION result is not handled before it is overwritten or ignored",
		tags = [4]string{"call-function", "sy-subrc", "", ""},
		tag_count = 2,
	},
	{
		id = EPC_UNVERIFIED_OPEN_SQL_SOURCE,
		group = .Correctness,
		origin = .Sap_Extended_Program_Check,
		default_level = .Deny,
		summary = "Open SQL source could not be verified against repository metadata",
		tags = [4]string{"open-sql", "repository", "", ""},
		tag_count = 2,
		sap_aliases = [4]string{"extended-program-check", "", "", ""},
		sap_alias_count = 1,
	},
	{
		id = EPC_INVALID_OPEN_SQL_INTO_TARGET,
		group = .Correctness,
		origin = .Sap_Extended_Program_Check,
		default_level = .Deny,
		summary = "Open SQL INTO or APPENDING target is incompatible with the query shape",
		tags = [4]string{"open-sql", "", "", ""},
		tag_count = 1,
		sap_aliases = [4]string{"extended-program-check", "", "", ""},
		sap_alias_count = 1,
	},
	{
		id = EPC_MISSING_TABLES_DECLARATION,
		group = .Correctness,
		origin = .Sap_Extended_Program_Check,
		default_level = .Deny,
		summary = "classic table work area is used without a matching TABLES declaration",
		tags = [4]string{"classic-abap", "", "", ""},
		tag_count = 1,
		sap_aliases = [4]string{"extended-program-check", "", "", ""},
		sap_alias_count = 1,
	},
}

config_default :: proc(allocator: mem.Allocator) -> Config {
	return Config {
		profile = strings.clone(PROFILE_RECOMMENDED, allocator),
		sap_atc = Sap_Atc_Config {
			mode = .Off,
			check_variant = strings.clone("DEFAULT", allocator),
		},
		groups = make([dynamic]Group_Level, 0, 2, allocator),
		rules = make([dynamic]Rule_Level, 0, 4, allocator),
		diagnostics = make([dynamic]Config_Diagnostic, 0, 2, allocator),
	}
}

policy_default :: proc(allocator: mem.Allocator) -> Policy {
	config := config_default(allocator)
	return policy_from_config(&config, allocator)
}

policy_from_config :: proc(config: ^Config, allocator: mem.Allocator) -> Policy {
	profile := PROFILE_RECOMMENDED
	report_suppressed := false
	groups: []Group_Level
	rules: []Rule_Level
	if config != nil {
		profile = normalize_profile(config.profile)
		report_suppressed = config.report_suppressed
		groups = config.groups[:]
		rules = config.rules[:]
	}
	policy := Policy {
		profile = strings.clone(profile, allocator),
		report_suppressed = report_suppressed,
		levels = make([dynamic]Rule_Level, 0, len(REGISTRY), allocator),
	}
	for metadata in REGISTRY {
		level := profile_level(profile, metadata)
		for override in groups {
			if override.group == metadata.group {
				level = override.level
			}
		}
		level = enforce_hard_error_level(metadata, level)
		append(
			&policy.levels,
			Rule_Level {
				id = strings.clone(metadata.id, allocator),
				level = level,
			},
		)
	}
	for override in rules {
		if metadata, ok := metadata_for(override.id); ok {
			level := enforce_hard_error_level(metadata, override.level)
			policy_set_level(&policy, metadata.id, level, allocator)
		} else if id_is_external_provider(override.id) {
			policy_set_level(&policy, normalized_id(override.id, allocator), override.level, allocator)
		}
	}
	return policy
}

policy_set_level :: proc(
	policy: ^Policy,
	id: string,
	level: Level,
	allocator: mem.Allocator,
) {
	normalized := normalized_id(id, context.temp_allocator)
	for &entry in policy.levels {
		if entry.id == normalized {
			entry.level = level
			return
		}
	}
	append(&policy.levels, Rule_Level{id = strings.clone(normalized, allocator), level = level})
}

policy_level_for :: proc(policy: ^Policy, id: string) -> Level {
	normalized := normalized_id(id, context.temp_allocator)
	if policy != nil {
		for entry in policy.levels {
			if entry.id == normalized {
				return entry.level
			}
		}
	}
	return .Allow
}

policy_enabled :: proc(policy: ^Policy, id: string) -> bool {
	return level_enabled(policy_level_for(policy, id))
}

level_enabled :: #force_inline proc "contextless" (level: Level) -> bool {
	return level != .Allow
}

profile_level :: proc(profile: string, metadata: Metadata) -> Level {
	switch profile {
	case PROFILE_NONE:
		return .Allow
	case PROFILE_STRICT:
		return strict_level(metadata.default_level)
	case PROFILE_ALL:
		return all_level(metadata.default_level)
	}
	if metadata.group == .Experimental {
		return .Allow
	}
	return metadata.default_level
}

enforce_hard_error_level :: #force_inline proc "contextless" (
	metadata: Metadata,
	level: Level,
) -> Level {
	return .Deny if metadata.default_level == .Deny else level
}

strict_level :: #force_inline proc "contextless" (level: Level) -> Level {
	switch level {
	case .Allow:
		return .Warn
	case .Info, .Warn, .Deny:
		return .Deny
	}
	return .Deny
}

all_level :: #force_inline proc "contextless" (level: Level) -> Level {
	return .Warn if level == .Allow else level
}

normalize_profile :: proc(value: string) -> string {
	key := normalized_key(value, context.temp_allocator)
	switch key {
	case PROFILE_STRICT:
		return PROFILE_STRICT
	case PROFILE_ALL:
		return PROFILE_ALL
	case PROFILE_NONE:
		return PROFILE_NONE
	}
	return PROFILE_RECOMMENDED
}

metadata_for :: proc(id: string) -> (Metadata, bool) {
	normalized := normalized_id(id, context.temp_allocator)
	for metadata in REGISTRY {
		if metadata.id == normalized {
			return metadata, true
		}
	}
	return {}, false
}

metadata_for_semantic_kind :: proc(
	kind: semantic.Checker_Diagnostic_Kind,
) -> (Metadata, bool) {
	#partial switch kind {
	case .Unresolved_Open_Sql_Source:
		return metadata_for(EPC_UNVERIFIED_OPEN_SQL_SOURCE)
	case .Invalid_Open_Sql_Into_Target:
		return metadata_for(EPC_INVALID_OPEN_SQL_INTO_TARGET)
	}
	return {}, false
}

docs_anchor :: proc(id: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	normalized := normalized_id(id, context.temp_allocator)
	for i in 0 ..< len(normalized) {
		ch := normalized[i]
		if ch == '_' {
			strings.write_byte(&out, '-')
		} else if ('a' <= ch && ch <= 'z') || ('0' <= ch && ch <= '9') || ch == '-' {
			strings.write_byte(&out, ch)
		}
	}
	return strings.to_string(out)
}

group_from_key :: proc(value: string) -> (Group, bool) {
	switch normalized_key(value, context.temp_allocator) {
	case "correctness":
		return .Correctness, true
	case "performance":
		return .Performance, true
	case "security":
		return .Security, true
	case "style":
		return .Style, true
	case "modernization":
		return .Modernization, true
	case "package":
		return .Package, true
	case "experimental":
		return .Experimental, true
	}
	return {}, false
}

level_from_key :: proc(value: string) -> (Level, bool) {
	switch normalized_key(value, context.temp_allocator) {
	case "allow":
		return .Allow, true
	case "info":
		return .Info, true
	case "warn", "warning":
		return .Warn, true
	case "deny", "error":
		return .Deny, true
	}
	return {}, false
}

group_string :: #force_inline proc "contextless" (group: Group) -> string {
	switch group {
	case .Correctness:
		return "correctness"
	case .Performance:
		return "performance"
	case .Security:
		return "security"
	case .Style:
		return "style"
	case .Modernization:
		return "modernization"
	case .Package:
		return "package"
	case .Experimental:
		return "experimental"
	}
	return "correctness"
}

origin_string :: #force_inline proc "contextless" (origin: Origin) -> string {
	switch origin {
	case .Abap_Lsp:
		return "abap-lsp"
	case .Sap_Extended_Program_Check:
		return "sap-extended-program-check"
	case .Sap_Code_Inspector:
		return "sap-code-inspector"
	case .Sap_Atc:
		return "sap-atc"
	}
	return "abap-lsp"
}

level_string :: #force_inline proc "contextless" (level: Level) -> string {
	switch level {
	case .Allow:
		return "allow"
	case .Info:
		return "info"
	case .Warn:
		return "warn"
	case .Deny:
		return "deny"
	}
	return "allow"
}

suppression_kind_string :: #force_inline proc "contextless" (kind: Suppression_Kind) -> string {
	switch kind {
	case .Pragma:
		return "pragma"
	case .Pseudo_Comment:
		return "pseudo-comment"
	case .Abap_Lsp_Allow:
		return "abap-lsp-allow"
	case .Config:
		return "config"
	}
	return "config"
}

diagnostic_from_metadata :: proc(
	metadata: Metadata,
	range: semantic.Range,
	message: string,
	file: ^semantic.Project_File,
	policy: ^Policy,
	allocator: mem.Allocator,
) -> (Diagnostic, bool) {
	level := policy_level_for(policy, metadata.id)
	is_config_suppressed := !level_enabled(level)
	if is_config_suppressed && (policy == nil || !policy.report_suppressed) {
		return {}, false
	}
	if is_config_suppressed {
		level = .Info
	}
	diagnostic := Diagnostic {
		id = strings.clone(metadata.id, allocator),
		level = level,
		severity = level_severity(level),
		group = metadata.group,
		origin = metadata.origin,
		range = range,
		message = strings.clone(message, allocator),
		file = file,
	}
	for i in 0 ..< metadata.tag_count {
		diagnostic.tags[i] = metadata.tags[i]
	}
	diagnostic.tag_count = metadata.tag_count
	for i in 0 ..< metadata.sap_alias_count {
		diagnostic.sap_aliases[i] = metadata.sap_aliases[i]
	}
	diagnostic.sap_alias_count = metadata.sap_alias_count
	if is_config_suppressed {
		mark_suppressed(
			&diagnostic,
			Suppression {
				kind = .Config,
				range = semantic.Range{start = range.start, end = range.start},
				token = "config",
			},
		)
	}
	return diagnostic, true
}

emit_diagnostic :: proc(
	out: ^Unit_Lints,
	metadata: Metadata,
	range: semantic.Range,
	message: string,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if diagnostic, ok := diagnostic_from_metadata(metadata, range, message, out.file, policy, allocator); ok {
		append(&out.diagnostics, diagnostic)
	}
}

mark_suppressed :: proc(diagnostic: ^Diagnostic, suppression: Suppression) {
	if diagnostic == nil {
		return
	}
	diagnostic.suppressed = true
	diagnostic.suppression = suppression
	diagnostic.has_suppression = true
	diagnostic.level = .Info
	diagnostic.severity = .Information
	for i in 0 ..< diagnostic.tag_count {
		if diagnostic.tags[i] == "suppressed" {
			return
		}
	}
	if diagnostic.tag_count < len(diagnostic.tags) {
		diagnostic.tags[diagnostic.tag_count] = "suppressed"
		diagnostic.tag_count += 1
	}
}

level_severity :: #force_inline proc "contextless" (level: Level) -> Diagnostic_Severity {
	switch level {
	case .Deny:
		return .Error
	case .Warn:
		return .Warning
	case .Info:
		return .Information
	case .Allow:
		return .Hint
	}
	return .Warning
}

normalized_key :: proc(value: string, allocator: mem.Allocator) -> string {
	return utils.to_lower_ascii(strings.trim_space(value), allocator)
}

normalized_id :: proc(value: string, allocator: mem.Allocator) -> string {
	return normalized_key(value, allocator)
}

normalized_alias :: proc(value: string, allocator: mem.Allocator) -> string {
	return normalized_key(value, allocator)
}

id_is_native_namespace :: proc(id: string) -> bool {
	normalized := normalized_id(id, context.temp_allocator)
	return strings.has_prefix(normalized, "abap-lsp.") || strings.has_prefix(normalized, "epc.")
}

id_is_external_provider :: proc(id: string) -> bool {
	normalized := normalized_id(id, context.temp_allocator)
	if strings.has_prefix(normalized, "abap-lsp.") || strings.has_prefix(normalized, "epc.") {
		return false
	}
	for i in 0 ..< len(normalized) {
		if normalized[i] == ':' {
			return i > 0 && i + 1 < len(normalized)
		}
	}
	return false
}
