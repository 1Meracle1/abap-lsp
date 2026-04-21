#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTypeKind {
    Type,
    Constant,
    Variable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinFieldSpec {
    pub name: &'static str,
    pub description: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinStructureSpec {
    pub name: &'static str,
    pub fields: &'static [BuiltinFieldSpec],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinSymbolSpec {
    pub name: &'static str,
    pub kind: BuiltinTypeKind,
    pub structure_name: Option<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinRoutineParamSpec {
    pub name: &'static str,
    pub type_name: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinRoutineSpec {
    pub name: &'static str,
    pub params: &'static [BuiltinRoutineParamSpec],
    pub hover_params: &'static [&'static str],
    pub return_type: &'static str,
    pub description: &'static str,
    /// When false, named parameter passing (`name = value`) is reported as invalid (ABAP style for most built-ins).
    pub supports_named_arguments: bool,
}

pub const SYST_FIELDS: &[BuiltinFieldSpec] = &[
    BuiltinFieldSpec {
        name: "abcde",
        description: "Latin alphabet helper text that can be indexed directly by offset and length.",
    },
    BuiltinFieldSpec {
        name: "batch",
        description: "Set to 'X' in background processing and initial in dialog processing.",
    },
    BuiltinFieldSpec {
        name: "binpt",
        description: "Set to 'X' while batch input is being processed.",
    },
    BuiltinFieldSpec {
        name: "calld",
        description: "Call-sequence marker: initial in the first program of a sequence and 'X' in called programs.",
    },
    BuiltinFieldSpec {
        name: "callr",
        description: "Spool origin marker that identifies where list spooling was started.",
    },
    BuiltinFieldSpec {
        name: "colno",
        description: "Current list-buffer column while a list is being created.",
    },
    BuiltinFieldSpec {
        name: "cpage",
        description: "Top page number of the displayed list when a list event was raised.",
    },
    BuiltinFieldSpec {
        name: "cprog",
        description: "Calling program for external procedures, otherwise the current program.",
    },
    BuiltinFieldSpec {
        name: "cucol",
        description: "Horizontal dynpro cursor position after PAI.",
    },
    BuiltinFieldSpec {
        name: "curow",
        description: "Vertical dynpro cursor position after PAI.",
    },
    BuiltinFieldSpec {
        name: "datar",
        description: "Set to 'X' in PAI when at least one screen input field was changed.",
    },
    BuiltinFieldSpec {
        name: "datlo",
        description: "Current user date in the user's time zone.",
    },
    BuiltinFieldSpec {
        name: "datum",
        description: "Current system date.",
    },
    BuiltinFieldSpec {
        name: "dayst",
        description: "Set to 'X' during daylight saving time in the system time zone.",
    },
    BuiltinFieldSpec {
        name: "dbcnt",
        description: "Number of database rows processed by the last SQL statement that documents it.",
    },
    BuiltinFieldSpec {
        name: "dbnam",
        description: "Logical database name of the current executable program, when one is linked.",
    },
    BuiltinFieldSpec {
        name: "dbsys",
        description: "Database system identifier of the standard database, such as HDB.",
    },
    BuiltinFieldSpec {
        name: "dyngr",
        description: "Screen group of the current dynpro.",
    },
    BuiltinFieldSpec {
        name: "dynnr",
        description: "Current dynpro number, including selection screens and subscreens.",
    },
    BuiltinFieldSpec {
        name: "fdayw",
        description: "Factory-calendar weekday in the system time zone, with Monday as 1.",
    },
    BuiltinFieldSpec {
        name: "fdpos",
        description: "Found offset after supported search and comparison operations such as FIND.",
    },
    BuiltinFieldSpec {
        name: "host",
        description: "Host name of the current application server instance.",
    },
    BuiltinFieldSpec {
        name: "index",
        description: "Loop counter inside DO and WHILE loops; nested loops use the innermost counter.",
    },
    BuiltinFieldSpec {
        name: "langu",
        description: "Single-character locale language key for the current internal session.",
    },
    BuiltinFieldSpec {
        name: "ldbpg",
        description: "Database program name of the linked logical database, when applicable.",
    },
    BuiltinFieldSpec {
        name: "lilli",
        description: "List line on which a list event was raised, including the page header.",
    },
    BuiltinFieldSpec {
        name: "linct",
        description: "Configured page length of the current list during list creation.",
    },
    BuiltinFieldSpec {
        name: "linno",
        description: "Current list line while a list is being created, including the page header.",
    },
    BuiltinFieldSpec {
        name: "linsz",
        description: "List-buffer line width of the current list during list creation.",
    },
    BuiltinFieldSpec {
        name: "lisel",
        description: "Displayed list line content for the list event cursor position, truncated to 255 characters.",
    },
    BuiltinFieldSpec {
        name: "listi",
        description: "List level of the list for which a list event was raised.",
    },
    BuiltinFieldSpec {
        name: "loopc",
        description: "Number of currently displayed rows in a table control after PAI.",
    },
    BuiltinFieldSpec {
        name: "lsind",
        description: "List level of the list currently being created.",
    },
    BuiltinFieldSpec {
        name: "macol",
        description: "Left spool margin set by SET MARGIN.",
    },
    BuiltinFieldSpec {
        name: "mandt",
        description: "Client of the current user session.",
    },
    BuiltinFieldSpec {
        name: "marow",
        description: "Top spool margin set by SET MARGIN.",
    },
    BuiltinFieldSpec {
        name: "modno",
        description: "Index of the current ABAP session within the SAP GUI session.",
    },
    BuiltinFieldSpec {
        name: "msgid",
        description: "Message class captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "msgno",
        description: "Message number captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "msgty",
        description: "Message type captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "msgv1",
        description: "First MESSAGE placeholder value captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "msgv2",
        description: "Second MESSAGE placeholder value captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "msgv3",
        description: "Third MESSAGE placeholder value captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "msgv4",
        description: "Fourth MESSAGE placeholder value captured by the last MESSAGE statement.",
    },
    BuiltinFieldSpec {
        name: "opsys",
        description: "Operating system identifier of the current application server.",
    },
    BuiltinFieldSpec {
        name: "pagno",
        description: "Current page number while a list is being created.",
    },
    BuiltinFieldSpec {
        name: "pfkey",
        description: "Current dynpro GUI status after PAI.",
    },
    BuiltinFieldSpec {
        name: "repid",
        description: "Program name exposed through sy-repid and syst-repid; SAP documents this as a predefined constant and type, not a real SYST component.",
    },
    BuiltinFieldSpec {
        name: "saprl",
        description: "ABAP release identifier of the current system.",
    },
    BuiltinFieldSpec {
        name: "scols",
        description: "Number of columns in the current screen layout after PAI.",
    },
    BuiltinFieldSpec {
        name: "slset",
        description: "Selection-screen variant name used to fill the current selection screen.",
    },
    BuiltinFieldSpec {
        name: "spono",
        description: "Spool request number while list spooling is active.",
    },
    BuiltinFieldSpec {
        name: "srows",
        description: "Number of rows in the current screen layout after PAI.",
    },
    BuiltinFieldSpec {
        name: "staco",
        description: "First displayed list column when a list event was raised.",
    },
    BuiltinFieldSpec {
        name: "staro",
        description: "Top displayed list line of the displayed page when a list event was raised.",
    },
    BuiltinFieldSpec {
        name: "stepl",
        description: "Current row index of a table control during loop processing.",
    },
    BuiltinFieldSpec {
        name: "subrc",
        description: "Return code set by many ABAP statements; 0 usually indicates success for the documented statement.",
    },
    BuiltinFieldSpec {
        name: "sysid",
        description: "System ID of the current ABAP system.",
    },
    BuiltinFieldSpec {
        name: "tabix",
        description: "Current internal-table index from READ TABLE or LOOP AT on indexed access paths.",
    },
    BuiltinFieldSpec {
        name: "tcode",
        description: "Current transaction code, if one is active.",
    },
    BuiltinFieldSpec {
        name: "tfill",
        description: "Row count of the internal table accessed by DESCRIBE TABLE, LOOP AT, or READ TABLE.",
    },
    BuiltinFieldSpec {
        name: "timlo",
        description: "Current user time in the user's time zone.",
    },
    BuiltinFieldSpec {
        name: "title",
        description: "Current dynpro title-bar text.",
    },
    BuiltinFieldSpec {
        name: "tleng",
        description: "Row length in bytes of the internal table accessed by DESCRIBE TABLE, LOOP AT, or READ TABLE.",
    },
    BuiltinFieldSpec {
        name: "tvar0",
        description: "TOP-OF-PAGE replacement variable for placeholder &0 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar1",
        description: "TOP-OF-PAGE replacement variable for placeholder &1 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar2",
        description: "TOP-OF-PAGE replacement variable for placeholder &2 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar3",
        description: "TOP-OF-PAGE replacement variable for placeholder &3 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar4",
        description: "TOP-OF-PAGE replacement variable for placeholder &4 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar5",
        description: "TOP-OF-PAGE replacement variable for placeholder &5 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar6",
        description: "TOP-OF-PAGE replacement variable for placeholder &6 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar7",
        description: "TOP-OF-PAGE replacement variable for placeholder &7 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar8",
        description: "TOP-OF-PAGE replacement variable for placeholder &8 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tvar9",
        description: "TOP-OF-PAGE replacement variable for placeholder &9 in list text elements.",
    },
    BuiltinFieldSpec {
        name: "tzone",
        description: "System time-zone offset from UTC in seconds.",
    },
    BuiltinFieldSpec {
        name: "ucomm",
        description: "Function code that triggered the current PAI processing.",
    },
    BuiltinFieldSpec {
        name: "uline",
        description: "List helper value containing 255 horizontal line characters.",
    },
    BuiltinFieldSpec {
        name: "uname",
        description: "User name of the current session.",
    },
    BuiltinFieldSpec {
        name: "uzeit",
        description: "Current system time.",
    },
    BuiltinFieldSpec {
        name: "vline",
        description: "List helper value containing a vertical line character.",
    },
    BuiltinFieldSpec {
        name: "wtitl",
        description: "Set to 'N' when NO STANDARD PAGE HEADING was specified in REPORT-like declarations.",
    },
    BuiltinFieldSpec {
        name: "xform",
        description: "ABAP System Field: Internal Use",
    },
    BuiltinFieldSpec {
        name: "zonlo",
        description: "User time zone identifier.",
    },
];

pub const MATCH_RESULT_FIELDS: &[BuiltinFieldSpec] = &[
    BuiltinFieldSpec {
        name: "offset",
        description: "Zero-based offset of the match in the searched data object.",
    },
    BuiltinFieldSpec {
        name: "length",
        description: "Length of the matched segment.",
    },
    BuiltinFieldSpec {
        name: "submatches",
        description: "Nested table containing captured submatches for a regex result.",
    },
    BuiltinFieldSpec {
        name: "line",
        description: "Line number of the match for searches in internal tables.",
    },
];

pub const SCREEN_FIELDS: &[BuiltinFieldSpec] = &[
    BuiltinFieldSpec {
        name: "name",
        description: "Name of the current dynpro field or screen element.",
    },
    BuiltinFieldSpec {
        name: "group1",
        description: "Modification group 1 of the current screen element.",
    },
    BuiltinFieldSpec {
        name: "group2",
        description: "Modification group 2 of the current screen element.",
    },
    BuiltinFieldSpec {
        name: "group3",
        description: "Modification group 3 of the current screen element.",
    },
    BuiltinFieldSpec {
        name: "group4",
        description: "Modification group 4 of the current screen element.",
    },
    BuiltinFieldSpec {
        name: "required",
        description: "Whether the field is mandatory on the current dynpro.",
    },
    BuiltinFieldSpec {
        name: "input",
        description: "Whether the field is ready for input on the current dynpro.",
    },
    BuiltinFieldSpec {
        name: "output",
        description: "Whether the field is output-only on the current dynpro.",
    },
    BuiltinFieldSpec {
        name: "intensified",
        description: "Whether the field is highlighted on the current dynpro.",
    },
    BuiltinFieldSpec {
        name: "invisible",
        description: "Whether the field is hidden on the current dynpro.",
    },
    BuiltinFieldSpec {
        name: "length",
        description: "Visible field length of the current dynpro element.",
    },
    BuiltinFieldSpec {
        name: "active",
        description: "Combined active flag for the current dynpro element.",
    },
    BuiltinFieldSpec {
        name: "display_3d",
        description: "Whether the current dynpro box is shown three-dimensionally.",
    },
    BuiltinFieldSpec {
        name: "value_help",
        description: "Whether input help is shown for the current dynpro field.",
    },
    BuiltinFieldSpec {
        name: "request",
        description: "Whether input exists, or is simulated, for the current dynpro field.",
    },
    BuiltinFieldSpec {
        name: "values_in_combo",
        description: "Whether values exist in the current dynpro dropdown list box.",
    },
];

pub const BAPIRET2_FIELDS: &[BuiltinFieldSpec] = &[
    BuiltinFieldSpec {
        name: "type",
        description: "Message type of the BAPI return entry (for example S, E, W, I, or A).",
    },
    BuiltinFieldSpec {
        name: "id",
        description: "Message class of the BAPI return entry.",
    },
    BuiltinFieldSpec {
        name: "number",
        description: "Message number of the BAPI return entry.",
    },
    BuiltinFieldSpec {
        name: "message",
        description: "Resolved long text/message text carried by the BAPI return entry.",
    },
    BuiltinFieldSpec {
        name: "log_no",
        description: "Application log number associated with the BAPI return entry.",
    },
    BuiltinFieldSpec {
        name: "log_msg_no",
        description: "Application log message number associated with the BAPI return entry.",
    },
    BuiltinFieldSpec {
        name: "message_v1",
        description: "First placeholder variable of the BAPI return message.",
    },
    BuiltinFieldSpec {
        name: "message_v2",
        description: "Second placeholder variable of the BAPI return message.",
    },
    BuiltinFieldSpec {
        name: "message_v3",
        description: "Third placeholder variable of the BAPI return message.",
    },
    BuiltinFieldSpec {
        name: "message_v4",
        description: "Fourth placeholder variable of the BAPI return message.",
    },
    BuiltinFieldSpec {
        name: "parameter",
        description: "Name of the BAPI parameter related to the return entry, when present.",
    },
    BuiltinFieldSpec {
        name: "row",
        description: "Row number related to the BAPI return entry, when present.",
    },
    BuiltinFieldSpec {
        name: "field",
        description: "Field name related to the BAPI return entry, when present.",
    },
    BuiltinFieldSpec {
        name: "system",
        description: "Logical system related to the BAPI return entry, when present.",
    },
];

pub const BUILTIN_STRUCTURES: &[BuiltinStructureSpec] = &[
    BuiltinStructureSpec {
        name: "syst",
        fields: SYST_FIELDS,
    },
    BuiltinStructureSpec {
        name: "screen",
        fields: SCREEN_FIELDS,
    },
    BuiltinStructureSpec {
        name: "match_result",
        fields: MATCH_RESULT_FIELDS,
    },
];

pub const BUILTIN_SYMBOLS: &[BuiltinSymbolSpec] = &[
    BuiltinSymbolSpec {
        name: "abap_bool",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "flag",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "xfeld",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "sy",
        kind: BuiltinTypeKind::Type,
        structure_name: Some("syst"),
    },
    BuiltinSymbolSpec {
        name: "syst",
        kind: BuiltinTypeKind::Type,
        structure_name: Some("syst"),
    },
    BuiltinSymbolSpec {
        name: "screen",
        kind: BuiltinTypeKind::Type,
        structure_name: Some("screen"),
    },
    BuiltinSymbolSpec {
        name: "syst",
        kind: BuiltinTypeKind::Variable,
        structure_name: Some("syst"),
    },
    BuiltinSymbolSpec {
        name: "sy",
        kind: BuiltinTypeKind::Variable,
        structure_name: Some("syst"),
    },
    BuiltinSymbolSpec {
        name: "screen",
        kind: BuiltinTypeKind::Variable,
        structure_name: Some("screen"),
    },
    BuiltinSymbolSpec {
        name: "guid",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "symsgv",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "sydatum",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "timestamp",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "cursor",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "match_result",
        kind: BuiltinTypeKind::Type,
        structure_name: Some("match_result"),
    },
    BuiltinSymbolSpec {
        name: "match_result_tab",
        kind: BuiltinTypeKind::Type,
        structure_name: Some("match_result"),
    },
    BuiltinSymbolSpec {
        name: "tabname",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "cdobjectcl",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "rs38l_fnam",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "memoryid",
        kind: BuiltinTypeKind::Type,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "abap_true",
        kind: BuiltinTypeKind::Constant,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "abap_false",
        kind: BuiltinTypeKind::Constant,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "abap_undefined",
        kind: BuiltinTypeKind::Constant,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "space",
        kind: BuiltinTypeKind::Constant,
        structure_name: None,
    },
    BuiltinSymbolSpec {
        name: "text",
        kind: BuiltinTypeKind::Variable,
        structure_name: None,
    },
];

const ARG_STRING_PARAMS: &[BuiltinRoutineParamSpec] = &[
    BuiltinRoutineParamSpec {
        name: "arg",
        type_name: "string",
    },
    BuiltinRoutineParamSpec {
        name: "val",
        type_name: "string",
    },
];

const ARG_XSTRING_PARAMS: &[BuiltinRoutineParamSpec] = &[
    BuiltinRoutineParamSpec {
        name: "arg",
        type_name: "xstring",
    },
    BuiltinRoutineParamSpec {
        name: "val",
        type_name: "xstring",
    },
];

const ARG_DATA_PARAMS: &[BuiltinRoutineParamSpec] = &[
    BuiltinRoutineParamSpec {
        name: "arg",
        type_name: "data",
    },
    BuiltinRoutineParamSpec {
        name: "val",
        type_name: "data",
    },
];

pub const BUILTIN_ROUTINES: &[BuiltinRoutineSpec] = &[
    BuiltinRoutineSpec {
        name: "line_exists",
        params: &[BuiltinRoutineParamSpec {
            name: "table_line",
            type_name: "data",
        }],
        hover_params: &["table_line"],
        return_type: "abap_bool",
        description: "Predicate function: returns whether a row exists for the given internal table expression.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "charlen",
        params: &[
            BuiltinRoutineParamSpec {
                name: "arg",
                type_name: "string",
            },
            BuiltinRoutineParamSpec {
                name: "text",
                type_name: "string",
            },
        ],
        hover_params: &["arg"],
        return_type: "i",
        description: "Length of the first character in the current code page.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "dbmaxlen",
        params: ARG_STRING_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Maximum ABAP Dictionary length for a string-like value.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "numofchar",
        params: &[
            BuiltinRoutineParamSpec {
                name: "arg",
                type_name: "string",
            },
            BuiltinRoutineParamSpec {
                name: "str",
                type_name: "string",
            },
        ],
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of characters in a text value.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "strlen",
        params: ARG_STRING_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of characters in a text value.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "substring",
        params: &[
            BuiltinRoutineParamSpec {
                name: "val",
                type_name: "string",
            },
            BuiltinRoutineParamSpec {
                name: "off",
                type_name: "i",
            },
            BuiltinRoutineParamSpec {
                name: "len",
                type_name: "i",
            },
        ],
        hover_params: &["val", "off", "len"],
        return_type: "string",
        description: "Returns a substring of a text-like value; optional `off` selects the start position and optional `len` limits the length (if `len` is omitted, the remainder is returned).",
        supports_named_arguments: true,
    },
    BuiltinRoutineSpec {
        name: "condense",
        params: &[
            BuiltinRoutineParamSpec {
                name: "val",
                type_name: "string",
            },
            BuiltinRoutineParamSpec {
                name: "del",
                type_name: "string",
            },
            BuiltinRoutineParamSpec {
                name: "from",
                type_name: "string",
            },
            BuiltinRoutineParamSpec {
                name: "to",
                type_name: "string",
            },
        ],
        hover_params: &["val", "del", "from", "to"],
        return_type: "string",
        description: "Returns a condensed character string: strips leading/trailing characters in `del`, replaces runs in `from` using `to` (all default to a single blank when omitted).",
        supports_named_arguments: true,
    },
    BuiltinRoutineSpec {
        name: "round",
        params: &[
            BuiltinRoutineParamSpec {
                name: "val",
                type_name: "decfloat34",
            },
            BuiltinRoutineParamSpec {
                name: "dec",
                type_name: "i",
            },
            BuiltinRoutineParamSpec {
                name: "prec",
                type_name: "i",
            },
            BuiltinRoutineParamSpec {
                name: "mode",
                type_name: "data",
            },
        ],
        hover_params: &["val", "dec", "prec", "mode"],
        return_type: "decfloat34",
        description: "Rounds a decimal floating-point value to a given number of decimal places (`dec`) or significant digits (`prec`), optionally using a rounding mode from `CL_ABAP_MATH`.",
        supports_named_arguments: true,
    },
    BuiltinRoutineSpec {
        name: "to_lower",
        params: ARG_STRING_PARAMS,
        hover_params: &["arg"],
        return_type: "string",
        description: "Returns a text value converted to lowercase.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "xstrlen",
        params: ARG_XSTRING_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of bytes in a byte string value.",
        supports_named_arguments: false,
    },
    BuiltinRoutineSpec {
        name: "lines",
        params: ARG_DATA_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of rows in an internal table value.",
        supports_named_arguments: false,
    },
];

pub fn builtin_routine_spec(name: &str) -> Option<&'static BuiltinRoutineSpec> {
    BUILTIN_ROUTINES
        .iter()
        .find(|spec| spec.name.eq_ignore_ascii_case(name))
}

/// Documentation line for a field of a built-in structure (for example `syst` / `sy-...`).
pub fn builtin_structure_field_description(
    structure_name: &str,
    field_name: &str,
) -> Option<&'static str> {
    let structure = BUILTIN_STRUCTURES
        .iter()
        .find(|spec| spec.name.eq_ignore_ascii_case(structure_name))?;
    let field = structure
        .fields
        .iter()
        .find(|f| f.name.eq_ignore_ascii_case(field_name))?;
    Some(field.description)
}

fn well_known_bapiret2_field_type(
    field_name: &str,
) -> Option<(&'static str, Option<&'static str>)> {
    if field_name.eq_ignore_ascii_case("type") {
        return Some(("char1", None));
    }
    if field_name.eq_ignore_ascii_case("id") {
        return Some(("c", None));
    }
    if field_name.eq_ignore_ascii_case("number") {
        return Some(("n", None));
    }
    if field_name.eq_ignore_ascii_case("message") {
        return Some(("string", None));
    }
    if field_name.eq_ignore_ascii_case("log_no") {
        return Some(("string", None));
    }
    if field_name.eq_ignore_ascii_case("log_msg_no") {
        return Some(("n", None));
    }
    if field_name.eq_ignore_ascii_case("message_v1")
        || field_name.eq_ignore_ascii_case("message_v2")
        || field_name.eq_ignore_ascii_case("message_v3")
        || field_name.eq_ignore_ascii_case("message_v4")
    {
        return Some(("c", None));
    }
    if field_name.eq_ignore_ascii_case("parameter")
        || field_name.eq_ignore_ascii_case("field")
        || field_name.eq_ignore_ascii_case("system")
    {
        return Some(("string", None));
    }
    if field_name.eq_ignore_ascii_case("row") {
        return Some(("i", None));
    }
    None
}

fn builtin_syst_field_type(field_name: &str) -> Option<(&'static str, Option<&'static str>)> {
    if field_name.eq_ignore_ascii_case("xform") {
        return Some(("char30", None));
    }
    let type_name = match field_name.to_ascii_lowercase().as_str() {
        "abcde" | "batch" | "binpt" | "calld" | "callr" | "cprog" | "datar" | "dayst" | "dbnam"
        | "dbsys" | "dyngr" | "dynnr" | "host" | "langu" | "ldbpg" | "lisel" | "opsys"
        | "pfkey" | "repid" | "saprl" | "slset" | "sysid" | "tcode" | "title" | "ucomm"
        | "uline" | "uname" | "vline" | "wtitl" | "zonlo" => "c",
        "colno" | "cpage" | "cucol" | "curow" | "dbcnt" | "fdpos" | "index" | "lilli" | "linct"
        | "linno" | "linsz" | "listi" | "loopc" | "lsind" | "macol" | "marow" | "modno"
        | "pagno" | "scols" | "srows" | "staco" | "staro" | "stepl" | "subrc" | "tabix"
        | "tfill" | "tleng" | "tzone" => "i",
        "datlo" | "datum" => "d",
        "fdayw" => "b",
        "msgno" | "spono" => "n",
        "timlo" | "uzeit" => "t",
        "mandt" | "msgid" | "msgty" | "msgv1" | "msgv2" | "msgv3" | "msgv4" | "tvar0" | "tvar1"
        | "tvar2" | "tvar3" | "tvar4" | "tvar5" | "tvar6" | "tvar7" | "tvar8" | "tvar9" => "c",
        _ => return None,
    };
    Some((type_name, None))
}

pub fn well_known_external_structure_field_description(
    structure_name: &str,
    field_name: &str,
) -> Option<&'static str> {
    if structure_name.eq_ignore_ascii_case("bapiret2") {
        return BAPIRET2_FIELDS
            .iter()
            .find(|field| field.name.eq_ignore_ascii_case(field_name))
            .map(|field| field.description);
    }
    None
}

pub fn well_known_external_structure_field_type(
    structure_name: &str,
    field_name: &str,
) -> Option<(&'static str, Option<&'static str>)> {
    if structure_name.eq_ignore_ascii_case("bapiret2") {
        return well_known_bapiret2_field_type(field_name);
    }
    None
}

pub fn builtin_structure_field_type(
    structure_name: &str,
    field_name: &str,
) -> Option<(&'static str, Option<&'static str>)> {
    if structure_name.eq_ignore_ascii_case("syst") {
        return builtin_syst_field_type(field_name);
    }
    if structure_name.eq_ignore_ascii_case("screen") {
        if matches!(
            field_name.to_ascii_lowercase().as_str(),
            "name"
                | "group1"
                | "group2"
                | "group3"
                | "group4"
                | "required"
                | "input"
                | "output"
                | "intensified"
                | "invisible"
                | "active"
                | "display_3d"
                | "value_help"
                | "request"
                | "values_in_combo"
        ) {
            return Some(("c", None));
        }
        if field_name.eq_ignore_ascii_case("length") {
            return Some(("x", None));
        }
    }
    if structure_name.eq_ignore_ascii_case("match_result") {
        if field_name.eq_ignore_ascii_case("offset")
            || field_name.eq_ignore_ascii_case("length")
            || field_name.eq_ignore_ascii_case("line")
        {
            return Some(("i", None));
        }
        if field_name.eq_ignore_ascii_case("submatches") {
            return Some(("match_result_tab", Some("match_result")));
        }
    }
    None
}
