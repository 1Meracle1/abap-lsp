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
        name: "subrc",
        description: "Return code from the last ABAP statement; 0 usually means success.",
    },
    BuiltinFieldSpec {
        name: "tabix",
        description: "Index of the current internal table row in LOOP AT or READ TABLE over the table body.",
    },
    BuiltinFieldSpec {
        name: "index",
        description: "Loop counter in DO … ENDDO and similar loop constructs.",
    },
    BuiltinFieldSpec {
        name: "tfill",
        description: "Number of lines in the internal table after statements that set this system field.",
    },
    BuiltinFieldSpec {
        name: "tleng",
        description: "Row length (bytes) of an internal table in contexts where this field is defined.",
    },
    BuiltinFieldSpec {
        name: "dbcnt",
        description: "Number of rows processed by the last Open SQL statement.",
    },
    BuiltinFieldSpec {
        name: "datum",
        description: "Current date on the application server at runtime.",
    },
    BuiltinFieldSpec {
        name: "uzeit",
        description: "Current time on the application server at runtime.",
    },
    BuiltinFieldSpec {
        name: "zonlo",
        description: "Time zone of the current user for local date/time (used with local conversion).",
    },
    BuiltinFieldSpec {
        name: "datlo",
        description: "Current date in the user's local time zone.",
    },
    BuiltinFieldSpec {
        name: "timlo",
        description: "Current time in the user's local time zone.",
    },
    BuiltinFieldSpec {
        name: "mandt",
        description: "Client (mandant) of the current SAP session.",
    },
    BuiltinFieldSpec {
        name: "uname",
        description: "Logon name of the current user.",
    },
    BuiltinFieldSpec {
        name: "langu",
        description: "Current logon language key.",
    },
    BuiltinFieldSpec {
        name: "batch",
        description: "Background processing: space in dialog, 'X' when running in batch.",
    },
    BuiltinFieldSpec {
        name: "cprog",
        description: "Name of the calling program in the current call chain.",
    },
    BuiltinFieldSpec {
        name: "repid",
        description: "Name of the current ABAP program.",
    },
    BuiltinFieldSpec {
        name: "tcode",
        description: "Transaction code of the current transaction.",
    },
    BuiltinFieldSpec {
        name: "ucomm",
        description: "Function code from the last user action (GUI status / function code).",
    },
    BuiltinFieldSpec {
        name: "srows",
        description: "Number of lines on the current list screen.",
    },
    BuiltinFieldSpec {
        name: "msgid",
        description: "Message class of the last message raised with MESSAGE.",
    },
    BuiltinFieldSpec {
        name: "msgty",
        description: "Message type of the last message (E, W, I, S, A, X, …).",
    },
    BuiltinFieldSpec {
        name: "msgno",
        description: "Message number of the last message.",
    },
    BuiltinFieldSpec {
        name: "msgv1",
        description: "First placeholder variable for the text of the last MESSAGE.",
    },
    BuiltinFieldSpec {
        name: "msgv2",
        description: "Second placeholder variable for the text of the last MESSAGE.",
    },
    BuiltinFieldSpec {
        name: "msgv3",
        description: "Third placeholder variable for the text of the last MESSAGE.",
    },
    BuiltinFieldSpec {
        name: "msgv4",
        description: "Fourth placeholder variable for the text of the last MESSAGE.",
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

/// Documentation line for a field of a built-in structure (for example `syst` / `sy-…`).
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
        return Some(("symsgid", None));
    }
    if field_name.eq_ignore_ascii_case("number") {
        return Some(("symsgno", None));
    }
    if field_name.eq_ignore_ascii_case("message") {
        return Some(("string", None));
    }
    if field_name.eq_ignore_ascii_case("log_no") {
        return Some(("string", None));
    }
    if field_name.eq_ignore_ascii_case("log_msg_no") {
        return Some(("symsgno", None));
    }
    if field_name.eq_ignore_ascii_case("message_v1")
        || field_name.eq_ignore_ascii_case("message_v2")
        || field_name.eq_ignore_ascii_case("message_v3")
        || field_name.eq_ignore_ascii_case("message_v4")
    {
        return Some(("symsgv", None));
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
