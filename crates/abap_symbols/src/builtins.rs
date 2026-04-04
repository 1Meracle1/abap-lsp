#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTypeKind {
    Type,
    Constant,
    Variable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinFieldSpec {
    pub name: &'static str,
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
}

pub const SYST_FIELDS: &[BuiltinFieldSpec] = &[
    BuiltinFieldSpec { name: "subrc" },
    BuiltinFieldSpec { name: "tabix" },
    BuiltinFieldSpec { name: "index" },
    BuiltinFieldSpec { name: "tfill" },
    BuiltinFieldSpec { name: "tleng" },
    BuiltinFieldSpec { name: "dbcnt" },
    BuiltinFieldSpec { name: "datum" },
    BuiltinFieldSpec { name: "uzeit" },
    BuiltinFieldSpec { name: "mandt" },
    BuiltinFieldSpec { name: "uname" },
    BuiltinFieldSpec { name: "langu" },
    BuiltinFieldSpec { name: "batch" },
    BuiltinFieldSpec { name: "cprog" },
    BuiltinFieldSpec { name: "repid" },
    BuiltinFieldSpec { name: "tcode" },
];

pub const BUILTIN_STRUCTURES: &[BuiltinStructureSpec] = &[BuiltinStructureSpec {
    name: "syst",
    fields: SYST_FIELDS,
}];

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
        name: "sy",
        kind: BuiltinTypeKind::Variable,
        structure_name: Some("syst"),
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
    },
    BuiltinRoutineSpec {
        name: "dbmaxlen",
        params: ARG_STRING_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Maximum ABAP Dictionary length for a string-like value.",
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
    },
    BuiltinRoutineSpec {
        name: "strlen",
        params: ARG_STRING_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of characters in a text value.",
    },
    BuiltinRoutineSpec {
        name: "to_lower",
        params: ARG_STRING_PARAMS,
        hover_params: &["arg"],
        return_type: "string",
        description: "Returns a text value converted to lowercase.",
    },
    BuiltinRoutineSpec {
        name: "xstrlen",
        params: ARG_XSTRING_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of bytes in a byte string value.",
    },
    BuiltinRoutineSpec {
        name: "lines",
        params: ARG_DATA_PARAMS,
        hover_params: &["arg"],
        return_type: "i",
        description: "Number of rows in an internal table value.",
    },
];

pub fn builtin_routine_spec(name: &str) -> Option<&'static BuiltinRoutineSpec> {
    BUILTIN_ROUTINES
        .iter()
        .find(|spec| spec.name.eq_ignore_ascii_case(name))
}
