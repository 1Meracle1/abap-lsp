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
