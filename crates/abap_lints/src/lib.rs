use std::collections::BTreeMap;
use std::ops::Range;

use serde::Deserialize;
use serde::de::{self, Visitor};

pub type LintId = &'static str;
pub type LintRange = Range<usize>;

pub const LINT_PROFILE_RECOMMENDED: &str = "recommended";
pub const LINT_PROFILE_STRICT: &str = "strict";
pub const LINT_PROFILE_ALL: &str = "all";
pub const LINT_PROFILE_NONE: &str = "none";

pub const ABAP_LSP_UNREACHABLE_CODE: LintId = "abap-lsp.unreachable-code";
pub const ABAP_LSP_USE_BEFORE_DEFINITE_ASSIGNMENT: LintId =
    "abap-lsp.use-before-definite-assignment";
pub const ABAP_LSP_POSSIBLY_UNBOUND_FIELD_SYMBOL: LintId = "abap-lsp.possibly-unbound-field-symbol";
pub const ABAP_LSP_DEAD_STORE: LintId = "abap-lsp.dead-store";
pub const ABAP_LSP_UNSORTED_READ_TABLE_BINARY_SEARCH: LintId =
    "abap-lsp.unsorted-read-table-binary-search";
pub const EPC_UNVERIFIED_OPEN_SQL_SOURCE: LintId = "epc.unverified-open-sql-source";
pub const EPC_INVALID_OPEN_SQL_INTO_TARGET: LintId = "epc.invalid-open-sql-into-target";
pub const EPC_MISSING_TABLES_DECLARATION: LintId = "epc.missing-tables-declaration";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LintGroup {
    Correctness,
    Performance,
    Security,
    Style,
    Modernization,
    Package,
    Experimental,
}

impl LintGroup {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Correctness => "correctness",
            Self::Performance => "performance",
            Self::Security => "security",
            Self::Style => "style",
            Self::Modernization => "modernization",
            Self::Package => "package",
            Self::Experimental => "experimental",
        }
    }

    pub fn from_key(value: &str) -> Option<Self> {
        match normalized_key(value).as_str() {
            "correctness" => Some(Self::Correctness),
            "performance" => Some(Self::Performance),
            "security" => Some(Self::Security),
            "style" => Some(Self::Style),
            "modernization" => Some(Self::Modernization),
            "package" => Some(Self::Package),
            "experimental" => Some(Self::Experimental),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintOrigin {
    AbapLsp,
    SapExtendedProgramCheck,
    SapCodeInspector,
    SapAtc,
}

impl LintOrigin {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::AbapLsp => "abap-lsp",
            Self::SapExtendedProgramCheck => "sap-extended-program-check",
            Self::SapCodeInspector => "sap-code-inspector",
            Self::SapAtc => "sap-atc",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LintLevel {
    Allow,
    Info,
    Warn,
    Deny,
}

impl LintLevel {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Info => "info",
            Self::Warn => "warn",
            Self::Deny => "deny",
        }
    }

    pub const fn is_enabled(self) -> bool {
        !matches!(self, Self::Allow)
    }

    pub fn from_key(value: &str) -> Option<Self> {
        match normalized_key(value).as_str() {
            "allow" => Some(Self::Allow),
            "info" => Some(Self::Info),
            "warn" | "warning" => Some(Self::Warn),
            "deny" | "error" => Some(Self::Deny),
            _ => None,
        }
    }
}

impl<'de> Deserialize<'de> for LintLevel {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct LintLevelVisitor;

        impl Visitor<'_> for LintLevelVisitor {
            type Value = LintLevel;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("one of: allow, info, warn, deny")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                LintLevel::from_key(value)
                    .ok_or_else(|| E::unknown_variant(value, &["allow", "info", "warn", "deny"]))
            }
        }

        deserializer.deserialize_str(LintLevelVisitor)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LintMetadata {
    pub id: LintId,
    pub group: LintGroup,
    pub origin: LintOrigin,
    pub default_level: LintLevel,
    pub summary: &'static str,
    pub tags: &'static [&'static str],
    pub sap_aliases: &'static [&'static str],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintDiagnostic {
    pub id: String,
    pub range: LintRange,
    pub message: String,
    pub level: LintLevel,
    pub origin: LintOrigin,
    pub group: LintGroup,
    pub tags: Vec<String>,
    pub sap_aliases: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct LintConfig {
    #[serde(default = "default_lint_profile")]
    pub profile: String,
    #[serde(default)]
    pub report_suppressed: bool,
    #[serde(default)]
    pub groups: BTreeMap<String, LintLevel>,
    #[serde(default)]
    pub rules: BTreeMap<String, LintLevel>,
}

impl Default for LintConfig {
    fn default() -> Self {
        Self {
            profile: default_lint_profile(),
            report_suppressed: false,
            groups: BTreeMap::new(),
            rules: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintPolicy {
    profile: &'static str,
    report_suppressed: bool,
    levels: BTreeMap<String, LintLevel>,
}

impl Default for LintPolicy {
    fn default() -> Self {
        Self::from_config(&LintConfig::default())
    }
}

impl LintPolicy {
    pub fn from_config(config: &LintConfig) -> Self {
        let profile = normalize_lint_profile(&config.profile);
        let group_overrides = normalized_group_overrides(&config.groups);
        let mut levels = BTreeMap::new();

        for metadata in registry() {
            let mut level = profile_level(profile, metadata);
            if let Some(group_level) = group_overrides.get(&metadata.group) {
                level = *group_level;
            }
            levels.insert(metadata.id.to_string(), level);
        }

        for (id, level) in &config.rules {
            let id = normalized_lint_id(id);
            if !id.is_empty() {
                levels.insert(id, *level);
            }
        }

        Self {
            profile,
            report_suppressed: config.report_suppressed,
            levels,
        }
    }

    pub fn from_config_opt(config: Option<&LintConfig>) -> Self {
        config.map(Self::from_config).unwrap_or_default()
    }

    pub const fn profile(&self) -> &'static str {
        self.profile
    }

    pub const fn report_suppressed(&self) -> bool {
        self.report_suppressed
    }

    pub fn level_for(&self, id: &str) -> LintLevel {
        self.levels
            .get(&normalized_lint_id(id))
            .copied()
            .unwrap_or(LintLevel::Allow)
    }

    pub fn is_enabled(&self, id: &str) -> bool {
        self.level_for(id).is_enabled()
    }
}

pub fn registry() -> &'static [LintMetadata] {
    REGISTRY
}

pub fn metadata_for(id: &str) -> Option<&'static LintMetadata> {
    let id = normalized_lint_id(id);
    registry().iter().find(|metadata| metadata.id == id)
}

pub fn normalize_lint_profile(value: &str) -> &'static str {
    match normalized_key(value).as_str() {
        LINT_PROFILE_STRICT => LINT_PROFILE_STRICT,
        LINT_PROFILE_ALL => LINT_PROFILE_ALL,
        LINT_PROFILE_NONE => LINT_PROFILE_NONE,
        _ => LINT_PROFILE_RECOMMENDED,
    }
}

const REGISTRY: &[LintMetadata] = &[
    LintMetadata {
        id: ABAP_LSP_UNREACHABLE_CODE,
        group: LintGroup::Correctness,
        origin: LintOrigin::AbapLsp,
        default_level: LintLevel::Warn,
        summary: "statement can never be executed",
        tags: &["control-flow", "unreachable"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: ABAP_LSP_USE_BEFORE_DEFINITE_ASSIGNMENT,
        group: LintGroup::Correctness,
        origin: LintOrigin::AbapLsp,
        default_level: LintLevel::Warn,
        summary: "variable may be read before it is definitely assigned",
        tags: &["data-flow"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: ABAP_LSP_POSSIBLY_UNBOUND_FIELD_SYMBOL,
        group: LintGroup::Correctness,
        origin: LintOrigin::AbapLsp,
        default_level: LintLevel::Warn,
        summary: "field symbol may be used before it is assigned",
        tags: &["data-flow", "field-symbol"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: ABAP_LSP_DEAD_STORE,
        group: LintGroup::Style,
        origin: LintOrigin::AbapLsp,
        default_level: LintLevel::Warn,
        summary: "assigned value is overwritten or unused before it is read",
        tags: &["data-flow", "unused"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: ABAP_LSP_UNSORTED_READ_TABLE_BINARY_SEARCH,
        group: LintGroup::Correctness,
        origin: LintOrigin::AbapLsp,
        default_level: LintLevel::Warn,
        summary: "READ TABLE ... BINARY SEARCH is used on a table not known to be sorted",
        tags: &["internal-table"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: EPC_UNVERIFIED_OPEN_SQL_SOURCE,
        group: LintGroup::Correctness,
        origin: LintOrigin::SapExtendedProgramCheck,
        default_level: LintLevel::Deny,
        summary: "Open SQL source could not be verified against repository metadata",
        tags: &["open-sql", "repository"],
        sap_aliases: &["extended-program-check"],
    },
    LintMetadata {
        id: EPC_INVALID_OPEN_SQL_INTO_TARGET,
        group: LintGroup::Correctness,
        origin: LintOrigin::SapExtendedProgramCheck,
        default_level: LintLevel::Deny,
        summary: "Open SQL INTO or APPENDING target is incompatible with the query shape",
        tags: &["open-sql"],
        sap_aliases: &["extended-program-check"],
    },
    LintMetadata {
        id: EPC_MISSING_TABLES_DECLARATION,
        group: LintGroup::Correctness,
        origin: LintOrigin::SapExtendedProgramCheck,
        default_level: LintLevel::Deny,
        summary: "classic table work area is used without a matching TABLES declaration",
        tags: &["classic-abap"],
        sap_aliases: &["extended-program-check"],
    },
];

fn profile_level(profile: &str, metadata: &LintMetadata) -> LintLevel {
    match profile {
        LINT_PROFILE_NONE => LintLevel::Allow,
        LINT_PROFILE_STRICT => strict_level(metadata.default_level),
        LINT_PROFILE_ALL => all_level(metadata.default_level),
        _ if metadata.group == LintGroup::Experimental => LintLevel::Allow,
        _ => metadata.default_level,
    }
}

fn strict_level(level: LintLevel) -> LintLevel {
    match level {
        LintLevel::Allow => LintLevel::Warn,
        LintLevel::Info | LintLevel::Warn => LintLevel::Deny,
        LintLevel::Deny => LintLevel::Deny,
    }
}

fn all_level(level: LintLevel) -> LintLevel {
    match level {
        LintLevel::Allow => LintLevel::Warn,
        _ => level,
    }
}

fn normalized_group_overrides(
    groups: &BTreeMap<String, LintLevel>,
) -> BTreeMap<LintGroup, LintLevel> {
    groups
        .iter()
        .filter_map(|(group, level)| LintGroup::from_key(group).map(|group| (group, *level)))
        .collect()
}

fn normalized_key(value: &str) -> String {
    value.trim().to_ascii_lowercase().replace('_', "-")
}

fn normalized_lint_id(value: &str) -> String {
    normalized_key(value)
}

fn default_lint_profile() -> String {
    LINT_PROFILE_RECOMMENDED.to_string()
}

#[cfg(test)]
mod tests {
    use super::{
        ABAP_LSP_DEAD_STORE, ABAP_LSP_UNREACHABLE_CODE, EPC_INVALID_OPEN_SQL_INTO_TARGET,
        EPC_UNVERIFIED_OPEN_SQL_SOURCE, LINT_PROFILE_NONE, LINT_PROFILE_RECOMMENDED,
        LINT_PROFILE_STRICT, LintConfig, LintGroup, LintLevel, LintOrigin, LintPolicy,
        metadata_for, registry,
    };
    use std::collections::BTreeMap;

    #[test]
    fn registry_contains_initial_lints() {
        assert_eq!(registry().len(), 8);

        let dead_store = metadata_for(ABAP_LSP_DEAD_STORE).expect("dead store metadata");
        assert_eq!(dead_store.group, LintGroup::Style);
        assert_eq!(dead_store.origin, LintOrigin::AbapLsp);
        assert_eq!(dead_store.default_level, LintLevel::Warn);

        let open_sql = metadata_for(EPC_UNVERIFIED_OPEN_SQL_SOURCE).expect("open sql metadata");
        assert_eq!(open_sql.group, LintGroup::Correctness);
        assert_eq!(open_sql.origin, LintOrigin::SapExtendedProgramCheck);
        assert_eq!(open_sql.default_level, LintLevel::Deny);
    }

    #[test]
    fn recommended_policy_uses_registry_defaults() {
        let policy = LintPolicy::default();

        assert_eq!(policy.profile(), LINT_PROFILE_RECOMMENDED);
        assert_eq!(policy.level_for(ABAP_LSP_UNREACHABLE_CODE), LintLevel::Warn);
        assert_eq!(
            policy.level_for(EPC_INVALID_OPEN_SQL_INTO_TARGET),
            LintLevel::Deny
        );
        assert_eq!(policy.level_for("unknown.future-lint"), LintLevel::Allow);
    }

    #[test]
    fn policy_applies_profile_group_and_rule_overrides() {
        let config = LintConfig {
            profile: LINT_PROFILE_NONE.to_string(),
            report_suppressed: true,
            groups: BTreeMap::from([("correctness".to_string(), LintLevel::Warn)]),
            rules: BTreeMap::from([
                (ABAP_LSP_DEAD_STORE.to_string(), LintLevel::Deny),
                ("custom.future-lint".to_string(), LintLevel::Info),
            ]),
        };

        let policy = LintPolicy::from_config(&config);

        assert_eq!(policy.profile(), LINT_PROFILE_NONE);
        assert!(policy.report_suppressed());
        assert_eq!(policy.level_for(ABAP_LSP_UNREACHABLE_CODE), LintLevel::Warn);
        assert_eq!(policy.level_for(ABAP_LSP_DEAD_STORE), LintLevel::Deny);
        assert_eq!(policy.level_for("custom.future-lint"), LintLevel::Info);
    }

    #[test]
    fn strict_policy_promotes_warnings_to_denies() {
        let config = LintConfig {
            profile: LINT_PROFILE_STRICT.to_string(),
            ..LintConfig::default()
        };

        let policy = LintPolicy::from_config(&config);

        assert_eq!(policy.level_for(ABAP_LSP_DEAD_STORE), LintLevel::Deny);
        assert_eq!(
            policy.level_for(EPC_INVALID_OPEN_SQL_INTO_TARGET),
            LintLevel::Deny
        );
    }
}
