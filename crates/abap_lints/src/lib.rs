use std::collections::BTreeMap;
use std::ops::Range;

use abap_lexer::{LexedSource, Token, TokenKind, TriviaKind, TriviaPiece};
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
pub const ABAP_LSP_SELECT_STAR: LintId = "abap-lsp.select-star";
pub const ABAP_LSP_SELECT_IN_LOOP: LintId = "abap-lsp.select-in-loop";
pub const ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD: LintId = "abap-lsp.for-all-entries-without-guard";
pub const ABAP_LSP_DYNAMIC_OPEN_SQL: LintId = "abap-lsp.dynamic-open-sql";
pub const ABAP_LSP_IGNORED_AUTHORITY_CHECK: LintId = "abap-lsp.ignored-authority-check";
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintSuppressionKind {
    Pragma,
    PseudoComment,
    AbapLspAllow,
    Config,
}

impl LintSuppressionKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Pragma => "pragma",
            Self::PseudoComment => "pseudo-comment",
            Self::AbapLspAllow => "abap-lsp-allow",
            Self::Config => "config",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintSuppression {
    pub kind: LintSuppressionKind,
    pub range: LintRange,
    pub token: String,
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
    pub suppressed: bool,
    pub suppression: Option<LintSuppression>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectLintAnalysis {
    diagnostics_by_uri: BTreeMap<String, Vec<LintDiagnostic>>,
}

impl ProjectLintAnalysis {
    pub fn from_diagnostics<I, S>(diagnostics: I) -> Self
    where
        I: IntoIterator<Item = (S, LintDiagnostic)>,
        S: Into<String>,
    {
        let mut diagnostics_by_uri = BTreeMap::<String, Vec<LintDiagnostic>>::new();
        for (uri, diagnostic) in diagnostics {
            diagnostics_by_uri
                .entry(uri.into())
                .or_default()
                .push(diagnostic);
        }
        for diagnostics in diagnostics_by_uri.values_mut() {
            sort_lint_diagnostics(diagnostics);
            diagnostics.dedup();
        }
        Self { diagnostics_by_uri }
    }

    pub fn diagnostics_for_uri(&self, uri: &str) -> &[LintDiagnostic] {
        self.diagnostics_by_uri
            .get(uri)
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    pub fn diagnostics_by_uri(&self) -> &BTreeMap<String, Vec<LintDiagnostic>> {
        &self.diagnostics_by_uri
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics_by_uri.values().all(Vec::is_empty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuppressionIndex {
    entries: Vec<SuppressionEntry>,
}

impl SuppressionIndex {
    pub fn new(source: &str, lexed: &LexedSource) -> Self {
        let statements = StatementRanges::new(lexed.tokens.as_ref());
        if statements.statements.is_empty() {
            return Self {
                entries: Vec::new(),
            };
        }

        let mut entries = Vec::new();
        for statement in &statements.statements {
            let first_token = &lexed.tokens[statement.first_token];
            for piece in lexed.leading_trivia(first_token) {
                match piece.kind {
                    TriviaKind::Pragma
                        if same_physical_line(source, piece.range.end, first_token.range.start) =>
                    {
                        push_pragma_entry(source, piece, statement.range.clone(), &mut entries);
                    }
                    TriviaKind::Comment => {
                        if let Some(action) = parse_abap_lsp_allow_comment(source, piece) {
                            match action {
                                AbapLspAllowAction::NextStatement(selectors)
                                    if !selectors.is_empty() =>
                                {
                                    entries.push(SuppressionEntry {
                                        target: SuppressionTarget::Statement(
                                            statement.range.clone(),
                                        ),
                                        selectors,
                                        kind: LintSuppressionKind::AbapLspAllow,
                                        range: piece.range.clone(),
                                        token: piece.lexeme(source).trim().to_string(),
                                    });
                                }
                                AbapLspAllowAction::File(selectors) if !selectors.is_empty() => {
                                    entries.push(SuppressionEntry {
                                        target: SuppressionTarget::File,
                                        selectors,
                                        kind: LintSuppressionKind::AbapLspAllow,
                                        range: piece.range.clone(),
                                        token: piece.lexeme(source).trim().to_string(),
                                    });
                                }
                                AbapLspAllowAction::CurrentStatement(_) => {}
                                AbapLspAllowAction::NextStatement(_)
                                | AbapLspAllowAction::File(_) => {}
                            }
                        }
                    }
                    _ => {}
                }
            }

            for token_idx in statement.first_token..=statement.last_token {
                let token = &lexed.tokens[token_idx];
                for piece in lexed.trailing_trivia(token) {
                    match piece.kind {
                        TriviaKind::Pragma => {
                            push_pragma_entry(source, piece, statement.range.clone(), &mut entries);
                        }
                        TriviaKind::Comment => {
                            push_pseudo_comment_entries(
                                source,
                                piece,
                                statement.range.clone(),
                                &mut entries,
                            );
                            if let Some(AbapLspAllowAction::CurrentStatement(selectors)) =
                                parse_abap_lsp_allow_comment(source, piece)
                            {
                                if !selectors.is_empty() {
                                    entries.push(SuppressionEntry {
                                        target: SuppressionTarget::Statement(
                                            statement.range.clone(),
                                        ),
                                        selectors,
                                        kind: LintSuppressionKind::AbapLspAllow,
                                        range: piece.range.clone(),
                                        token: piece.lexeme(source).trim().to_string(),
                                    });
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        entries.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
                .then(left.kind.as_str().cmp(right.kind.as_str()))
                .then(left.token.cmp(&right.token))
        });
        entries.dedup();
        Self { entries }
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn suppression_for(&self, diagnostic: &LintDiagnostic) -> Option<LintSuppression> {
        let lint_id = normalized_lint_id(&diagnostic.id);
        let aliases: Vec<_> = diagnostic
            .sap_aliases
            .iter()
            .map(|alias| normalized_lint_alias(alias))
            .collect();

        self.entries
            .iter()
            .find(|entry| {
                entry.applies_to(diagnostic.range.start)
                    && entry.matches(lint_id.as_str(), aliases.as_slice())
            })
            .map(|entry| LintSuppression {
                kind: entry.kind,
                range: entry.range.clone(),
                token: entry.token.clone(),
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SuppressionEntry {
    target: SuppressionTarget,
    selectors: Vec<SuppressionSelector>,
    kind: LintSuppressionKind,
    range: LintRange,
    token: String,
}

impl SuppressionEntry {
    fn applies_to(&self, offset: usize) -> bool {
        match &self.target {
            SuppressionTarget::File => true,
            SuppressionTarget::Statement(range) => range.start <= offset && offset < range.end,
        }
    }

    fn matches(&self, lint_id: &str, aliases: &[String]) -> bool {
        self.selectors.iter().any(|selector| match selector {
            SuppressionSelector::LintId(id) => id == lint_id,
            SuppressionSelector::SapAlias(alias) => {
                aliases.iter().any(|candidate| candidate == alias)
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SuppressionTarget {
    File,
    Statement(LintRange),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum SuppressionSelector {
    LintId(String),
    SapAlias(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StatementRange {
    first_token: usize,
    last_token: usize,
    range: LintRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StatementRanges {
    statements: Vec<StatementRange>,
}

impl StatementRanges {
    fn new(tokens: &[Token]) -> Self {
        let significant_len = tokens
            .iter()
            .position(|token| token.kind == TokenKind::Eof)
            .unwrap_or(tokens.len());
        let mut statements = Vec::new();
        let mut first_token = None;

        for idx in 0..significant_len {
            if first_token.is_none() {
                first_token = Some(idx);
            }
            if tokens[idx].kind == TokenKind::Period {
                let start = first_token.expect("statement start should be set");
                statements.push(StatementRange {
                    first_token: start,
                    last_token: idx,
                    range: tokens[start].range.start..tokens[idx].range.end,
                });
                first_token = None;
            }
        }

        if let Some(start) = first_token {
            let last = significant_len.saturating_sub(1);
            if start <= last {
                statements.push(StatementRange {
                    first_token: start,
                    last_token: last,
                    range: tokens[start].range.start..tokens[last].range.end,
                });
            }
        }

        Self { statements }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AbapLspAllowAction {
    CurrentStatement(Vec<SuppressionSelector>),
    NextStatement(Vec<SuppressionSelector>),
    File(Vec<SuppressionSelector>),
}

fn push_pragma_entry(
    source: &str,
    piece: &TriviaPiece,
    statement_range: LintRange,
    entries: &mut Vec<SuppressionEntry>,
) {
    let token = piece.lexeme(source).trim();
    let Some(alias) = token.strip_prefix("##") else {
        return;
    };
    let alias = alias.trim();
    if alias.is_empty() {
        return;
    }
    entries.push(SuppressionEntry {
        target: SuppressionTarget::Statement(statement_range),
        selectors: vec![SuppressionSelector::SapAlias(normalized_lint_alias(alias))],
        kind: LintSuppressionKind::Pragma,
        range: piece.range.clone(),
        token: token.to_string(),
    });
}

fn push_pseudo_comment_entries(
    source: &str,
    piece: &TriviaPiece,
    statement_range: LintRange,
    entries: &mut Vec<SuppressionEntry>,
) {
    for alias in pseudo_comment_aliases(piece.lexeme(source)) {
        entries.push(SuppressionEntry {
            target: SuppressionTarget::Statement(statement_range.clone()),
            selectors: vec![SuppressionSelector::SapAlias(normalized_lint_alias(
                alias.as_str(),
            ))],
            kind: LintSuppressionKind::PseudoComment,
            range: piece.range.clone(),
            token: alias,
        });
    }
}

fn pseudo_comment_aliases(comment: &str) -> Vec<String> {
    let Some(ec_pos) = find_ascii_case_insensitive(comment, "#EC") else {
        return Vec::new();
    };
    let tail = &comment[ec_pos + 3..];
    let mut out = Vec::new();
    for part in tail.split_whitespace() {
        let alias = part.trim_matches(|ch: char| !is_selector_char(ch));
        if alias.is_empty() || alias == "*" {
            continue;
        }
        if alias.chars().all(is_selector_char) {
            out.push(alias.to_string());
        }
    }
    out
}

fn parse_abap_lsp_allow_comment(source: &str, piece: &TriviaPiece) -> Option<AbapLspAllowAction> {
    let mut text = piece.lexeme(source).trim_start();
    text = text
        .strip_prefix('"')
        .or_else(|| text.strip_prefix('*'))
        .unwrap_or(text)
        .trim_start();
    let marker_pos = find_ascii_case_insensitive(text, "abap-lsp:")?;
    let mut rest = text[marker_pos + "abap-lsp:".len()..].trim_start();
    let action = if let Some(after) = strip_ascii_case_prefix(rest, "allow-next-line") {
        rest = after.trim_start();
        AllowActionName::NextStatement
    } else if let Some(after) = strip_ascii_case_prefix(rest, "allow-file") {
        rest = after.trim_start();
        AllowActionName::File
    } else if let Some(after) = strip_ascii_case_prefix(rest, "allow") {
        rest = after.trim_start();
        AllowActionName::CurrentStatement
    } else {
        return None;
    };
    let inner = rest.strip_prefix('(')?.split_once(')')?.0;
    let selectors = parse_lint_id_selectors(inner);
    Some(match action {
        AllowActionName::CurrentStatement => AbapLspAllowAction::CurrentStatement(selectors),
        AllowActionName::NextStatement => AbapLspAllowAction::NextStatement(selectors),
        AllowActionName::File => AbapLspAllowAction::File(selectors),
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AllowActionName {
    CurrentStatement,
    NextStatement,
    File,
}

fn parse_lint_id_selectors(value: &str) -> Vec<SuppressionSelector> {
    let mut selectors = value
        .split(',')
        .filter_map(|part| {
            let part = part.trim();
            if part.is_empty() || part.contains(':') {
                return None;
            }
            if !part.chars().all(is_selector_char) {
                return None;
            }
            let id = normalized_lint_id(part);
            (!id.is_empty()).then_some(SuppressionSelector::LintId(id))
        })
        .collect::<Vec<_>>();
    selectors.sort();
    selectors.dedup();
    selectors
}

fn same_physical_line(source: &str, left_end: usize, right_start: usize) -> bool {
    source.get(left_end..right_start).is_some_and(|gap| {
        !gap.as_bytes()
            .iter()
            .any(|byte| *byte == b'\n' || *byte == b'\r')
    })
}

fn strip_ascii_case_prefix<'a>(value: &'a str, prefix: &str) -> Option<&'a str> {
    let candidate = value.get(..prefix.len())?;
    candidate
        .eq_ignore_ascii_case(prefix)
        .then(|| &value[prefix.len()..])
}

fn find_ascii_case_insensitive(haystack: &str, needle: &str) -> Option<usize> {
    let needle_len = needle.len();
    if needle_len == 0 || haystack.len() < needle_len {
        return None;
    }
    (0..=haystack.len() - needle_len).find(|start| {
        haystack
            .get(*start..*start + needle_len)
            .is_some_and(|candidate| candidate.eq_ignore_ascii_case(needle))
    })
}

fn is_selector_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-' | '.')
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct LintConfig {
    #[serde(default = "default_lint_profile")]
    pub profile: String,
    #[serde(default)]
    pub report_suppressed: bool,
    #[serde(default)]
    pub sap_atc: SapAtcLintConfig,
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
            sap_atc: SapAtcLintConfig::default(),
            groups: BTreeMap::new(),
            rules: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SapAtcLintMode {
    Off,
    Manual,
    OnSave,
}

impl Default for SapAtcLintMode {
    fn default() -> Self {
        Self::Off
    }
}

impl SapAtcLintMode {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Off => "off",
            Self::Manual => "manual",
            Self::OnSave => "on-save",
        }
    }

    pub const fn is_enabled(self) -> bool {
        !matches!(self, Self::Off)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct SapAtcLintConfig {
    #[serde(default)]
    pub mode: SapAtcLintMode,
    #[serde(default = "default_sap_atc_check_variant")]
    pub check_variant: String,
    #[serde(default)]
    pub configuration: Option<String>,
}

impl Default for SapAtcLintConfig {
    fn default() -> Self {
        Self {
            mode: SapAtcLintMode::Off,
            check_variant: default_sap_atc_check_variant(),
            configuration: None,
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
            levels.insert(
                metadata.id.to_string(),
                enforce_hard_error_level(metadata, level),
            );
        }

        for (id, level) in &config.rules {
            let id = normalized_lint_id(id);
            if !id.is_empty() {
                let level = metadata_for(id.as_str())
                    .map(|metadata| enforce_hard_error_level(metadata, *level))
                    .unwrap_or(*level);
                levels.insert(id, level);
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
        default_level: LintLevel::Info,
        summary: "assigned value is overwritten or unused before it is read",
        tags: &["data-flow", "unused"],
        sap_aliases: &["NEEDED"],
    },
    LintMetadata {
        id: ABAP_LSP_UNSORTED_READ_TABLE_BINARY_SEARCH,
        group: LintGroup::Correctness,
        origin: LintOrigin::AbapLsp,
        default_level: LintLevel::Info,
        summary: "READ TABLE ... BINARY SEARCH is used on a table not known to be sorted",
        tags: &["internal-table"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: ABAP_LSP_SELECT_STAR,
        group: LintGroup::Performance,
        origin: LintOrigin::SapCodeInspector,
        default_level: LintLevel::Info,
        summary: "Open SQL SELECT * reads all columns instead of an explicit projection",
        tags: &["open-sql", "projection"],
        sap_aliases: &["CI_ALL_FIELDS_NEEDED"],
    },
    LintMetadata {
        id: ABAP_LSP_SELECT_IN_LOOP,
        group: LintGroup::Performance,
        origin: LintOrigin::SapCodeInspector,
        default_level: LintLevel::Info,
        summary: "Open SQL SELECT runs inside a LOOP, DO, or WHILE body",
        tags: &["open-sql", "loop"],
        sap_aliases: &["CI_SEL_NESTED"],
    },
    LintMetadata {
        id: ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD,
        group: LintGroup::Correctness,
        origin: LintOrigin::SapCodeInspector,
        default_level: LintLevel::Info,
        summary: "FOR ALL ENTRIES is used without an enclosing initial-table guard",
        tags: &["open-sql", "for-all-entries"],
        sap_aliases: &["CI_FAE_LINES_ENSURED"],
    },
    LintMetadata {
        id: ABAP_LSP_DYNAMIC_OPEN_SQL,
        group: LintGroup::Security,
        origin: LintOrigin::SapCodeInspector,
        default_level: LintLevel::Info,
        summary: "Open SQL contains a dynamic source, projection, or WHERE fragment",
        tags: &["open-sql", "dynamic", "experimental"],
        sap_aliases: &[],
    },
    LintMetadata {
        id: ABAP_LSP_IGNORED_AUTHORITY_CHECK,
        group: LintGroup::Security,
        origin: LintOrigin::SapAtc,
        default_level: LintLevel::Info,
        summary: "AUTHORITY-CHECK result is not checked before sy-subrc is overwritten",
        tags: &["authorization", "sy-subrc"],
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

fn enforce_hard_error_level(metadata: &LintMetadata, level: LintLevel) -> LintLevel {
    if metadata.default_level == LintLevel::Deny {
        LintLevel::Deny
    } else {
        level
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

fn normalized_lint_alias(value: &str) -> String {
    normalized_key(value)
}

fn sort_lint_diagnostics(diagnostics: &mut [LintDiagnostic]) {
    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.id.cmp(&right.id))
            .then(left.message.cmp(&right.message))
    });
}

fn default_lint_profile() -> String {
    LINT_PROFILE_RECOMMENDED.to_string()
}

fn default_sap_atc_check_variant() -> String {
    "DEFAULT".to_string()
}

#[cfg(test)]
mod tests {
    use super::{
        ABAP_LSP_DEAD_STORE, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD,
        ABAP_LSP_IGNORED_AUTHORITY_CHECK, ABAP_LSP_SELECT_IN_LOOP, ABAP_LSP_SELECT_STAR,
        ABAP_LSP_UNREACHABLE_CODE, EPC_INVALID_OPEN_SQL_INTO_TARGET,
        EPC_UNVERIFIED_OPEN_SQL_SOURCE, LINT_PROFILE_NONE, LINT_PROFILE_RECOMMENDED,
        LINT_PROFILE_STRICT, LintConfig, LintDiagnostic, LintGroup, LintLevel, LintOrigin,
        LintPolicy, LintSuppressionKind, SuppressionIndex, metadata_for, registry,
    };
    use abap_lexer::tokenize;
    use std::collections::BTreeMap;

    #[test]
    fn registry_contains_initial_lints() {
        assert_eq!(registry().len(), 13);

        let dead_store = metadata_for(ABAP_LSP_DEAD_STORE).expect("dead store metadata");
        assert_eq!(dead_store.group, LintGroup::Style);
        assert_eq!(dead_store.origin, LintOrigin::AbapLsp);
        assert_eq!(dead_store.default_level, LintLevel::Info);

        let open_sql = metadata_for(EPC_UNVERIFIED_OPEN_SQL_SOURCE).expect("open sql metadata");
        assert_eq!(open_sql.group, LintGroup::Correctness);
        assert_eq!(open_sql.origin, LintOrigin::SapExtendedProgramCheck);
        assert_eq!(open_sql.default_level, LintLevel::Deny);

        let select_star = metadata_for(ABAP_LSP_SELECT_STAR).expect("select star metadata");
        assert_eq!(select_star.group, LintGroup::Performance);
        assert_eq!(select_star.origin, LintOrigin::SapCodeInspector);
        assert_eq!(select_star.default_level, LintLevel::Info);
        assert_eq!(select_star.sap_aliases, &["CI_ALL_FIELDS_NEEDED"]);

        let select_in_loop =
            metadata_for(ABAP_LSP_SELECT_IN_LOOP).expect("select in loop metadata");
        assert_eq!(select_in_loop.default_level, LintLevel::Info);
        assert_eq!(select_in_loop.sap_aliases, &["CI_SEL_NESTED"]);

        let for_all_entries =
            metadata_for(ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD).expect("fae metadata");
        assert_eq!(for_all_entries.group, LintGroup::Correctness);
        assert_eq!(for_all_entries.sap_aliases, &["CI_FAE_LINES_ENSURED"]);

        let authority_check =
            metadata_for(ABAP_LSP_IGNORED_AUTHORITY_CHECK).expect("authority check metadata");
        assert_eq!(authority_check.group, LintGroup::Security);
        assert_eq!(authority_check.origin, LintOrigin::SapAtc);
        assert_eq!(authority_check.default_level, LintLevel::Info);
    }

    #[test]
    fn recommended_policy_uses_registry_defaults() {
        let policy = LintPolicy::default();

        assert_eq!(policy.profile(), LINT_PROFILE_RECOMMENDED);
        assert_eq!(policy.level_for(ABAP_LSP_UNREACHABLE_CODE), LintLevel::Warn);
        assert_eq!(policy.level_for(ABAP_LSP_DEAD_STORE), LintLevel::Info);
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
            ..LintConfig::default()
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

    #[test]
    fn hard_error_lints_are_not_muted_by_lint_profile_or_rule_overrides() {
        let none = LintPolicy::from_config(&LintConfig {
            profile: LINT_PROFILE_NONE.to_string(),
            ..LintConfig::default()
        });
        assert_eq!(
            none.level_for(EPC_INVALID_OPEN_SQL_INTO_TARGET),
            LintLevel::Deny
        );

        let explicit_allow = LintPolicy::from_config(&LintConfig {
            rules: BTreeMap::from([(
                EPC_INVALID_OPEN_SQL_INTO_TARGET.to_string(),
                LintLevel::Allow,
            )]),
            ..LintConfig::default()
        });
        assert_eq!(
            explicit_allow.level_for(EPC_INVALID_OPEN_SQL_INTO_TARGET),
            LintLevel::Deny
        );
    }

    fn lint(id: &str, range: std::ops::Range<usize>, aliases: &[&str]) -> LintDiagnostic {
        LintDiagnostic {
            id: id.to_string(),
            range,
            message: "test lint".to_string(),
            level: LintLevel::Warn,
            origin: LintOrigin::AbapLsp,
            group: LintGroup::Correctness,
            tags: Vec::new(),
            sap_aliases: aliases.iter().map(|alias| (*alias).to_string()).collect(),
            suppressed: false,
            suppression: None,
        }
    }

    #[test]
    fn pseudo_comment_suppresses_matching_sap_alias() {
        let source = "\
REPORT ztest.
SELECT * FROM mara INTO TABLE @DATA(lt_mara). \"#EC CI_BUFFJOIN";
        let lexed = tokenize(source).lexed;
        let index = SuppressionIndex::new(source, &lexed);
        let offset = source.find("SELECT").expect("SELECT");
        let suppressed = index
            .suppression_for(&lint(
                "sap.buffered-join",
                offset..offset + "SELECT".len(),
                &["CI_BUFFJOIN"],
            ))
            .expect("matching CI alias suppression");

        assert_eq!(suppressed.kind, LintSuppressionKind::PseudoComment);
        assert_eq!(suppressed.token, "CI_BUFFJOIN");
    }

    #[test]
    fn pragma_suppresses_matching_sap_alias() {
        let source = "\
REPORT ztest.
READ TABLE lt_data INDEX 1 INTO DATA(ls_row) ##subrc_read.";
        let lexed = tokenize(source).lexed;
        let index = SuppressionIndex::new(source, &lexed);
        let offset = source.find("READ").expect("READ");
        let suppressed = index
            .suppression_for(&lint(
                "sap.subrc-read",
                offset..offset + "READ".len(),
                &["SUBRC_READ"],
            ))
            .expect("matching pragma alias suppression");

        assert_eq!(suppressed.kind, LintSuppressionKind::Pragma);
        assert_eq!(suppressed.token, "##subrc_read");
    }

    #[test]
    fn unrelated_suppression_does_not_hide_diagnostic() {
        let source = "\
REPORT ztest.
SELECT * FROM mara INTO TABLE @DATA(lt_mara). \"#EC CI_BUFFJOIN";
        let lexed = tokenize(source).lexed;
        let index = SuppressionIndex::new(source, &lexed);
        let offset = source.find("SELECT").expect("SELECT");

        assert!(
            index
                .suppression_for(&lint(
                    "sap.where-clause",
                    offset..offset + "SELECT".len(),
                    &["CI_NOWHERE"],
                ))
                .is_none()
        );
    }

    #[test]
    fn abap_lsp_allow_suppresses_only_intended_lint_and_statement() {
        let source = "\
REPORT ztest.
DATA lv_first TYPE i. \" abap-lsp:allow(abap-lsp.dead-store)
DATA lv_second TYPE i.";
        let lexed = tokenize(source).lexed;
        let index = SuppressionIndex::new(source, &lexed);
        let first = source.find("lv_first").expect("lv_first");
        let second = source.find("lv_second").expect("lv_second");
        let suppressed = index
            .suppression_for(&lint(
                ABAP_LSP_DEAD_STORE,
                first..first + "lv_first".len(),
                &[],
            ))
            .expect("matching abap-lsp allow");

        assert_eq!(suppressed.kind, LintSuppressionKind::AbapLspAllow);
        assert!(
            index
                .suppression_for(&lint(
                    ABAP_LSP_UNREACHABLE_CODE,
                    first..first + "lv_first".len(),
                    &[],
                ))
                .is_none()
        );
        assert!(
            index
                .suppression_for(&lint(
                    ABAP_LSP_DEAD_STORE,
                    second..second + "lv_second".len(),
                    &[],
                ))
                .is_none()
        );
    }

    #[test]
    fn broad_group_allow_comment_does_not_suppress() {
        let source = "\
REPORT ztest.
DATA lv_first TYPE i. \" abap-lsp:allow(group:style, all)";
        let lexed = tokenize(source).lexed;
        let index = SuppressionIndex::new(source, &lexed);
        let first = source.find("lv_first").expect("lv_first");

        assert!(
            index
                .suppression_for(&lint(
                    ABAP_LSP_DEAD_STORE,
                    first..first + "lv_first".len(),
                    &[],
                ))
                .is_none()
        );
    }
}
