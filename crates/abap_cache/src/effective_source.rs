use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use abap_symbols::{ByteRange, UnitId};
use serde::Serialize;

use crate::AnalysisSnapshot;

const DEFAULT_MAX_UNITS: usize = 256;
const DEFAULT_MAX_DEPTH: usize = 64;
const DEFAULT_MAX_OUTPUT_BYTES: usize = 4_000_000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EffectiveSourceLimits {
    pub max_units: usize,
    pub max_depth: usize,
    pub max_output_bytes: usize,
}

impl Default for EffectiveSourceLimits {
    fn default() -> Self {
        Self {
            max_units: DEFAULT_MAX_UNITS,
            max_depth: DEFAULT_MAX_DEPTH,
            max_output_bytes: DEFAULT_MAX_OUTPUT_BYTES,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectiveSourceUnit {
    pub unit_id: u32,
    pub uri: String,
    pub object_name: Option<String>,
    pub is_dependency: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectiveSourceSegment {
    pub expanded_range: ByteRange,
    pub source_unit: EffectiveSourceUnit,
    pub source_range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectiveSourceDiagnostic {
    pub kind: &'static str,
    pub message: String,
    pub include_name: Option<String>,
    pub source_uri: String,
    pub source_range: Option<ByteRange>,
    pub target_uri: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectiveSource {
    pub schema: &'static str,
    pub schema_version: u32,
    pub root: EffectiveSourceUnit,
    pub expanded_text: String,
    pub segments: Vec<EffectiveSourceSegment>,
    pub included_units: Vec<EffectiveSourceUnit>,
    pub diagnostics: Vec<EffectiveSourceDiagnostic>,
}

#[derive(Debug, Clone)]
struct IncludeNameEntry {
    name: Arc<str>,
    range: Range<usize>,
    target: Option<UnitId>,
}

#[derive(Debug, Clone)]
struct IncludeStmtEntry {
    range: Range<usize>,
    names: Vec<IncludeNameEntry>,
}

#[derive(Debug, Clone)]
struct IncludeSite<'a> {
    snapshot: &'a AnalysisSnapshot,
    entry: &'a IncludeNameEntry,
}

struct ExpansionBuilder<'a> {
    snapshots_by_unit: HashMap<UnitId, &'a AnalysisSnapshot>,
    limits: EffectiveSourceLimits,
    expanded_text: String,
    segments: Vec<EffectiveSourceSegment>,
    included_units: Vec<EffectiveSourceUnit>,
    diagnostics: Vec<EffectiveSourceDiagnostic>,
    expanded_units: HashSet<UnitId>,
    visiting_units: Vec<UnitId>,
    output_truncated: bool,
}

pub fn build_effective_source(
    root: &AnalysisSnapshot,
    snapshots: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
) -> EffectiveSource {
    build_effective_source_with_limits(root, snapshots, EffectiveSourceLimits::default())
}

pub fn build_effective_source_with_limits(
    root: &AnalysisSnapshot,
    snapshots: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    limits: EffectiveSourceLimits,
) -> EffectiveSource {
    let snapshots_by_unit: HashMap<_, _> = snapshots
        .values()
        .map(|snapshot| (snapshot.symbols.unit_id, snapshot.as_ref()))
        .collect();

    let mut builder = ExpansionBuilder {
        snapshots_by_unit,
        limits,
        expanded_text: String::new(),
        segments: Vec::new(),
        included_units: Vec::new(),
        diagnostics: Vec::new(),
        expanded_units: HashSet::from([root.symbols.unit_id]),
        visiting_units: vec![root.symbols.unit_id],
        output_truncated: false,
    };
    builder.expand_unit(root, 0);
    builder.visiting_units.pop();

    EffectiveSource {
        schema: "abap/effective-source",
        schema_version: 1,
        root: unit_descriptor(root),
        expanded_text: builder.expanded_text,
        segments: builder.segments,
        included_units: builder.included_units,
        diagnostics: builder.diagnostics,
    }
}

impl ExpansionBuilder<'_> {
    fn expand_unit(&mut self, snapshot: &AnalysisSnapshot, depth: usize) {
        if self.output_truncated {
            return;
        }

        let statements = include_statements(snapshot);
        let mut cursor = 0usize;
        for statement in statements {
            if self.output_truncated {
                break;
            }

            if cursor < statement.range.start {
                self.append_source_slice(snapshot, cursor..statement.range.start);
            }
            for include_name in &statement.names {
                if self.output_truncated {
                    break;
                }
                self.expand_include(
                    IncludeSite {
                        snapshot,
                        entry: include_name,
                    },
                    depth + 1,
                );
            }
            cursor = statement.range.end;
        }

        if !self.output_truncated && cursor < snapshot.text.len() {
            self.append_source_slice(snapshot, cursor..snapshot.text.len());
        }
    }

    fn expand_include(&mut self, site: IncludeSite<'_>, depth: usize) {
        let Some(target_id) = site.entry.target else {
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "missing_include",
                message: format!("include '{}' could not be resolved", site.entry.name),
                include_name: Some(site.entry.name.to_string()),
                source_uri: site.snapshot.uri.to_string(),
                source_range: Some(byte_range(&site.entry.range)),
                target_uri: None,
            });
            return;
        };

        if self.visiting_units.contains(&target_id) {
            let target_uri = self
                .snapshots_by_unit
                .get(&target_id)
                .map(|snapshot| snapshot.uri.to_string())
                .or_else(|| {
                    site.snapshot
                        .project
                        .units
                        .get(target_id.as_usize())
                        .map(|unit| unit.uri.to_string())
                });
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "include_cycle",
                message: format!("include '{}' would introduce a cycle", site.entry.name),
                include_name: Some(site.entry.name.to_string()),
                source_uri: site.snapshot.uri.to_string(),
                source_range: Some(byte_range(&site.entry.range)),
                target_uri,
            });
            return;
        }

        if self.expanded_units.contains(&target_id) {
            let target_uri = self
                .snapshots_by_unit
                .get(&target_id)
                .map(|snapshot| snapshot.uri.to_string());
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "repeated_include",
                message: format!("include '{}' was already expanded earlier", site.entry.name),
                include_name: Some(site.entry.name.to_string()),
                source_uri: site.snapshot.uri.to_string(),
                source_range: Some(byte_range(&site.entry.range)),
                target_uri,
            });
            return;
        }

        if depth > self.limits.max_depth {
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "truncation",
                message: format!(
                    "include '{}' exceeded max include depth of {}",
                    site.entry.name, self.limits.max_depth
                ),
                include_name: Some(site.entry.name.to_string()),
                source_uri: site.snapshot.uri.to_string(),
                source_range: Some(byte_range(&site.entry.range)),
                target_uri: None,
            });
            return;
        }

        if self.included_units.len() >= self.limits.max_units {
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "truncation",
                message: format!(
                    "include '{}' exceeded max expanded unit count of {}",
                    site.entry.name, self.limits.max_units
                ),
                include_name: Some(site.entry.name.to_string()),
                source_uri: site.snapshot.uri.to_string(),
                source_range: Some(byte_range(&site.entry.range)),
                target_uri: None,
            });
            return;
        }

        let Some(target_snapshot) = self.snapshots_by_unit.get(&target_id).copied() else {
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "missing_include",
                message: format!(
                    "include '{}' resolved semantically, but source text was not loaded",
                    site.entry.name
                ),
                include_name: Some(site.entry.name.to_string()),
                source_uri: site.snapshot.uri.to_string(),
                source_range: Some(byte_range(&site.entry.range)),
                target_uri: site
                    .snapshot
                    .project
                    .units
                    .get(target_id.as_usize())
                    .map(|unit| unit.uri.to_string()),
            });
            return;
        };

        self.expanded_units.insert(target_id);
        self.included_units.push(unit_descriptor(target_snapshot));
        self.visiting_units.push(target_id);
        self.expand_unit(target_snapshot, depth);
        self.visiting_units.pop();
    }

    fn append_source_slice(&mut self, snapshot: &AnalysisSnapshot, range: Range<usize>) {
        if range.is_empty() || self.output_truncated {
            return;
        }

        let start = range.start.min(snapshot.text.len());
        let end = range.end.min(snapshot.text.len());
        if start >= end {
            return;
        }

        let remaining = self
            .limits
            .max_output_bytes
            .saturating_sub(self.expanded_text.len());
        if remaining == 0 {
            self.output_truncated = true;
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "truncation",
                message: format!(
                    "effective source exceeded max output size of {} bytes",
                    self.limits.max_output_bytes
                ),
                include_name: None,
                source_uri: snapshot.uri.to_string(),
                source_range: Some(byte_range(&(start..end))),
                target_uri: None,
            });
            return;
        }

        let allowed_end = end.min(start + remaining);
        let expanded_start = self.expanded_text.len();
        self.expanded_text
            .push_str(&snapshot.text[start..allowed_end]);
        let expanded_end = self.expanded_text.len();
        self.push_segment(snapshot, expanded_start..expanded_end, start..allowed_end);

        if allowed_end < end {
            self.output_truncated = true;
            self.diagnostics.push(EffectiveSourceDiagnostic {
                kind: "truncation",
                message: format!(
                    "effective source exceeded max output size of {} bytes",
                    self.limits.max_output_bytes
                ),
                include_name: None,
                source_uri: snapshot.uri.to_string(),
                source_range: Some(byte_range(&(allowed_end..end))),
                target_uri: None,
            });
        }
    }

    fn push_segment(
        &mut self,
        snapshot: &AnalysisSnapshot,
        expanded_range: Range<usize>,
        source_range: Range<usize>,
    ) {
        if expanded_range.is_empty() || source_range.is_empty() {
            return;
        }

        let source_unit = unit_descriptor(snapshot);
        if let Some(last) = self.segments.last_mut()
            && last.source_unit == source_unit
            && last.expanded_range.end == expanded_range.start
            && last.source_range.end == source_range.start
        {
            last.expanded_range.end = expanded_range.end;
            last.source_range.end = source_range.end;
            return;
        }

        self.segments.push(EffectiveSourceSegment {
            expanded_range: byte_range(&expanded_range),
            source_unit,
            source_range: byte_range(&source_range),
        });
    }
}

fn include_statements(snapshot: &AnalysisSnapshot) -> Vec<IncludeStmtEntry> {
    let edge_by_start: HashMap<_, _> = snapshot
        .symbols
        .include_edges
        .iter()
        .map(|edge| ((edge.range.start, edge.range.end), edge))
        .collect();
    let mut stack = vec![snapshot.parse.file.root()];
    let mut statements = Vec::new();

    while let Some(node) = stack.pop() {
        let mut children: Vec<_> = snapshot.parse.file.children(node).collect();
        children.reverse();
        stack.extend(children);

        if snapshot.parse.file.kind(node).as_str() != "IncludeStmt" {
            continue;
        }

        let mut names = Vec::new();
        for child in snapshot.parse.file.children(node) {
            if snapshot.parse.file.kind(child).as_str() != "IncludeName" {
                continue;
            }
            let range = snapshot.parse.file.range(child);
            let name = snapshot
                .text
                .get(range.clone())
                .map(|text| Arc::<str>::from(text.trim().to_ascii_lowercase()))
                .unwrap_or_else(|| Arc::<str>::from(""));
            let target = edge_by_start
                .get(&(range.start, range.end))
                .and_then(|edge| edge.target);
            names.push(IncludeNameEntry {
                name,
                range,
                target,
            });
        }

        statements.push(IncludeStmtEntry {
            range: snapshot.parse.file.range(node),
            names,
        });
    }

    statements.sort_by(|left, right| left.range.start.cmp(&right.range.start));
    statements
}

fn unit_descriptor(snapshot: &AnalysisSnapshot) -> EffectiveSourceUnit {
    EffectiveSourceUnit {
        unit_id: snapshot.symbols.unit_id.0,
        uri: snapshot.uri.to_string(),
        object_name: snapshot.object_name.as_ref().map(|name| name.to_string()),
        is_dependency: snapshot.is_dependency,
    }
}

fn byte_range(range: &Range<usize>) -> ByteRange {
    ByteRange {
        start: range.start,
        end: range.end,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;

    use super::{EffectiveSourceDiagnostic, build_effective_source};
    use crate::{DocumentInput, DocumentStore};

    fn diagnostics_of_kind<'a>(
        diagnostics: &'a [EffectiveSourceDiagnostic],
        kind: &str,
    ) -> Vec<&'a EffectiveSourceDiagnostic> {
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.kind == kind)
            .collect()
    }

    #[test]
    fn expands_simple_include_chain_in_order() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE zinc_first.
START-OF-SELECTION.
  lv_second = 1.
";
        let first_src = "\
INCLUDE zinc_second.
DATA lv_first TYPE i.
";
        let second_src = "DATA lv_second TYPE i.\n";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///zmain.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: Some(Arc::from("zmain")),
            },
            DocumentInput {
                uri: Arc::from("file:///zinc_first.abap"),
                version: 1,
                text: Arc::from(first_src),
                is_dependency: false,
                object_name: Some(Arc::from("zinc_first")),
            },
            DocumentInput {
                uri: Arc::from("file:///zinc_second.abap"),
                version: 1,
                text: Arc::from(second_src),
                is_dependency: false,
                object_name: Some(Arc::from("zinc_second")),
            },
        ]);
        let main = snapshots.get("file:///zmain.abap").expect("main snapshot");

        let expanded = build_effective_source(main, &snapshots);

        assert_eq!(
            expanded.expanded_text,
            "\
REPORT zmain.
DATA lv_second TYPE i.

DATA lv_first TYPE i.

START-OF-SELECTION.
  lv_second = 1.
"
        );
        assert_eq!(
            expanded
                .included_units
                .iter()
                .map(|unit| unit.object_name.as_deref().unwrap_or(""))
                .collect::<Vec<_>>(),
            vec!["zinc_first", "zinc_second"]
        );
        assert!(
            expanded.diagnostics.is_empty(),
            "{:?}",
            expanded.diagnostics
        );
    }

    #[test]
    fn skips_repeated_include_targets_after_first_expansion() {
        let store = DocumentStore::default();
        let main_src = "\
INCLUDE: za, zb.
WRITE / lv_common.
";
        let a_src = "\
INCLUDE zcommon.
DATA lv_a TYPE i.
";
        let b_src = "\
INCLUDE zcommon.
DATA lv_b TYPE i.
";
        let common_src = "DATA lv_common TYPE i.\n";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: Some(Arc::from("main")),
            },
            DocumentInput {
                uri: Arc::from("file:///za.abap"),
                version: 1,
                text: Arc::from(a_src),
                is_dependency: false,
                object_name: Some(Arc::from("za")),
            },
            DocumentInput {
                uri: Arc::from("file:///zb.abap"),
                version: 1,
                text: Arc::from(b_src),
                is_dependency: false,
                object_name: Some(Arc::from("zb")),
            },
            DocumentInput {
                uri: Arc::from("file:///zcommon.abap"),
                version: 1,
                text: Arc::from(common_src),
                is_dependency: false,
                object_name: Some(Arc::from("zcommon")),
            },
        ]);
        let main = snapshots.get("file:///main.abap").expect("main snapshot");

        let expanded = build_effective_source(main, &snapshots);

        assert_eq!(
            expanded
                .expanded_text
                .matches("DATA lv_common TYPE i.")
                .count(),
            1
        );
        assert_eq!(
            diagnostics_of_kind(&expanded.diagnostics, "repeated_include").len(),
            1
        );
        assert_eq!(
            expanded
                .included_units
                .iter()
                .map(|unit| unit.object_name.as_deref().unwrap_or(""))
                .collect::<Vec<_>>(),
            vec!["za", "zcommon", "zb"]
        );
    }

    #[test]
    fn reports_missing_include_without_breaking_remaining_text() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE zmissing.
WRITE / 'ok'.
";
        let snapshots = store.replace_all(vec![DocumentInput {
            uri: Arc::from("file:///main.abap"),
            version: 1,
            text: Arc::from(main_src),
            is_dependency: false,
            object_name: Some(Arc::from("zmain")),
        }]);
        let main = snapshots.get("file:///main.abap").expect("main snapshot");

        let expanded = build_effective_source(main, &snapshots);
        let missing = diagnostics_of_kind(&expanded.diagnostics, "missing_include");

        assert_eq!(missing.len(), 1);
        assert_eq!(missing[0].include_name.as_deref(), Some("zmissing"));
        assert_eq!(
            expanded.expanded_text,
            "\
REPORT zmain.

WRITE / 'ok'.
"
        );
    }

    #[test]
    fn source_map_segments_match_original_slices() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE zinc.
WRITE / lv_inc.
";
        let include_src = "DATA lv_inc TYPE i.\n";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: Some(Arc::from("zmain")),
            },
            DocumentInput {
                uri: Arc::from("file:///zinc.abap"),
                version: 1,
                text: Arc::from(include_src),
                is_dependency: false,
                object_name: Some(Arc::from("zinc")),
            },
        ]);
        let main = snapshots.get("file:///main.abap").expect("main snapshot");

        let expanded = build_effective_source(main, &snapshots);
        let sources = HashMap::from([
            ("file:///main.abap", main_src),
            ("file:///zinc.abap", include_src),
        ]);

        for segment in &expanded.segments {
            let expanded_slice =
                &expanded.expanded_text[segment.expanded_range.start..segment.expanded_range.end];
            let source = sources
                .get(segment.source_unit.uri.as_str())
                .expect("source text for URI");
            let original_slice = &source[segment.source_range.start..segment.source_range.end];
            assert_eq!(expanded_slice, original_slice);
        }

        assert_eq!(expanded.segments.len(), 3);
        assert_eq!(expanded.segments[0].source_unit.uri, "file:///main.abap");
        assert_eq!(expanded.segments[1].source_unit.uri, "file:///zinc.abap");
        assert_eq!(expanded.segments[2].source_unit.uri, "file:///main.abap");
    }

    #[test]
    fn reports_include_cycles_without_recursing_forever() {
        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from("INCLUDE za.\n"),
                is_dependency: false,
                object_name: Some(Arc::from("main")),
            },
            DocumentInput {
                uri: Arc::from("file:///za.abap"),
                version: 1,
                text: Arc::from("INCLUDE zb.\n"),
                is_dependency: false,
                object_name: Some(Arc::from("za")),
            },
            DocumentInput {
                uri: Arc::from("file:///zb.abap"),
                version: 1,
                text: Arc::from("INCLUDE za.\n"),
                is_dependency: false,
                object_name: Some(Arc::from("zb")),
            },
        ]);
        let main = snapshots.get("file:///main.abap").expect("main snapshot");

        let expanded = build_effective_source(main, &snapshots);

        assert_eq!(
            diagnostics_of_kind(&expanded.diagnostics, "include_cycle").len(),
            1
        );
        assert!(
            !expanded.expanded_text.contains("INCLUDE"),
            "{}",
            expanded.expanded_text
        );
    }
}
