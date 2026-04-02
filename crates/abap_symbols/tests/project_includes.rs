use abap_parser::parse;

use abap_symbols::{DiagnosticKind, ProjectInput, Resolution, analyze_project};

#[test]
fn resolves_symbols_from_included_units() {
    let root_src = "INCLUDE zinc. lv_inc = 1.";
    let include_src = "DATA lv_inc TYPE i.";
    let root_parse = parse(root_src);
    let include_parse = parse(include_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zinc.abap",
            source: include_src,
            parse: &include_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(root.include_edges.iter().any(|edge| edge.target.is_some()));
    assert!(root.references.iter().any(|reference| {
        reference.name.as_ref() == "lv_inc" && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn reports_unresolved_include_targets() {
    let root_src = "INCLUDE zmissing. lv_inc = 1.";
    let root_parse = parse(root_src);
    let project = analyze_project(&[ProjectInput {
        uri: "zmain.abap",
        source: root_src,
        parse: &root_parse,
    }]);

    assert!(project
        .diagnostics
        .iter()
        .any(|diag| diag.kind == DiagnosticKind::UnresolvedInclude));
}
