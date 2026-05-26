use std::fs;
use std::path::PathBuf;

use abap_parser::parse;

#[test]
fn parses_workspace_examples() {
    let examples_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples");
    for entry in fs::read_dir(&examples_dir).expect("read examples dir") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) != Some("abap") {
            continue;
        }
        let name = path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("<unknown>");
        let src = fs::read_to_string(&path).expect("read example");
        let parsed = parse(&src);
        if name.starts_with("negative_") {
            assert!(
                !parsed.errors.is_empty(),
                "expected parse errors for negative example {name}"
            );
        } else {
            assert!(
                parsed.errors.is_empty(),
                "expected no parse errors for {name}, got {:?}",
                parsed.errors
            );
        }
    }
}
