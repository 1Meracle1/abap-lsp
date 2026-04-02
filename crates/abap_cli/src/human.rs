//! rustc-style human-readable diagnostics (stderr + optional stdout summaries).

use std::io::{self, IsTerminal, Write};
use std::ops::Range;

pub struct Diagnostic<'a> {
    pub message: &'a str,
    pub range: Range<usize>,
}

struct LineSpan {
    /// 1-based line number.
    line_nr: usize,
    /// Byte range of line text (excluding `\n`).
    text_range: Range<usize>,
}

fn collect_lines(source: &str) -> Vec<LineSpan> {
    let mut out = Vec::new();
    let mut line_start = 0usize;
    let mut line_nr = 1usize;
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            out.push(LineSpan {
                line_nr,
                text_range: line_start..i,
            });
            line_nr += 1;
            line_start = i + ch.len_utf8();
        }
    }
    out.push(LineSpan {
        line_nr,
        text_range: line_start..source.len(),
    });
    out
}

fn byte_col_1based(source: &str, line_start: usize, byte: usize) -> usize {
    let byte = byte.max(line_start).min(source.len());
    source[line_start..byte].chars().count() + 1
}

fn gutter_width(last_line: usize) -> usize {
    ((last_line as f64).log10().floor() as usize + 1).max(1)
}

fn color_stderr(color: bool, code: &str, s: &str) -> String {
    if color {
        format!("\x1b[{code}m{s}\x1b[0m")
    } else {
        s.to_string()
    }
}

fn stderr_color_enabled() -> bool {
    io::stderr().is_terminal() && std::env::var_os("NO_COLOR").is_none()
}

/// Writes rustc-style blocks to stderr. Returns true if anything was written.
pub fn write_diagnostics(
    diagnostics: &[Diagnostic<'_>],
    source: &str,
    file_label: &str,
) -> io::Result<bool> {
    if diagnostics.is_empty() {
        return Ok(false);
    }

    let color = stderr_color_enabled();
    let err_lbl = color_stderr(color, "1;31", "error");
    let lines = collect_lines(source);
    let width = gutter_width(lines.last().map(|l| l.line_nr).unwrap_or(1));

    let mut stderr = io::stderr().lock();
    for d in diagnostics {
        writeln!(stderr, "{err_lbl}: {}", d.message, err_lbl = err_lbl)?;

        let (line, col) = line_col_for_byte(source, d.range.start);
        writeln!(
            stderr,
            "{:>width$} --> {file}:{line}:{col}",
            "",
            width = width,
            file = file_label
        )?;

        let affected: Vec<&LineSpan> = lines
            .iter()
            .filter(|ln| {
                let tr = &ln.text_range;
                d.range.start < tr.end && d.range.end > tr.start
            })
            .collect();

        if affected.is_empty() {
            writeln!(stderr, "{:>width$} |", "", width = width)?;
            continue;
        }

        writeln!(stderr, "{:>width$} |", "", width = width)?;

        for ln in affected {
            let line_text = source.get(ln.text_range.clone()).unwrap_or("");
            writeln!(
                stderr,
                "{:>width$} | {}",
                ln.line_nr,
                line_text,
                width = width,
            )?;

            let seg_start = d.range.start.max(ln.text_range.start);
            let seg_end = d.range.end.min(ln.text_range.end);
            let pad = byte_col_1based(source, ln.text_range.start, seg_start);
            let tick_chars = source
                .get(seg_start..seg_end)
                .map(|s| s.chars().count())
                .unwrap_or(1)
                .max(1);

            let underline: String = std::iter::repeat(' ')
                .take(pad.saturating_sub(1))
                .chain(std::iter::repeat('^').take(tick_chars))
                .collect();
            let line_mark = color_stderr(color, "1;32", &underline);
            writeln!(stderr, "{:>width$} | {}", "", line_mark, width = width)?;
        }

        writeln!(stderr, "{:>width$} |", "", width = width)?;
    }

    Ok(true)
}

fn line_col_for_byte(source: &str, byte_idx: usize) -> (usize, usize) {
    let byte_idx = byte_idx.min(source.len());
    let mut line = 1usize;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if i >= byte_idx {
            let col = byte_col_1based(source, line_start, byte_idx);
            return (line, col);
        }
        if ch == '\n' {
            line += 1;
            line_start = i + ch.len_utf8();
        }
    }
    let col = byte_col_1based(source, line_start, byte_idx);
    (line, col)
}

pub fn write_token_list(
    source: &str,
    tokens: &[abap_lexer::Token],
    use_color: bool,
) -> io::Result<()> {
    let mut out = io::stdout().lock();
    let hdr = color_if_stdout(use_color, "1", &format!("{} tokens:", tokens.len()));
    writeln!(out, "{hdr}")?;
    for t in tokens {
        let kind = format!("{:?}", t.kind);
        let lex = t.lexeme(source);
        let preview = if lex.len() > 60 {
            format!("{}…", &lex[..60])
        } else {
            lex.to_string()
        };
        writeln!(
            out,
            "  {:18} {:>6}..{:<6}  {:?}",
            kind, t.range.start, t.range.end, preview,
        )?;
    }
    Ok(())
}

fn color_if_stdout(color: bool, code: &str, s: &str) -> String {
    if color {
        format!("\x1b[{code}m{s}\x1b[0m")
    } else {
        s.to_string()
    }
}

pub fn stdout_color_enabled() -> bool {
    io::stdout().is_terminal() && std::env::var_os("NO_COLOR").is_none()
}

pub fn write_symbols_table(
    names: &[(String, &str, Range<usize>)],
    use_color: bool,
) -> io::Result<()> {
    let mut out = io::stdout().lock();
    let hdr = color_if_stdout(use_color, "1", &format!("{} symbols:", names.len()));
    writeln!(out, "{hdr}")?;
    for (name, kind, range) in names {
        writeln!(
            out,
            "  {:12}  {:>6}..{:<6}  {}",
            kind, range.start, range.end, name
        )?;
    }
    Ok(())
}
