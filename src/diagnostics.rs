use std::path::{Path, PathBuf};

use bstr::{BStr, ByteSlice};

#[derive(Debug)]
pub struct SourceMap<'s> {
    src: &'s BStr,
    path: PathBuf,
    line_starts: Vec<usize>,
}

#[derive(Clone, Copy, Debug)]
pub struct SymLoc {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl<'s> SourceMap<'s> {
    pub fn new(src: &'s [u8], path: &Path) -> Self {
        let mut line_starts = Vec::new();
        let src = BStr::new(src);

        let mut bytes_read = 0;
        for line in src.lines_with_terminator() {
            line_starts.push(bytes_read);
            bytes_read += line.len();
        }

        line_starts.push(bytes_read);

        debug_assert!(line_starts.is_sorted());

        Self {
            src,
            path: PathBuf::from(path),
            line_starts,
        }
    }

    pub fn locate(&self, pos: usize) -> Option<SymLoc> {
        let next_line = self
            .line_starts
            .partition_point(|line_start| *line_start <= pos);

        self.line_starts.get(next_line).map(|_| {
            let line_start = self.line_starts[next_line - 1];
            let column = self.src[line_start..pos].len() + 1;
            // NOTE: uses next_line, because lines start from 1
            SymLoc::new(next_line, column)
        })
    }

    pub fn locate_span(&self, span: Span) -> Option<(SymLoc, SymLoc)> {
        match (self.locate(span.start), self.locate(span.end)) {
            (Some(start), Some(end)) => Some((start, end)),
            _ => None,
        }
    }

    pub fn file_path(&self) -> &Path {
        &self.path
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start <= end);
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self::new(0, 0)
    }
}

impl SymLoc {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}
