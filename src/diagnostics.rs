use std::borrow::Cow;
use std::fmt;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use bstr::{BStr, ByteSlice};

#[derive(Debug)]
pub struct Diagnostics {
    src: Rc<[u8]>,
    errors: Vec<DiagError>,
    src_map: SourceMap,
}

#[derive(Clone, Debug)]
pub struct DiagError {
    kind: DiagErrorKind,
    span: Span,
}

#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum DiagErrorKind {
    Unexpected {
        ty: Cow<'static, str>,
        expected: Cow<'static, str>,
        found: Cow<'static, str>,
    },
    Other(Cow<'static, str>),
}

impl Diagnostics {
    pub fn new(src: &[u8], path: &Path, max_errors: usize) -> Self {
        assert!(max_errors > 0);

        let errors = Vec::with_capacity(max_errors);
        let src: Rc<[u8]> = Rc::from(src);
        let src_map = SourceMap::new(src.clone(), path);

        Self {
            src,
            errors,
            src_map,
        }
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.src_map
    }

    pub fn error(&mut self, kind: DiagErrorKind, span: Span) {
        // max errors reached
        if self.errors.spare_capacity_mut().is_empty() {
            return;
        }

        self.errors.push(DiagError::new(kind, span));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    // TODO: better diagnostics
    pub fn print_errors(&self) {
        for error in &self.errors {
            let (start_loc, _end_loc) = self.src_map.locate_span(error.span).unwrap();

            use DiagErrorKind::*;
            print!(
                "{}:{}:{} | [ERROR] ",
                self.src_map.file_path().display(),
                start_loc.line,
                start_loc.column
            );
            match &error.kind {
                Unexpected {
                    ty,
                    expected,
                    found,
                } => println!("Expected {ty} {expected}, found {found}"),
                Other(msg) => println!("{msg}"),
            }
            println!(
                "HERE -> {}",
                BStr::new(&self.src[error.span.start..error.span.end])
            );
        }
    }
}

impl DiagError {
    pub fn new(kind: DiagErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl DiagErrorKind {
    pub fn unexpected<I, J, K>(ty: I, expected: J, found: K) -> Self
    where
        I: Into<Cow<'static, str>>,
        J: Into<Cow<'static, str>>,
        K: Into<Cow<'static, str>>,
    {
        Self::Unexpected {
            ty: ty.into(),
            expected: expected.into(),
            found: found.into(),
        }
    }

    pub fn other<I>(msg: I) -> Self
    where
        I: Into<Cow<'static, str>>,
    {
        Self::Other(msg.into())
    }
}

#[derive(Debug)]
pub struct SourceMap {
    src: Rc<[u8]>,
    path: PathBuf,
    line_starts: Vec<usize>,
}

#[derive(Clone, Copy, Debug)]
pub struct SymLoc {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for SymLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl SourceMap {
    pub fn new(src: Rc<[u8]>, path: &Path) -> Self {
        let mut line_starts = Vec::new();

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
