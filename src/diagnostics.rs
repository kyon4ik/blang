use std::cell::{RefCell, RefMut};
use std::convert::Infallible;
use std::num::NonZeroU8;
use std::path::Path;
use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
pub struct DiagConfig {
    pub max_errors: Option<NonZeroU8>,
}

#[derive(Clone)]
pub struct SourceMap {
    utf_src: Rc<ariadne::Source<String>>,
    path: Rc<Path>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FullSpan {
    pub path: Rc<Path>,
    pub start: u32,
    pub end: u32,
}

pub struct Diagnostics {
    config: DiagConfig,
    report_config: ariadne::Config,
    source_map: SourceMap,
    errors: RefCell<Vec<ariadne::Report<'static, FullSpan>>>,
}

pub struct ReportBuilder<'a> {
    source_map: &'a SourceMap,
    builder: ariadne::ReportBuilder<'static, FullSpan>,
    reports: RefMut<'a, Vec<ariadne::Report<'static, FullSpan>>>,
    config: &'a DiagConfig,
}

impl ReportBuilder<'_> {
    #[must_use]
    pub fn add_label(mut self, span: Span, message: impl ToString) -> Self {
        let full_span = self.source_map.full_span(span);
        let label = ariadne::Label::new(full_span)
            .with_message(message)
            .with_color(ariadne::Color::Blue);
        self.builder.add_label(label);
        self
    }

    pub fn finish(mut self) {
        if self.reports.len() < self.config.max_errors() {
            self.reports.push(self.builder.finish());
        }
    }
}

impl Diagnostics {
    pub fn new(config: DiagConfig, source_map: SourceMap) -> Self {
        let report_config = ariadne::Config::new()
            .with_cross_gap(true)
            .with_underlines(true)
            .with_multiline_arrows(true)
            .with_color(true)
            .with_index_type(ariadne::IndexType::Byte);

        let max_errors = config.max_errors.map(NonZeroU8::get).unwrap_or(0) as usize;
        let errors = RefCell::new(Vec::with_capacity(max_errors));

        Self {
            config,
            report_config,
            source_map,
            errors,
        }
    }

    #[must_use]
    pub fn error(&self, span: Span, message: impl ToString) -> ReportBuilder {
        let reports = self.errors.borrow_mut();
        let full_span = self.source_map.full_span(span);
        let builder = ariadne::Report::build(ariadne::ReportKind::Error, full_span.clone())
            .with_config(self.report_config)
            .with_message(message)
            .with_label(ariadne::Label::new(full_span).with_color(ariadne::Color::Magenta));
        ReportBuilder {
            source_map: &self.source_map,
            builder,
            config: &self.config,
            reports,
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }

    pub fn print_errors(&self) -> std::io::Result<()> {
        for error in self.errors.borrow().iter() {
            error.print(&self.source_map)?;
        }
        Ok(())
    }
}

impl SourceMap {
    pub fn new(src: &[u8], path: &Path) -> Self {
        let utf_src = ariadne::Source::from(String::from_utf8_lossy(src).into_owned());

        Self {
            utf_src: Rc::new(utf_src),
            path: Rc::from(path),
        }
    }

    pub fn full_span(&self, span: Span) -> FullSpan {
        FullSpan {
            path: self.path.clone(),
            start: span.start,
            end: span.end,
        }
    }
}

impl ariadne::Cache<Path> for &SourceMap {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &Path,
    ) -> Result<&ariadne::Source<Self::Storage>, impl std::fmt::Debug> {
        debug_assert_eq!(id, self.path.as_ref());
        Result::<_, Infallible>::Ok(self.utf_src.as_ref())
    }

    fn display<'a>(&self, id: &'a Path) -> Option<impl std::fmt::Display + 'a> {
        Some(id.display())
    }
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        assert!(start <= end);
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self::new(0, 0)
    }
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        Span::new(value.0 as u32, value.1 as u32)
    }
}

impl ariadne::Span for FullSpan {
    type SourceId = Path;

    #[inline]
    fn source(&self) -> &Self::SourceId {
        self.path.as_ref()
    }

    #[inline]
    fn start(&self) -> usize {
        self.start as usize
    }

    #[inline]
    fn end(&self) -> usize {
        self.end as usize
    }
}

impl DiagConfig {
    pub fn max_errors(&self) -> usize {
        self.max_errors
            .map(|me| me.get() as usize)
            .unwrap_or(usize::MAX)
    }
}

impl Default for DiagConfig {
    fn default() -> Self {
        Self {
            max_errors: NonZeroU8::new(1),
        }
    }
}
