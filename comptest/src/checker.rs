use std::path::Path;
use std::{fmt, fs};

use anstyle::{AnsiColor, Color, Style};

use crate::runner::{RunStatus, TestOutput};

#[derive(Clone, Debug)]
pub enum TestResult {
    Ok,
    Fail(FailReason),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TestStatus {
    Ok,
    Fail(FailReasonKind),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum FailReasonKind {
    WrongStatus,
    WrongStdout,
    WrongStderr,
    #[default]
    Unspecified,
}

#[derive(Clone, Debug, Default)]
pub enum FailReason {
    WrongStatus {
        expected: RunStatus,
        found: RunStatus,
        stderr: Vec<u8>,
    },
    WrongStdout {
        expected: Vec<u8>,
        found: Vec<u8>,
    },
    WrongStderr {
        expected: Vec<u8>,
        found: Vec<u8>,
    },
    #[default]
    Unspecified,
}

pub fn mapper_short(test_path: &Path, output: TestOutput) -> TestStatus {
    let expected_path = test_path.with_extension("output");
    let expected_output = TestOutput::from_bytes(&fs::read(expected_path).unwrap()).unwrap();
    if output.run_status != expected_output.run_status {
        TestStatus::Fail(FailReasonKind::WrongStatus)
    } else if output.stdout != expected_output.stdout {
        TestStatus::Fail(FailReasonKind::WrongStdout)
    } else if output.stderr != expected_output.stderr {
        TestStatus::Fail(FailReasonKind::WrongStderr)
    } else {
        TestStatus::Ok
    }
}

pub fn mapper_long(test_path: &Path, output: TestOutput) -> TestResult {
    let expected_path = test_path.with_extension("output");
    let expected_output =
        TestOutput::from_bytes(&fs::read(expected_path).unwrap()).expect("Parsing output file");
    // TODO: use difference function here
    if output.run_status != expected_output.run_status {
        TestResult::Fail(FailReason::WrongStatus {
            expected: expected_output.run_status,
            found: output.run_status,
            stderr: output.stderr,
        })
    } else if output.stdout != expected_output.stdout {
        TestResult::Fail(FailReason::WrongStdout {
            expected: expected_output.stdout,
            found: output.stdout,
        })
    } else if output.stderr != expected_output.stderr {
        TestResult::Fail(FailReason::WrongStderr {
            expected: expected_output.stderr,
            found: output.stderr,
        })
    } else {
        TestResult::Ok
    }
}

impl Default for TestResult {
    fn default() -> Self {
        Self::Fail(Default::default())
    }
}

impl Default for TestStatus {
    fn default() -> Self {
        Self::Fail(Default::default())
    }
}

impl TestStatus {
    pub fn style(&self) -> Style {
        let ansi = match self {
            Self::Ok => AnsiColor::Green,
            Self::Fail(_) => AnsiColor::Red,
        };
        anstyle::Style::new().fg_color(Some(Color::Ansi(ansi)))
    }
}

impl TestResult {
    pub fn status(&self) -> TestStatus {
        match self {
            Self::Ok => TestStatus::Ok,
            Self::Fail(fail_reason) => TestStatus::Fail(fail_reason.kind()),
        }
    }
}

impl FailReason {
    pub fn kind(&self) -> FailReasonKind {
        match self {
            FailReason::WrongStatus { .. } => FailReasonKind::WrongStatus,
            FailReason::WrongStdout { .. } => FailReasonKind::WrongStdout,
            FailReason::WrongStderr { .. } => FailReasonKind::WrongStderr,
            FailReason::Unspecified => FailReasonKind::Unspecified,
        }
    }
}

impl fmt::Display for TestStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ok => write!(f, "{}ok{}", self.style(), anstyle::Reset),
            Self::Fail(reason) => write!(f, "{}fail ({reason}){}", self.style(), anstyle::Reset),
        }
    }
}

impl fmt::Display for FailReasonKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            FailReasonKind::WrongStatus => "wrong status",
            FailReasonKind::WrongStdout => "wrong stdout",
            FailReasonKind::WrongStderr => "wrong stderr",
            FailReasonKind::Unspecified => "unspecified",
        };
        f.write_str(str)
    }
}

impl fmt::Display for FailReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{}{}{}",
            Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow))),
            self.kind(),
            anstyle::Reset
        )?;
        match self {
            Self::WrongStatus {
                expected,
                found,
                stderr,
            } => {
                let stderr_utf = String::from_utf8_lossy(stderr);
                writeln!(f, "Expected {expected}, found {found}")?;
                write!(f, "Stderr:\n{stderr_utf}")
            }
            Self::WrongStdout { expected, found } => {
                let expected_utf = String::from_utf8_lossy(expected);
                let found_utf = String::from_utf8_lossy(found);
                write!(
                    f,
                    "Expected:\n{}\nFound:\n{}",
                    expected_utf.escape_default(),
                    found_utf.escape_default()
                )
            }
            Self::WrongStderr { expected, found } => {
                let expected_utf = String::from_utf8_lossy(expected);
                let found_utf = String::from_utf8_lossy(found);
                write!(
                    f,
                    "Expected:\n{}\nFound:\n{}",
                    expected_utf.escape_default(),
                    found_utf.escape_default()
                )
            }
            Self::Unspecified => write!(f, "Probably a runner bug!"),
        }
    }
}
