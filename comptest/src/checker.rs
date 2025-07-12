use std::path::Path;
use std::{fmt, fs};

use anstyle::{AnsiColor, Color, Style};

use crate::runner::{RunStatus, TestOutput};

#[derive(Clone, Debug)]
pub enum TestResult {
    Ok,
    Fail(FailReason),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum TestStatus {
    Ok,
    #[default]
    Fail,
}

#[derive(Clone, Debug, Default)]
pub enum FailReason {
    WrongStatus {
        expected: RunStatus,
        found: RunStatus,
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
    if output == expected_output {
        TestStatus::Ok
    } else {
        TestStatus::Fail
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

impl TestStatus {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Ok => "ok",
            Self::Fail => "fail",
        }
    }

    pub fn style(&self) -> Style {
        let ansi = match self {
            Self::Ok => AnsiColor::Green,
            Self::Fail => AnsiColor::Red,
        };
        anstyle::Style::new().fg_color(Some(Color::Ansi(ansi)))
    }
}

impl TestResult {
    pub fn status(&self) -> TestStatus {
        match self {
            Self::Ok => TestStatus::Ok,
            Self::Fail(_) => TestStatus::Fail,
        }
    }
}

impl fmt::Display for TestStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.style(), self.as_str(), anstyle::Reset)
    }
}

impl fmt::Display for FailReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WrongStatus { expected, found } => {
                write!(f, "Wrong status:\nExpected {expected}, found {found}")
            }
            Self::WrongStdout { expected, found } => {
                let expected_utf = String::from_utf8_lossy(expected);
                let found_utf = String::from_utf8_lossy(found);
                write!(
                    f,
                    "Wrong stdout:\nExpected:\n{}\nFound:\n{}",
                    expected_utf.escape_default(),
                    found_utf.escape_default()
                )
            }
            Self::WrongStderr { expected, found } => {
                let expected_utf = String::from_utf8_lossy(expected);
                let found_utf = String::from_utf8_lossy(found);
                write!(
                    f,
                    "Wrong stderr:\nExpected:\n{}\nFound:\n{}",
                    expected_utf.escape_default(),
                    found_utf.escape_default()
                )
            }
            Self::Unspecified => write!(f, "Probably a runner bug!"),
        }
    }
}
