use std::fs;
use std::path::Path;

use crate::runner::{RunStatus, TestOutput};

pub fn mapper_short(test_path: &Path, output: TestOutput) -> RunStatus {
    let expected_path = test_path.with_extension("output");
    fs::write(expected_path, output.into_bytes()).unwrap();
    output.run_status
}

pub fn mapper_long(test_path: &Path, output: TestOutput) -> TestOutput {
    let expected_path = test_path.with_extension("output");
    fs::write(expected_path, output.into_bytes()).unwrap();
    output
}
