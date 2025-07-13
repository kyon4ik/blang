use std::ffi::OsStr;
use std::fs::{self, DirEntry};
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use anstyle::{AnsiColor, Color, Style};
use clap::Parser;
use comptest::checker::{TestResult, TestStatus};
use comptest::runner::{RunStatus, Test, TestRunner, TestStage, TestStager};
use comptest::{checker, recorder};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the test file or directory
    tests: PathBuf,
    /// Record output of test(s)
    #[arg(long)]
    record: bool,
    /// Print more info for multiple tests
    #[arg(long)]
    verbose: bool,
    /// Compiler flags
    #[arg(last = true)]
    compiler_args: Vec<String>,
}

fn main() {
    let args = Args::parse();

    let package_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let project_dir = package_dir.parent().unwrap();
    let compiler_path = project_dir.join("target/debug/blang");

    let test_suite_path = &args.tests;
    let test_suite_meta = fs::metadata(test_suite_path).unwrap();
    let test_suite_type = test_suite_meta.file_type();

    if test_suite_type.is_file() {
        let stp = LoudStager::new(&compiler_path, &args.compiler_args);
        if args.record {
            let mut runner = TestRunner::new(stp, recorder::mapper_long);
            let output = runner.run(Test::new(test_suite_path));
            println!("{output}");
        } else {
            let mut runner = TestRunner::new(stp, checker::mapper_long);
            let result = runner.run(Test::new(test_suite_path));
            print_test_result(test_suite_path, &result);
            if result.status() != TestStatus::Ok {
                process::exit(1);
            }
        }
    } else if test_suite_type.is_dir() {
        let stp = QuietStager::new(&compiler_path, &args.compiler_args);
        let tests: Vec<PathBuf> = test_suite_path
            .read_dir()
            .unwrap()
            .filter_map(Result::ok)
            .filter(is_b_src_file)
            .map(|entry| entry.path())
            .collect();

        if args.record {
            let mut runner = TestRunner::new(stp, recorder::mapper_short);
            for test_path in &tests {
                runner.submit(Test::new(test_path));
            }
            let run_statuses = runner.wait();
            for (test_path, run_status) in tests.iter().zip(run_statuses) {
                print_run_status(test_path, run_status);
            }
        } else if args.verbose {
            let mut runner = TestRunner::new(stp, checker::mapper_long);
            for test_path in &tests {
                runner.submit(Test::new(test_path));
            }
            let results = runner.wait();
            let tests_count = tests.len();
            let mut failed_tests_count = 0;
            for (test_path, result) in tests.iter().zip(results) {
                print_test_result(test_path, &result);
                if matches!(result, TestResult::Fail(_)) {
                    failed_tests_count += 1;
                }
            }
            if failed_tests_count > 0 {
                println!("{failed_tests_count} out of {tests_count} tests failed.");
                process::exit(1);
            }
        } else {
            let mut runner = TestRunner::new(stp, checker::mapper_short);
            for test_path in &tests {
                runner.submit(Test::new(test_path));
            }
            let statuses = runner.wait();
            let tests_count = tests.len();
            let mut failed_tests_count = 0;
            for (test_path, status) in tests.iter().zip(statuses) {
                print_test_status(test_path, status);
                if matches!(status, TestStatus::Fail(_)) {
                    failed_tests_count += 1;
                }
            }
            if failed_tests_count > 0 {
                println!("{failed_tests_count} out of {tests_count} tests failed.");
                process::exit(1);
            }
        }
    } else {
        println!("'{}' is not a file or directory", test_suite_path.display());
        process::exit(1);
    }
}

struct QuietStager<'c, 'a> {
    compiler_path: &'c Path,
    compiler_args: &'a [String],
}

struct LoudStager<'c, 'a> {
    compiler_path: &'c Path,
    compiler_args: &'a [String],
}

impl<'c, 'a> QuietStager<'c, 'a> {
    pub fn new(compiler_path: &'c Path, compiler_args: &'a [String]) -> Self {
        Self {
            compiler_path,
            compiler_args,
        }
    }
}
impl<'c, 'a> LoudStager<'c, 'a> {
    pub fn new(compiler_path: &'c Path, compiler_args: &'a [String]) -> Self {
        Self {
            compiler_path,
            compiler_args,
        }
    }
}

impl TestStager for QuietStager<'_, '_> {
    fn create(&self, stage: TestStage, test_path: &Path) -> Command {
        match stage {
            TestStage::Compiling => compile_cmd(test_path, self.compiler_path, self.compiler_args),
            TestStage::Running => Command::new(test_path.with_extension("")),
        }
    }
}

impl TestStager for LoudStager<'_, '_> {
    fn create(&self, stage: TestStage, test_path: &Path) -> Command {
        println!("-- Stage {stage} --");
        match stage {
            TestStage::Compiling => {
                let cmd = compile_cmd(test_path, self.compiler_path, self.compiler_args);
                print_command(&cmd);
                cmd
            }
            TestStage::Running => {
                let cmd = Command::new(test_path.with_extension(""));
                print_command(&cmd);
                cmd
            }
        }
    }
}

fn is_b_src_file(entry: &DirEntry) -> bool {
    let entry_type = entry.file_type().unwrap();
    let entry_path = entry.path();
    entry_type.is_file() && entry_path.extension().is_some_and(|ext| ext.eq("b"))
}

fn print_command(cmd: &Command) {
    print!(
        "{}Running{} {}",
        Style::new().fg_color(Some(Color::Ansi(AnsiColor::Magenta))),
        anstyle::Reset,
        cmd.get_program().display()
    );
    for arg in cmd.get_args() {
        print!(" {}", arg.display());
    }
    println!();
}

fn print_test_result(test_path: &Path, result: &TestResult) {
    print_test_status(test_path, result.status());
    match result {
        TestResult::Ok => {}
        TestResult::Fail(fail_reason) => println!("Test failed due to: {fail_reason}"),
    }
}

fn print_run_status(test_path: &Path, run_status: RunStatus) {
    println!("{} - {}", test_path.display(), run_status);
}

fn print_test_status(test_path: &Path, status: TestStatus) {
    println!("{} - {}", test_path.display(), status);
}

fn compile_cmd<S: AsRef<OsStr>>(
    test_path: &Path,
    compiler_path: &Path,
    comp_args: &[S],
) -> process::Command {
    let test_exe_path = test_path.with_extension("");
    let mut compile_cmd = Command::new(compiler_path);
    compile_cmd
        .arg("-o")
        .arg(test_exe_path)
        .arg(test_path)
        .args(comp_args);
    compile_cmd
}
