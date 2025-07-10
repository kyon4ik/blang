use std::ffi::OsStr;
use std::fs::{DirEntry, File};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use anstream::println;
use clap::Parser;
use comptest::runner::{ChildError, IntoTask, Runner, Task};
use comptest::status::Status;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the test or directory
    input: PathBuf,
    /// Record output of test(s)
    #[arg(long)]
    record: bool,
    /// Compiler flags
    #[arg(last = true)]
    compiler_args: Vec<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let package_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let project_dir = package_dir.parent().unwrap();
    let compiler_path = project_dir.join("target/release/blang");
    println!("Compiler: {}", compiler_path.display());

    if args.input.is_file() {
        todo!("Single file run")
    } else if args.input.is_dir() {
        for status in Status::all() {
            println!("{} - {}", status.styled(), status.description());
        }

        let tests: Vec<_> = args
            .input
            .read_dir()?
            .filter_map(Result::ok)
            .filter(is_test_file)
            .collect();

        if args.record {
            let mut runner = Runner::default();
            for test in tests.iter() {
                let test_path = test.path();
                let compile = compile_test_cmd(&test_path, &compiler_path, &args.compiler_args);
                let link = link_test_cmd(&test_path);
                let run = run_test_cmd(&test_path);

                runner.run(
                    compile
                        .into_task()
                        .then(link)
                        .then(run)
                        .map(move |out| record_output(&test_path, out)),
                );
            }

            let outputs = runner.wait();
            for (output, test_file) in outputs.into_iter().zip(tests) {
                println!(
                    "{:>30}: {}",
                    test_file.path().display(),
                    output.unwrap_or(Status::XX).styled()
                );
            }
        } else {
            let mut runner = Runner::default();
            for test in tests.iter() {
                let test_path = test.path();
                let compile = compile_test_cmd(&test_path, &compiler_path, &args.compiler_args);
                let link = link_test_cmd(&test_path);
                let run = run_test_cmd(&test_path);

                runner.run(
                    compile
                        .into_task()
                        .then(link)
                        .then(run)
                        .map(move |out| check_output(&test_path, out)),
                );
            }
            let outputs = runner.wait();
            let mut failed_count = 0;
            let test_count = tests.len();
            for (output, test_file) in outputs.into_iter().zip(tests) {
                let status = output.unwrap_or(Status::XX);
                failed_count += (status != Status::OK) as usize;
                println!("{:>30}: {}", test_file.path().display(), status.styled());
            }

            if failed_count > 0 {
                eprintln!("{failed_count} out of {test_count} tests failed");
                process::exit(1);
            }
        }
    } else {
        eprintln!("INPUT must be either file or directory");
        process::exit(1);
    }

    Ok(())
}

fn is_test_file(entry: &DirEntry) -> bool {
    let entry_type = entry.file_type().unwrap();
    let entry_path = entry.path();
    entry_type.is_file() && entry_path.extension().is_some_and(|ext| ext.eq("b"))
}

fn compile_test_cmd<S: AsRef<OsStr>>(
    test_path: &Path,
    compiler_path: &Path,
    comp_args: &[S],
) -> process::Command {
    let test_obj = test_path.with_extension("o");
    let mut compile_cmd = Command::new(compiler_path);
    compile_cmd
        .arg("-o")
        .arg(test_obj.as_path())
        .arg(test_path)
        .args(comp_args);
    compile_cmd
}

fn link_test_cmd(test_path: &Path) -> process::Command {
    let test_exe = test_path.with_extension("");
    let test_obj = test_path.with_extension("o");
    let mut link_cmd = Command::new("gcc");
    link_cmd
        .arg("-o")
        .arg(test_exe.as_path())
        .arg(test_obj.as_path());
    link_cmd
}

fn run_test_cmd(test_path: &Path) -> process::Command {
    let test_exe_path = test_path.with_extension("");
    Command::new(test_exe_path)
}

fn check_output(test_path: &Path, output: Result<Vec<u8>, ChildError>) -> Status {
    let expect_path = test_path.with_extension("output");
    let mut expect_file = BufReader::new(File::open(expect_path).expect("Opening output file"));

    let mut status_str = String::new();
    expect_file.read_line(&mut status_str).unwrap();
    let expected_status = status_str
        .trim_end_matches('\n')
        .parse::<Status>()
        .expect("Parsing status");
    let mut expected_output = Vec::new();
    expect_file.read_to_end(&mut expected_output).unwrap();
    let (status, output) = match output {
        Ok(stdout) => (Status::OK, stdout),
        Err(err) => match err {
            ChildError::Fail { stderr, .. } => (Status::CF, stderr),
            ChildError::Io(_) => (Status::XX, vec![]),
        },
    };
    if status == expected_status && output == expected_output {
        Status::OK
    } else {
        status
    }
}

fn record_output(test_path: &Path, output: Result<Vec<u8>, ChildError>) -> Status {
    let output_path = test_path.with_extension("output");
    let mut output_file = File::create(output_path).expect("Creating output file");

    match output {
        Ok(stdout) => {
            writeln!(output_file, "{}", Status::OK).unwrap();
            output_file
                .write_all(&stdout)
                .expect("Writing to output file");
            Status::OK
        }
        Err(err) => match err {
            ChildError::Fail { stderr, .. } => {
                writeln!(output_file, "{}", Status::CF).unwrap();
                output_file
                    .write_all(&stderr)
                    .expect("Writing to output file");
                Status::CF
            }
            ChildError::Io(e) => {
                eprintln!("IO error on test {}: {}", test_path.display(), e);
                Status::XX
            }
        },
    }
}
