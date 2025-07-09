use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the test or directory
    input: PathBuf,
    /// Compiler flags
    #[arg(last = true)]
    compiler_args: Vec<String>,
}

fn main() {
    let args = Args::parse();

    let compiler_path = build_compiler();

    if args.input.is_file() {
        let test_exe_path =
            match compile_test_sync(&args.input, &compiler_path, &args.compiler_args) {
                Ok(exe) => exe,
                Err(Phase::Compilataion(output)) => {
                    eprintln!("Test compilation failed [{}]:", output.status);
                    eprintln!(
                        "{}",
                        String::from_utf8(output.stderr)
                            .unwrap_or_else(|_| "Non UTF-8 output found.".to_string())
                    );
                    process::exit(1);
                }
                Err(Phase::Linking(output)) => {
                    eprintln!("Test linking failed [{}]:", output.status);
                    eprintln!(
                        "{}",
                        String::from_utf8(output.stdout)
                            .unwrap_or_else(|_| "Non UTF-8 output found.".to_string())
                    );
                    process::exit(1);
                }
            };
        run_test(&test_exe_path);
    } else if args.input.is_dir() {
        let mut compiling = ProcessBarrier::default();
        let mut linking = ProcessBarrier::default();
        let mut tests = Vec::new();
        for entry in args.input.read_dir().expect("Reading directory") {
            let entry = entry.expect("Reading dir entry");
            let entry_type = entry.file_type().unwrap();
            let entry_path = entry.path();

            if entry_type.is_file() && entry_path.extension().is_some_and(|ext| ext.eq("b")) {
                let proc = compile_test_async(&entry_path, &compiler_path, &args.compiler_args);
                compiling.add(proc);
                tests.push(entry);
            } else if entry_type.is_dir() {
                println!("skipping directory {}", entry_path.display());
            }
        }
        let comp_statuses = compiling.wait();
        let mut compiled_tests = Vec::new();
        for (status, entry) in comp_statuses.into_iter().zip(tests) {
            if let Some(status) = status {
                if status.success() {
                    let proc = link_test_async(&entry.path());
                    linking.add(proc);
                    compiled_tests.push(entry);
                } else {
                    println!("{:>30}: CF", entry.file_name().display());
                }
            } else {
                println!("{:>30}: XX", entry.file_name().display());
            }
        }
        let link_statuses = linking.wait();
        for (status, entry) in link_statuses.into_iter().zip(compiled_tests) {
            if let Some(status) = status {
                if status.success() {
                    println!("{:>30}: OK", entry.file_name().display());
                } else {
                    println!("{:>30}: LF", entry.file_name().display());
                }
            } else {
                println!("{:>30}: XX", entry.file_name().display());
            }
        }

        println!("OK - success");
        println!("CF - compilation failed");
        println!("LF - linking failed");
        println!("XX - system error")
    } else {
        eprintln!("INPUT must be either file or directory");
        process::exit(1);
    }
}

#[derive(Default)]
struct ProcessBarrier {
    statuses: Vec<Option<process::ExitStatus>>,
    children: Vec<(process::Child, usize)>,
    counter: usize,
}

impl ProcessBarrier {
    fn add(&mut self, child: process::Child) -> usize {
        let child_id = self.counter;
        self.children.push((child, child_id));
        self.counter += 1;
        child_id
    }

    fn wait(mut self) -> Vec<Option<process::ExitStatus>> {
        self.statuses.resize(self.counter, None);
        loop {
            if self.children.is_empty() {
                break;
            }

            self.children
                .retain_mut(|(child, id)| match child.try_wait() {
                    Ok(Some(status)) => {
                        self.statuses[*id] = Some(status);
                        false
                    }
                    Ok(None) => true,
                    Err(_) => false,
                });
        }

        self.statuses
    }
}

fn build_compiler() -> PathBuf {
    let project_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let project_name = env!("CARGO_PKG_NAME");

    let building_status = Command::new("cargo")
        .args(["build", "--release"])
        .status()
        .expect("Building compiler");

    if building_status.success() {
        println!("Build compiler successfully.");
    } else {
        eprintln!("Failed to build compiler with status [{building_status}]");
        process::exit(1);
    }

    project_dir.join(format!("target/release/{project_name}"))
}

enum Phase {
    Compilataion(process::Output),
    Linking(process::Output),
}

fn compile_test_sync<S: AsRef<OsStr>>(
    test_path: &Path,
    compiler_path: &Path,
    comp_args: &[S],
) -> Result<PathBuf, Phase> {
    let comp_res = compile_test_async(test_path, compiler_path, comp_args)
        .wait_with_output()
        .expect("Waiting compilation");
    if !comp_res.status.success() {
        return Err(Phase::Compilataion(comp_res));
    }

    let link_res = link_test_async(test_path)
        .wait_with_output()
        .expect("Waiting linking");
    if !link_res.status.success() {
        return Err(Phase::Linking(link_res));
    }

    let test_exe = test_path.with_extension("");
    Ok(test_exe)
}

fn compile_test_async<S: AsRef<OsStr>>(
    test_path: &Path,
    compiler_path: &Path,
    comp_args: &[S],
) -> process::Child {
    let test_obj = test_path.with_extension("o");
    let mut compile_cmd = Command::new(compiler_path);
    compile_cmd
        .arg("-o")
        .arg(test_obj.as_path())
        .arg(test_path)
        .args(comp_args)
        .stdout(process::Stdio::null())
        .stderr(process::Stdio::null());

    compile_cmd.spawn().expect("Compiling test")
}

fn link_test_async(test_path: &Path) -> process::Child {
    let test_exe = test_path.with_extension("");
    let test_obj = test_path.with_extension("o");
    let mut link_cmd = Command::new("gcc");
    link_cmd
        .arg("-o")
        .arg(test_exe.as_path())
        .arg(test_obj.as_path())
        .stderr(process::Stdio::null());

    link_cmd.spawn().expect("Linking test")
}

fn run_test(test_exe_path: &Path) {
    let test_res = Command::new(test_exe_path).output().expect("Running test");
    println!("Test stdout:");
    println!(
        "{}",
        String::from_utf8(test_res.stdout)
            .unwrap_or_else(|_| "Non UTF-8 output found.".to_string())
    );
    println!("Test stderr:");
    println!(
        "{}",
        String::from_utf8(test_res.stderr)
            .unwrap_or_else(|_| "Non UTF-8 output found.".to_string())
    );
}
