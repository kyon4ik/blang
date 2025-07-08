use std::path::{Path, PathBuf};
use std::process::{self, Command};

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the b source file
    input: PathBuf,
    /// Enable optimisations
    #[arg(short = 'O')]
    optimize: bool,
    /// Specify the target  
    #[arg(short, long, default_value_t = String::from("x86_64"))]
    target: String,
}

fn main() {
    let args = Args::parse();

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

    let test_obj = args.input.with_extension("o");
    let compiler_path = project_dir.join(format!("target/release/{project_name}"));
    println!("Compiler path: {}", compiler_path.display());
    let mut compile_cmd = Command::new(compiler_path);
    compile_cmd
        .args(["-t", &args.target])
        .arg("-o")
        .arg(test_obj.as_path())
        .arg(&args.input);
    if args.optimize {
        compile_cmd.arg("-O");
    }

    let comp_res = compile_cmd.output().expect("Compiling test");
    if comp_res.status.success() {
        println!("Compiled test successfully.");
    } else {
        eprintln!("Test compilation failed [{}]:", comp_res.status);
        eprintln!(
            "{}",
            String::from_utf8(comp_res.stderr)
                .unwrap_or_else(|_| "Non UTF-8 output found.".to_string())
        );
        process::exit(1);
    }

    let test_exe = args.input.with_extension("");
    let link_res = Command::new("gcc")
        .arg("-o")
        .arg(test_exe.as_path())
        .arg(test_obj.as_path())
        .output()
        .expect("Linking test");

    if link_res.status.success() {
        println!("Linked test successfully.");
    } else {
        eprintln!("Test linking failed [{}]:", link_res.status);
        eprintln!(
            "{}",
            String::from_utf8(link_res.stdout)
                .unwrap_or_else(|_| "Non UTF-8 output found.".to_string())
        );
        process::exit(1);
    }

    let test_res = Command::new(test_exe).output().expect("Running test");
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
