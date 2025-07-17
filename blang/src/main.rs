use std::fmt;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::num::NonZeroU8;
use std::path::{Path, PathBuf};
use std::process::{self, Command};
use std::rc::Rc;
use std::str::FromStr;

use blang::ast::print::PrettyPrinter;
use blang::codegen::Module;
use blang::diagnostics::{DiagConfig, Diagnostics, SourceMap};
use blang::parser::Parser;
use clap::{Parser as _, ValueEnum};
use target_lexicon::Triple;

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the b source file
    input: PathBuf,
    /// Path to store object file
    #[arg(short)]
    output: Option<PathBuf>,
    /// Enable optimizations
    #[arg(short = 'O')]
    optimize: bool,
    /// Compile as static library
    #[arg(short = 's')]
    static_library: bool,
    /// Compile without linking
    #[arg(short = 'c')]
    only_compile: bool,
    /// Specify the target  
    #[arg(short, long, value_parser = parse_triple, default_value_t = Triple::host())]
    target: Triple,
    /// Do not delete intermediate files
    #[arg(long)]
    save_temps: bool,
    /// Print info to stdout
    #[arg(long, value_enum, default_value_os_t)]
    print: PrintInfo,
    /// Maximal number of errors
    #[arg(long, default_value_t = 5)]
    max_errors: u8,
}

fn main() {
    let args = Args::parse();
    if args.only_compile && args.static_library {
        panic!("-c and -s can not coexist");
    }

    let mut input_file = File::open(&args.input).unwrap();
    let mut src = Vec::new();
    input_file.read_to_end(&mut src).unwrap();

    let source_map = SourceMap::new(&src, &args.input);
    let config = DiagConfig {
        max_errors: NonZeroU8::new(args.max_errors),
    };
    let diag = Rc::new(Diagnostics::new(config, source_map));

    let mut parser = Parser::new(&src, diag.clone());
    let defs = parser.parse_program();

    let mut module = Module::new(
        args.target.clone(),
        &args.input,
        args.optimize,
        diag.clone(),
    );
    module.run_global_pass(&defs);
    module.run_local_pass(&defs, args.print == PrintInfo::Ir);

    if diag.has_errors() {
        diag.print_errors().unwrap();
        process::exit(1);
    } else {
        match args.print {
            PrintInfo::None | PrintInfo::Ir => {}
            PrintInfo::Ast => {
                let mut pp = PrettyPrinter::new(2);
                for def in &defs {
                    pp.visit_def(def);
                }
                println!("{}", pp.display());
            }
        }

        println!("Target: {}", args.target);
        println!(
            "Default CallConv: {:#?}",
            args.target.default_calling_convention().unwrap()
        );
        let obj = module.finish();
        let obj_path = if args.only_compile {
            args.output
                .clone()
                .unwrap_or_else(|| args.input.with_extension("o"))
        } else {
            args.input.with_extension("o")
        };

        {
            let mut obj_file = File::create(&obj_path).unwrap();
            obj_file.write_all(&obj.emit().unwrap()).unwrap();
            println!("Created object file '{}'", obj_path.display());
        }

        println!(
            "Compiled to '{}' [{}]",
            obj_path.display(),
            if args.optimize {
                "optimized"
            } else {
                "unoptimized"
            }
        );
        if args.only_compile {
            process::exit(0);
        }

        if args.static_library {
            let lib_extension = if cfg!(windows) { "lib" } else { "a" };
            let lib_path = args
                .output
                .unwrap_or_else(|| args.input.with_extension(lib_extension));
            let lib_output = create_static_lib(&obj_path, &lib_path, &args.target);

            if !args.save_temps {
                fs::remove_file(&obj_path).expect("Removing object file");
                println!("Remove object file '{}'", obj_path.display());
            }

            if !lib_output.status.success() {
                eprintln!("Creating static library failed with {}", lib_output.status);
                io::stderr().write_all(&lib_output.stderr).unwrap();
                process::exit(1);
            }
        } else {
            // call linker
            let exe_extension = if cfg!(windows) { "exe" } else { "" };
            let exe_path = args
                .output
                .unwrap_or_else(|| args.input.with_extension(exe_extension));
            let linker_output = link_binary(&exe_path, &obj_path, &args.target);

            if !args.save_temps {
                fs::remove_file(&obj_path).expect("Removing object file");
                println!("Remove object file '{}'", obj_path.display());
            }

            if !linker_output.status.success() {
                eprintln!("Linking failed with {}", linker_output.status);
                io::stderr().write_all(&linker_output.stderr).unwrap();
                process::exit(1);
            }
        }
    }
}

fn create_static_lib(obj_path: &Path, lib_path: &Path, target: &Triple) -> process::Output {
    use target_lexicon::OperatingSystem as Os;
    let mut cmd = match target.operating_system {
        Os::MacOSX(_) | Os::Darwin(_) => {
            let mut cmd = Command::new("ar");
            cmd.arg("rcs").arg(lib_path);
            cmd
        }
        Os::Linux => {
            let mut cmd = Command::new("ar");
            cmd.arg("rcs").arg(lib_path);
            cmd
        }
        Os::Windows => {
            let mut cmd = Command::new("lib");
            cmd.arg(format!("/OUT:{}", lib_path.display()));
            cmd
        }
        _ => panic!(
            "operating system {} is not supported",
            target.operating_system
        ),
    };
    cmd.arg(obj_path);
    print!("Running: ");
    print_command(&cmd);
    cmd.output().unwrap()
}

fn link_binary(exe_path: &Path, obj_path: &Path, target: &Triple) -> process::Output {
    use target_lexicon::OperatingSystem as Os;
    let libb_path = Path::new("./libb").display();
    let mut cmd = match target.operating_system {
        Os::MacOSX(_) | Os::Darwin(_) => {
            let mut cmd = Command::new("clang");
            cmd.arg(obj_path)
                .arg("-o")
                .arg(exe_path)
                .arg(format!("-L{libb_path}"))
                .arg("-lb");
            cmd
        }
        Os::Linux => {
            let mut cmd = Command::new("gcc");
            cmd.arg(obj_path)
                .arg("-o")
                .arg(exe_path)
                .arg(format!("-L{libb_path}"))
                .arg("-lb");
            cmd
        }
        Os::Windows => {
            let mut cmd = Command::new("cl");
            cmd.arg(obj_path)
                .arg(format!("/Fe:{}", exe_path.display()))
                .arg("/link")
                .arg(format!("/LIBPATH:{libb_path}\\libb.lib"));
            cmd
        }
        _ => panic!(
            "operating system {} is not supported",
            target.operating_system
        ),
    };
    print!("Running linker: ");
    print_command(&cmd);
    cmd.output().unwrap()
}

fn print_command(cmd: &Command) {
    print!("{}", cmd.get_program().display());
    for arg in cmd.get_args() {
        print!(" {}", arg.display());
    }
    println!();
}

#[derive(Debug)]
struct TripleParseErrorWrapper(target_lexicon::ParseError);

impl fmt::Display for TripleParseErrorWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for TripleParseErrorWrapper {}

fn parse_triple(s: &str) -> Result<Triple, TripleParseErrorWrapper> {
    Triple::from_str(s).map_err(TripleParseErrorWrapper)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, ValueEnum)]
#[value(rename_all = "lower")]
enum PrintInfo {
    #[default]
    None,
    Ast,
    Ir,
}
