use std::fs::File;
use std::io::{Read, Write};
use std::num::NonZeroU8;
use std::path::PathBuf;
use std::rc::Rc;

use blang::ast::print::PrettyPrinter;
use blang::codegen::Module;
use blang::diagnostics::{DiagConfig, Diagnostics, SourceMap};
use blang::parser::Parser;
use clap::{Parser as _, ValueEnum};

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the b source file
    input: PathBuf,
    /// Path to store object file
    #[arg(short)]
    output: Option<PathBuf>,
    /// Enable optimisations
    #[arg(short = 'O')]
    optimize: bool,
    /// Specify the target  
    #[arg(short, long, default_value_t = String::from("x86_64"))]
    target: String,
    /// Print info to stdout
    #[arg(long, value_enum, default_value_os_t)]
    print: PrintInfo,
    /// Maximal number of errors
    #[arg(long, default_value_t = 5)]
    max_errors: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, ValueEnum)]
#[value(rename_all = "lower")]
enum PrintInfo {
    #[default]
    None,
    Ast,
    Ir,
}

fn main() {
    let args = Args::parse();

    // TODO: move this to SourceMap
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

    let mut module = Module::new(&args.target, &args.input, args.optimize, diag.clone());
    module.run_global_pass(&defs);
    module.run_local_pass(&defs, args.print == PrintInfo::Ir);

    if diag.has_errors() {
        diag.print_errors().unwrap();
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

        let obj = module.finish();
        let out_path = args
            .output
            .unwrap_or_else(|| args.input.with_extension("o"));

        let mut output = File::create(&out_path).unwrap();
        output.write_all(&obj.emit().unwrap()).unwrap();
        println!(
            "Compiled to '{}' [{}]",
            out_path.display(),
            if args.optimize {
                "optimized"
            } else {
                "unoptimized"
            }
        );
    }
}
