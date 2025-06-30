use std::fs::File;
use std::io::{Read, Write};
use std::num::NonZeroU8;
use std::path::PathBuf;
use std::rc::Rc;

use blang::ast::print::PrettyPrinter;
use blang::ast::resolve::{NameResolver, ValueChecker};
use blang::diagnostics::{DiagConfig, Diagnostics, SourceMap};
use blang::ir::CraneliftBackend;
use blang::lexer::Lexer;
use blang::parser::Parser;
use clap::Parser as _;

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: PathBuf,
    /// Enable optimisations
    #[arg(short = 'O')]
    optimize: bool,
    #[arg(long, default_value_t = 3)]
    max_errors: u8,
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

    let mut parser = Parser::new(Lexer::new(&src, diag.clone()), diag.clone());
    let defs = parser.parse_program();
    let mut resolver = NameResolver::new(diag.clone());
    let mut checker = ValueChecker::new(diag.clone());
    // let mut ir = CraneliftBackend::new("x86_64", args.optimize);
    for def in &defs {
        resolver.visit_def(def);
        checker.visit_def(def);
        // ir.visit_def(def);
    }

    // let obj = ir.finish();
    // let mut out_path = args.input.clone();
    // out_path.set_extension("o");
    // println!("Create object file: {}", out_path.display());
    // let mut output = File::create(out_path).unwrap();
    // output.write_all(&obj.emit().unwrap()).unwrap();

    if diag.has_errors() {
        diag.print_errors().unwrap();
    } else {
        let mut pp = PrettyPrinter::new();
        for def in &defs {
            pp.visit_def(def);
        }
        println!("{}", pp.display());
    }
}

// fn print_tokens(src: Rc<[u8]>, tokens: &[Token], path: &Path) {
//     let src_map = SourceMap::new(src, path);

//     let mut prev_line = 0;
//     for token in tokens {
//         let (start_loc, end_loc) = src_map.locate_span(token.span).unwrap();
//         assert_eq!(start_loc.line, end_loc.line);
//         if prev_line != start_loc.line {
//             println!("{:>3} | {}", start_loc.line, token.kind);
//             prev_line = start_loc.line;
//         } else {
//             println!("     | {}", token.kind);
//         }
//     }
// }
