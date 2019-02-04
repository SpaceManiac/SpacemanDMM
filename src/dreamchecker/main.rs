//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.

extern crate dreammaker as dm;
use dm::objtree::{ProcValue, Code};
use dm::ast::Statement;

fn some_analysis(func: &ProcValue, code: &[Statement]) {
    println!("{:?}", func.location);
    for statement in code {
        println!("{:?}", statement);
    }
}

fn main() {
    let mut context = dm::Context::default();
    context.set_print_severity(Some(dm::Severity::Error));
    let env = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    let pp = dm::preprocessor::Preprocessor::new(&context, env)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    let mut present = 0;
    let mut invalid = 0;
    let mut builtin = 0;
    let mut disabled = 0;

    tree.root().recurse(&mut |ty| {
        for proc in ty.procs.values() {
            for value in proc.value.iter() {
                match value.code {
                    Code::Present(ref code) => {
                        present += 1;
                        some_analysis(&value, code);
                        std::process::exit(0);
                    }
                    Code::Invalid(_) => invalid += 1,
                    Code::Builtin => builtin += 1,
                    Code::Disabled => disabled += 1,
                }
            }
        }
    });

    println!("{:?}", (present, invalid, builtin, disabled));
}
