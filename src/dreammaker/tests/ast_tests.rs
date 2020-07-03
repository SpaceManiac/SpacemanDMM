extern crate dreammaker as dm;

use dm::*;
use dm::preprocessor::Preprocessor;
use dm::objtree::ObjectTree;

fn with_code<F: FnOnce(Context, ObjectTree)>(code: &'static str, f: F) {
    let context = Context::default();
    let path = std::path::PathBuf::from(r"test.dm");
    let pp = Preprocessor::from_buffer(&context, path, code.trim());
    let indents = indents::IndentProcessor::new(&context, pp);
    let mut parser = parser::Parser::new(&context, indents);
    parser.enable_procs();
    let _tree = parser.parse_object_tree();

    f(context, _tree)
}

#[test]
fn check_semicolon_in_proc_parameters() {
    with_code("
#define DEF1 0x01;
#define DEF2 \"asdf\" as text;

/proc/darn(foo = DEF1, bar = DEF2, anotherarg = 1)
", |context, _| {
        let errors = context.errors();
        assert_eq!(errors.len(), 2);

        for error in errors.into_iter() {
            assert_eq!(error.errortype().expect("No errortype set!"), "semicolon_in_proc_parameter");
        }
    });
}
