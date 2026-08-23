//! Print total number of `var` declarations and overrides in a project.

extern crate dreammaker as dm;

fn main() {
    let context = dm::Context::default();
    let env = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    let pp = dm::Preprocessor::new(&context, env).expect("i/o error opening .dme");
    let mut parser = dm::Parser::new(&context, pp);
    parser.enable_procs();
    let ot = parser.parse_object_tree();

    let mut decls = 0;
    let mut overrides = 0;
    ot.root().recurse(&mut |ty: dm::objtree::TypeRef| {
        for v in ty.vars.values() {
            if v.declaration.is_some() {
                decls += 1;
            } else {
                overrides += 1;
            }
        }
    });
    println!(
        "decls: {}\noverrides: {}\ntotal: {}",
        decls,
        overrides,
        decls + overrides
    );
}
