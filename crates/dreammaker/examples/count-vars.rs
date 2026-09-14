//! Print total number of `var` declarations and overrides in a project.

extern crate dreammaker as dm;

fn main() {
    let mut context = dm::Context::default();
    let env = context.configure_cli(".");
    let pp = context.unwrap(dm::Preprocessor::new(&context, env));
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
