//! Print total number of `var` declarations and overrides in a project.

extern crate dreammaker as dm;

fn main() {
    let context = dm::Context::default();
    let env = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    let mut file_provider = dm::preprocessor::DiskFileProvider{};
    let pp = dm::preprocessor::Preprocessor::new(&context, env, &mut file_provider)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let ot = parser.parse().object_tree();

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
    println!("decls: {}\noverrides: {}\ntotal: {}", decls, overrides, decls + overrides);
}
