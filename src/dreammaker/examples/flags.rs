//! Two examples where SpacemanDMM helped check large refactors for errors.

extern crate dreammaker as dm;

fn main() {
    println!("---- parsing environment ----");
    std::env::set_current_dir("../tgstation").unwrap();

    let ctx = dm::Context::default();
    let objtree = &ctx.parse_environment("tgstation.dme".as_ref()).unwrap();

    // Used to check https://github.com/tgstation/tgstation/pull/38171
    // for mistakes transferring between `flags_1` and `item_flags`.
    println!("---- item_flags example ----");
    objtree.expect("/obj/item").recurse(&mut |ty| {
        print!("{}: ", ty.path);
        let mut flags_1 = ty
            .get_value("flags_1")
            .expect("flags_1")
            .constant
            .as_ref()
            .expect("f1c")
            .to_int()
            .unwrap_or(0);
        let mut item_flags = ty
            .get_value("item_flags")
            .expect("item_flags")
            .constant
            .as_ref()
            .expect("ofc")
            .to_int()
            .unwrap_or(0);

        // Migrate old flags_1 values to their item_flags equivalents
        let lhs =
            if flags_1 & (1<<1) != 0 { 1<<8 } else { 0 } |
            if flags_1 & (1<<2) != 0 { 1<<7 } else { 0 } |
            if flags_1 & (1<<6) != 0 { 1<<9 } else { 0 } |
            if flags_1 & (1<<10) != 0 { 1<<6 } else { 0 };
        flags_1 &= !((1<<1) | (1<<2) | (1<<6) | (1<<10));

        let rhs = item_flags & ((1<<6) | (1<<7) | (1<<8) | (1<<9));
        item_flags &= !rhs;

        let crossover = if rhs != 0 && lhs != 0 {
            panic!(
                "flags_1={}, item_flags={}, lhs={}, rhs={}",
                flags_1, item_flags, lhs, rhs
            );
        } else if lhs != 0 {
            lhs
        } else {
            rhs
        };

        println!(
            "flags_1={}, item_flags={}, crossover={}",
            flags_1, item_flags, crossover
        );
    });

    println!("---- anchored example ----");
    // Used to check https://github.com/tgstation/tgstation/pull/38116
    // for changes to any machinery types's `anchored` value, and to find
    // machinery for which `anchored = TRUE` was then redundant.
    objtree.expect("/obj/machinery").recurse(&mut |ty| {
        // print every type's `anchored` value for diffing
        let var = ty.get_value("anchored").unwrap();
        let anch = var.constant.as_ref().unwrap().to_bool();
        println!("{} -> {}", ty.path, anch);

        // print location info for any type with a redundant `anchored = TRUE`
        if anch && ty
            .parent_type()
            .unwrap()
            .get_value("anchored")
            .unwrap()
            .constant
            .as_ref()
            .unwrap()
            .to_bool()
        {
            println!("{}:{}", ctx.file_path(var.location.file).display(), var.location.line);
        }
    });
}
