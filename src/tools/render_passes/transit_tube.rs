use super::*;

#[derive(Default)]
pub struct TransitTube;
impl RenderPass for TransitTube {
    fn overlays<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _: &mut Vec<Atom<'a>>,
        overlays: &mut Vec<Atom<'a>>,
    ) {
        use dmi::*;

        if !atom.istype("/obj/structure/transit_tube/") {
            return;
        }

        let dir = atom.get_var("dir", objtree).to_int().unwrap_or(::dmi::SOUTH);
        let mut fulfill = |items: &[i32]| {
            for &dir in items {
                if dir == NORTHEAST || dir == NORTHWEST || dir == SOUTHEAST || dir == SOUTHWEST {
                    if dir & NORTH != 0 {
                        create_tube_overlay(overlays, objtree, atom, dir ^ 3, NORTH);
                        if dir & EAST != 0 {
                            create_tube_overlay(overlays, objtree, atom, dir ^ 12, EAST);
                        } else {
                            create_tube_overlay(overlays, objtree, atom, dir ^ 12, WEST);
                        }
                    }
                } else {
                    create_tube_overlay(overlays, objtree, atom, dir, 0);
                }
            }
        };

        if atom.istype("/obj/structure/transit_tube/station/reverse/") {
            fulfill(&match dir {
                NORTH => [EAST],
                SOUTH => [WEST],
                EAST => [SOUTH],
                WEST => [NORTH],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/station/") {
            fulfill(&match dir {
                NORTH | SOUTH => [EAST, WEST],
                EAST | WEST => [NORTH, SOUTH],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/junction/flipped/") {
            fulfill(&match dir {
                NORTH => [NORTH, SOUTHWEST, SOUTHEAST],
                SOUTH => [SOUTH, NORTHEAST, NORTHWEST],
                EAST => [EAST, NORTHWEST, SOUTHWEST],
                WEST => [WEST, SOUTHEAST, NORTHEAST],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/junction/") {
            fulfill(&match dir {
                NORTH => [NORTH, SOUTHEAST, SOUTHWEST],
                SOUTH => [SOUTH, NORTHWEST, NORTHEAST],
                EAST => [EAST, SOUTHWEST, NORTHWEST],
                WEST => [WEST, NORTHEAST, SOUTHEAST],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/curved/flipped/") {
            fulfill(&match dir {
                NORTH => [NORTH, SOUTHEAST],
                SOUTH => [SOUTH, NORTHWEST],
                EAST => [EAST, SOUTHWEST],
                WEST => [NORTHEAST, WEST],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/curved/") {
            fulfill(&match dir {
                NORTH => [NORTH, SOUTHWEST],
                SOUTH => [SOUTH, NORTHEAST],
                EAST => [EAST, NORTHWEST],
                WEST => [SOUTHEAST, WEST],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/diagonal/") {
            fulfill(&match dir {
                NORTH | SOUTH => [NORTHEAST, SOUTHWEST],
                EAST | WEST => [NORTHWEST, SOUTHEAST],
                _ => return,
            })
        } else {
            fulfill(&match dir {
                NORTH | SOUTH => [NORTH, SOUTH],
                EAST | WEST => [EAST, WEST],
                _ => return,
            })
        }
    }
}

fn create_tube_overlay<'a>(
    output: &mut Vec<Atom<'a>>,
    objtree: &'a ObjectTree,
    source: &Atom<'a>,
    dir: i32,
    shift: i32,
) {
    use dmi::*;

    let mut copy = Atom::from_type(objtree, "/atom", source.loc).unwrap();
    copy.set_var("dir", Constant::Int(dir));
    copy.copy_var("layer", source, objtree);
    copy.copy_var("icon", source, objtree);
    if shift != 0 {
        copy.set_var("icon_state", Constant::string("decorative_diag"));
        match shift {
            NORTH => copy.set_var("pixel_y", Constant::Int(32)),
            SOUTH => copy.set_var("pixel_y", Constant::Int(-32)),
            EAST => copy.set_var("pixel_x", Constant::Int(32)),
            WEST => copy.set_var("pixel_x", Constant::Int(-32)),
            _ => {}
        }
    } else {
        copy.set_var("icon_state", Constant::string("decorative"));
    }
    output.push(copy);
}
