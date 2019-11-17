use super::*;

#[derive(Default)]
pub struct TransitTube;
impl RenderPass for TransitTube {
    fn overlays<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
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
    output: &mut Vec<Sprite<'a>>,
    _objtree: &'a ObjectTree,
    source: &Atom<'a>,
    dir: i32,
    shift: i32,
) {
    use dmi::*;

    let mut sprite = Sprite {
        dir,
        icon: source.sprite.icon,
        layer: source.sprite.layer,
        icon_state: "decorative",
        .. Default::default()
    };
    if shift != 0 {
        sprite.icon_state = "decorative_diag";
        match shift {
            NORTH => sprite.ofs_y += 32,
            SOUTH => sprite.ofs_y -= 32,
            EAST => sprite.ofs_x += 32,
            WEST => sprite.ofs_x -= 32,
            _ => {}
        }
    }
    output.push(sprite);
}
