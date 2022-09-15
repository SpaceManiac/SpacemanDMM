use super::*;
use crate::dmi::Dir;

#[derive(Default)]
pub struct TransitTube;
impl RenderPass for TransitTube {
    fn overlays<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
        _: &bumpalo::Bump,
    ) {
        use crate::dmi::Dir::*;

        if !atom.istype("/obj/structure/transit_tube/") {
            return;
        }

        let mut fulfill = |items: &[Dir]| {
            for &dir in items {
                if dir.is_diagonal() {
                    if dir.contains(North) {
                        create_tube_overlay(overlays, objtree, atom, dir.flip_ns(), Some(North));
                        if dir.contains(East) {
                            create_tube_overlay(overlays, objtree, atom, dir.flip_ew(), Some(East));
                        } else {
                            create_tube_overlay(overlays, objtree, atom, dir.flip_ew(), Some(West));
                        }
                    }
                } else {
                    create_tube_overlay(overlays, objtree, atom, dir, None);
                }
            }
        };

        let dir = atom
            .get_var("dir", objtree)
            .to_int()
            .and_then(Dir::from_int)
            .unwrap_or_default();
        if atom.istype("/obj/structure/transit_tube/station/reverse/") {
            fulfill(&match dir {
                North => [East],
                South => [West],
                East => [South],
                West => [North],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/station/") {
            fulfill(&match dir {
                North | South => [East, West],
                East | West => [North, South],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/junction/flipped/") {
            fulfill(&match dir {
                North => [North, Southwest, Southeast],
                South => [South, Northeast, Northwest],
                East => [East, Northwest, Southwest],
                West => [West, Southeast, Northeast],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/junction/") {
            fulfill(&match dir {
                North => [North, Southeast, Southwest],
                South => [South, Northwest, Northeast],
                East => [East, Southwest, Northwest],
                West => [West, Northeast, Southeast],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/curved/flipped/") {
            fulfill(&match dir {
                North => [North, Southeast],
                South => [South, Northwest],
                East => [East, Southwest],
                West => [Northeast, West],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/curved/") {
            fulfill(&match dir {
                North => [North, Southwest],
                South => [South, Northeast],
                East => [East, Northwest],
                West => [Southeast, West],
                _ => return,
            })
        } else if atom.istype("/obj/structure/transit_tube/diagonal/") {
            fulfill(&match dir {
                North | South => [Northeast, Southwest],
                East | West => [Northwest, Southeast],
                _ => return,
            })
        } else {
            fulfill(&match dir {
                North | South => [North, South],
                East | West => [East, West],
                _ => return,
            })
        }
    }
}

fn create_tube_overlay<'a>(
    output: &mut Vec<Sprite<'a>>,
    _objtree: &'a ObjectTree,
    source: &Atom<'a>,
    dir: Dir,
    shift: Option<Dir>,
) {
    let mut sprite = Sprite {
        dir,
        icon: source.sprite.icon,
        layer: source.sprite.layer,
        icon_state: "decorative",
        ..Default::default()
    };
    if let Some(shift) = shift {
        sprite.icon_state = "decorative_diag";
        match shift {
            Dir::North => sprite.ofs_y += 32,
            Dir::South => sprite.ofs_y -= 32,
            Dir::East => sprite.ofs_x += 32,
            Dir::West => sprite.ofs_x -= 32,
            _ => {}
        }
    }
    output.push(sprite);
}
