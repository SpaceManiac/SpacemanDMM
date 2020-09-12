//! Port of icon smoothing subsystem as of 2020.
//!
//! https://github.com/tgstation/tgstation/pull/52864
//! (b0726e032ba01221bf318928917fa9a55887c5ef)

use dm::objtree::ObjectTree;
use dm::constants::Constant;
use crate::dmi::Dir;
use crate::minimap::{Sprite, Atom, GetVar, Neighborhood};

use super::RenderPass;

const N_NORTH: i32 = 1 << 0;
const N_SOUTH: i32 = 1 << 1;
const N_EAST: i32 = 1 << 2;
const N_WEST: i32 = 1 << 3;
const N_NORTHEAST: i32 = 1 << 4;
const N_NORTHWEST: i32 = 1 << 5;
const N_SOUTHEAST: i32 = 1 << 6;
const N_SOUTHWEST: i32 = 1 << 7;

const SMOOTH_CORNERS: i32 = 1 << 0;  // Smoothing system in where adjacencies are calculated and used to build an image by mounting each corner at runtime.
const SMOOTH_DIAGONAL: i32 = 1 << 2;  // if atom should smooth diagonally, this should be present in 'smoothing_flags' var
const SMOOTH_BORDER: i32 = 1 << 3;  // atom will smooth with the borders of the map

pub struct IconSmoothing {
    pub mask: i32,
}

impl Default for IconSmoothing {
    fn default() -> Self {
        IconSmoothing { mask: !0 }
    }
}

impl RenderPass for IconSmoothing {
    fn adjust_sprite<'a>(&self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        _objtree: &'a ObjectTree,
        _bump: &'a bumpalo::Bump,
    ) {
        if atom.istype("/turf/closed/mineral/") {
            sprite.ofs_x -= 4;
            sprite.ofs_y -= 4;
        }
    }

    fn neighborhood_appearance<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        neighborhood: &Neighborhood<'a, '_>,
        output: &mut Vec<Sprite<'a>>,
        bump: &'a bumpalo::Bump,
    ) -> bool {
        let smooth_flags = self.mask & atom.get_var("smoothing_flags", objtree).to_int().unwrap_or(0);
        if smooth_flags & SMOOTH_CORNERS != 0 {
            let adjacencies = calculate_adjacencies(objtree, neighborhood, atom, smooth_flags);
            if smooth_flags & SMOOTH_DIAGONAL != 0 {
                diagonal_smooth(output, objtree, bump, neighborhood, atom, adjacencies);
            } else {
                cardinal_smooth(output, objtree, bump, atom, adjacencies);
            }
            false
        } else {
            true
        }
    }
}

fn calculate_adjacencies(objtree: &ObjectTree, neighborhood: &Neighborhood, atom: &Atom, smooth_flags: i32) -> i32 {
    if atom.istype("/atom/movable/") {
        if atom.get_var("can_be_unanchored", objtree).to_bool() && !atom.get_var("anchored", objtree).to_bool() {
            return 0;
        }
    }

    let mut adjacencies = 0;
    let check_one = |direction, flag| {
        if find_type_in_direction(objtree, neighborhood, atom, direction, smooth_flags) {
            flag
        } else {
            0
        }
    };

    adjacencies |= check_one(Dir::North, N_NORTH);
    adjacencies |= check_one(Dir::South, N_SOUTH);
    adjacencies |= check_one(Dir::East, N_EAST);
    adjacencies |= check_one(Dir::West, N_WEST);

    if adjacencies & N_NORTH != 0 {
        if adjacencies & N_WEST != 0 {
            adjacencies |= check_one(Dir::Northwest, N_NORTHWEST);
        }
        if adjacencies & N_EAST != 0 {
            adjacencies |= check_one(Dir::Northeast, N_NORTHEAST);
        }
    }
    if adjacencies & N_SOUTH != 0 {
        if adjacencies & N_WEST != 0 {
            adjacencies |= check_one(Dir::Southwest, N_SOUTHWEST);
        }
        if adjacencies & N_EAST != 0 {
            adjacencies |= check_one(Dir::Southeast, N_SOUTHEAST);
        }
    }

    adjacencies
}

fn find_type_in_direction(objtree: &ObjectTree, adjacency: &Neighborhood, source: &Atom, direction: Dir, smooth_flags: i32) -> bool {
    let atom_list = adjacency.offset(direction);
    if atom_list.is_empty() {
        return smooth_flags & SMOOTH_BORDER != 0;
    }

    match source.get_var("canSmoothWith", objtree) {
        &Constant::List(ref elements) => {
            // smooth with anything for which their smoothing_groups overlaps our canSmoothWith
            let set: std::collections::HashSet<_> = elements.iter().map(|x| &x.0).collect();
            for atom in atom_list {
                if let &Constant::List(ref elements2) = atom.get_var("smoothing_groups", objtree) {
                    let set2: std::collections::HashSet<_> = elements2.iter().map(|x| &x.0).collect();
                    if set.intersection(&set2).next().is_some() {
                        return true;
                    }
                }
            }
        },
        _ => {
            // smooth only with the same type
            for atom in atom_list {
                if std::ptr::eq(atom.get_path(), source.get_path()) {
                    return true;
                }
            }
        },
    }
    false
}

fn cardinal_smooth<'a>(output: &mut Vec<Sprite<'a>>, objtree: &'a ObjectTree, bump: &'a bumpalo::Bump, source: &Atom<'a>, adjacencies: i32) {
    for &(what, f1, n1, f2, n2, f3) in &[
        ("1", N_NORTH, "n", N_WEST, "w", N_NORTHWEST),
        ("2", N_NORTH, "n", N_EAST, "e", N_NORTHEAST),
        ("3", N_SOUTH, "s", N_WEST, "w", N_SOUTHWEST),
        ("4", N_SOUTH, "s", N_EAST, "e", N_SOUTHEAST),
    ] {
        let name = if (adjacencies & f1 != 0) && (adjacencies & f2 != 0) {
            if (adjacencies & f3) != 0 {
                bumpalo::format!(in bump, "{}-f", what)
            } else {
                bumpalo::format!(in bump, "{}-{}{}", what, n1, n2)
            }
        } else if adjacencies & f1 != 0 {
            bumpalo::format!(in bump, "{}-{}", what, n1)
        } else if adjacencies & f2 != 0 {
            bumpalo::format!(in bump, "{}-{}", what, n2)
        } else {
            bumpalo::format!(in bump, "{}-i", what)
        };

        let mut sprite = Sprite {
            icon_state: name.into_bump_str(),
            .. source.sprite
        };
        if let Some(icon) = source.get_var("smooth_icon", objtree).as_path_str() {
            sprite.icon = icon;
        }
        output.push(sprite);
    }
}

fn diagonal_smooth<'a>(output: &mut Vec<Sprite<'a>>, objtree: &'a ObjectTree, bump: &'a bumpalo::Bump, neighborhood: &Neighborhood<'a, '_>, source: &Atom<'a>, adjacencies: i32) {
    let presets = if adjacencies == N_NORTH | N_WEST {
        ["d-se", "d-se-0"]
    } else if adjacencies == N_NORTH | N_EAST {
        ["d-sw", "d-sw-0"]
    } else if adjacencies == N_SOUTH | N_WEST {
        ["d-ne", "d-ne-0"]
    } else if adjacencies == N_SOUTH | N_EAST {
        ["d-nw", "d-nw-0"]
    } else if adjacencies == N_NORTH | N_WEST | N_NORTHWEST {
        ["d-se", "d-se-1"]
    } else if adjacencies == N_NORTH | N_EAST | N_NORTHEAST {
        ["d-sw", "d-sw-1"]
    } else if adjacencies == N_SOUTH | N_WEST | N_SOUTHWEST {
        ["d-ne", "d-ne-1"]
    } else if adjacencies == N_SOUTH | N_EAST | N_SOUTHEAST {
        ["d-nw", "d-nw-1"]
    } else {
        return cardinal_smooth(output, objtree, bump, source, adjacencies);
    };

    // turf underneath
    if source.istype("/turf/closed/wall/") {
        // BYOND memes
        if source
            .get_var("fixed_underlay", objtree)
            .index(&Constant::string("space"))
            .is_some()
        {
            output.push(Sprite::from_vars(objtree, &objtree.expect("/turf/open/space/basic")));
        } else {
            let dir = reverse_ndir(adjacencies).flip();
            let mut needs_plating = true;
            // check direct, then 45deg left, then 45deg right
            'dirs: for &each in &[dir, dir.counterclockwise_45(), dir.clockwise_45()] {
                let atom_list = neighborhood.offset(each);
                for atom in atom_list {
                    if atom.istype("/turf/open/") {
                        output.push(Sprite::from_vars(objtree, atom));
                        needs_plating = false;
                        break 'dirs;
                    }
                }
            }
            if needs_plating {
                output.push(Sprite::from_vars(objtree, &objtree.expect("/turf/open/floor/plating")));
            }
        }
    }

    // the diagonal overlay
    for &each in presets.iter() {
        let mut copy = Sprite {
            icon_state: each,
            .. source.sprite
        };
        if let Some(icon) = source.get_var("smooth_icon", objtree).as_path_str() {
            copy.icon = icon;
        }
        output.push(copy);
    }
}

fn reverse_ndir(ndir: i32) -> Dir {
    const NW1: i32 = N_NORTH | N_WEST;
    const NW2: i32 = NW1 | N_NORTHWEST;
    const NE1: i32 = N_NORTH | N_EAST;
    const NE2: i32 = NE1 | N_NORTHEAST;
    const SW1: i32 = N_SOUTH | N_WEST;
    const SW2: i32 = SW1 | N_SOUTHWEST;
    const SE1: i32 = N_SOUTH | N_EAST;
    const SE2: i32 = SE1 | N_SOUTHEAST;

    match ndir {
        N_NORTH => Dir::North,
        N_SOUTH => Dir::South,
        N_WEST => Dir::West,
        N_EAST => Dir::East,
        N_SOUTHEAST | SE1 | SE2 => Dir::Southeast,
        N_SOUTHWEST | SW1 | SW2 => Dir::Southwest,
        N_NORTHEAST | NE1 | NE2 => Dir::Northeast,
        N_NORTHWEST | NW1 | NW2 => Dir::Northwest,
        _ => panic!(),
    }
}
