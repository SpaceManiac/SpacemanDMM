//! Port of icon smoothing subsystem as of 2020.
//!
//! https://github.com/tgstation/tgstation/pull/52864
//! followed by
//! https://github.com/tgstation/tgstation/pull/53906

use dm::objtree::ObjectTree;
use dm::constants::Constant;
use crate::dmi::Dir;
use crate::minimap::{Sprite, Atom, GetVar, Neighborhood};

use super::RenderPass;

const NORTH_JUNCTION: i32 = 1 << 0;
const SOUTH_JUNCTION: i32 = 1 << 1;
const EAST_JUNCTION: i32 = 1 << 2;
const WEST_JUNCTION: i32 = 1 << 3;
const NORTHEAST_JUNCTION: i32 = 1 << 4;
const SOUTHEAST_JUNCTION: i32 = 1 << 5;
const SOUTHWEST_JUNCTION: i32 = 1 << 6;
const NORTHWEST_JUNCTION: i32 = 1 << 7;

/// Smoothing system in where adjacencies are calculated and used to build an image by mounting each corner at runtime.
const SMOOTH_CORNERS: i32 = 1 << 0;
/// Smoothing system in where adjacencies are calculated and used to select a pre-baked icon_state, encoded by bitmasking.
const SMOOTH_BITMASK: i32 = 1 << 1;
/// Atom has diagonal corners, with underlays under them.
const SMOOTH_DIAGONAL_CORNERS: i32 = 1 << 2;
/// Atom will smooth with the borders of the map.
const SMOOTH_BORDER: i32 = 1 << 3;

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
            if smooth_flags & SMOOTH_DIAGONAL_CORNERS != 0 {
                diagonal_smooth(output, objtree, bump, neighborhood, atom, adjacencies);
            } else {
                cardinal_smooth(output, objtree, bump, atom, adjacencies);
            }
            false
        } else if smooth_flags & SMOOTH_BITMASK != 0 {
            let adjacencies = calculate_adjacencies(objtree, neighborhood, atom, smooth_flags);
            bitmask_smooth(output, objtree, bump, neighborhood, atom, adjacencies, smooth_flags)
        } else {
            true
        }
    }
}

// ----------------------------------------------------------------------------
// Older cardinal smoothing system

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

    adjacencies |= check_one(Dir::North, NORTH_JUNCTION);
    adjacencies |= check_one(Dir::South, SOUTH_JUNCTION);
    adjacencies |= check_one(Dir::East, EAST_JUNCTION);
    adjacencies |= check_one(Dir::West, WEST_JUNCTION);

    if adjacencies & NORTH_JUNCTION != 0 {
        if adjacencies & WEST_JUNCTION != 0 {
            adjacencies |= check_one(Dir::Northwest, NORTHWEST_JUNCTION);
        }
        if adjacencies & EAST_JUNCTION != 0 {
            adjacencies |= check_one(Dir::Northeast, NORTHEAST_JUNCTION);
        }
    }
    if adjacencies & SOUTH_JUNCTION != 0 {
        if adjacencies & WEST_JUNCTION != 0 {
            adjacencies |= check_one(Dir::Southwest, SOUTHWEST_JUNCTION);
        }
        if adjacencies & EAST_JUNCTION != 0 {
            adjacencies |= check_one(Dir::Southeast, SOUTHEAST_JUNCTION);
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
        ("1", NORTH_JUNCTION, "n", WEST_JUNCTION, "w", NORTHWEST_JUNCTION),
        ("2", NORTH_JUNCTION, "n", EAST_JUNCTION, "e", NORTHEAST_JUNCTION),
        ("3", SOUTH_JUNCTION, "s", WEST_JUNCTION, "w", SOUTHWEST_JUNCTION),
        ("4", SOUTH_JUNCTION, "s", EAST_JUNCTION, "e", SOUTHEAST_JUNCTION),
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
    let presets = if adjacencies == NORTH_JUNCTION | WEST_JUNCTION {
        ["d-se", "d-se-0"]
    } else if adjacencies == NORTH_JUNCTION | EAST_JUNCTION {
        ["d-sw", "d-sw-0"]
    } else if adjacencies == SOUTH_JUNCTION | WEST_JUNCTION {
        ["d-ne", "d-ne-0"]
    } else if adjacencies == SOUTH_JUNCTION | EAST_JUNCTION {
        ["d-nw", "d-nw-0"]
    } else if adjacencies == NORTH_JUNCTION | WEST_JUNCTION | NORTHWEST_JUNCTION {
        ["d-se", "d-se-1"]
    } else if adjacencies == NORTH_JUNCTION | EAST_JUNCTION | NORTHEAST_JUNCTION {
        ["d-sw", "d-sw-1"]
    } else if adjacencies == SOUTH_JUNCTION | WEST_JUNCTION | SOUTHWEST_JUNCTION {
        ["d-ne", "d-ne-1"]
    } else if adjacencies == SOUTH_JUNCTION | EAST_JUNCTION | SOUTHEAST_JUNCTION {
        ["d-nw", "d-nw-1"]
    } else {
        return cardinal_smooth(output, objtree, bump, source, adjacencies);
    };

    // turf underneath
    if source.istype("/turf/closed/wall/") {
        diagonal_underlay(output, objtree, neighborhood, source, adjacencies);
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

fn diagonal_underlay<'a>(output: &mut Vec<Sprite<'a>>, objtree: &'a ObjectTree, neighborhood: &Neighborhood<'a, '_>, source: &Atom<'a>, adjacencies: i32) {
    // BYOND memes
    if source
        .get_var("fixed_underlay", objtree)
        .index(&Constant::string("space"))
        .is_some()
    {
        output.push(Sprite::from_vars(objtree, &objtree.expect("/turf/open/space/basic")));
    } else if let Some(dir) = reverse_ndir(adjacencies) {
        let dir = dir.flip();
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

fn reverse_ndir(ndir: i32) -> Option<Dir> {
    const NW1: i32 = NORTH_JUNCTION | WEST_JUNCTION;
    const NW2: i32 = NW1 | NORTHWEST_JUNCTION;
    const NE1: i32 = NORTH_JUNCTION | EAST_JUNCTION;
    const NE2: i32 = NE1 | NORTHEAST_JUNCTION;
    const SW1: i32 = SOUTH_JUNCTION | WEST_JUNCTION;
    const SW2: i32 = SW1 | SOUTHWEST_JUNCTION;
    const SE1: i32 = SOUTH_JUNCTION | EAST_JUNCTION;
    const SE2: i32 = SE1 | SOUTHEAST_JUNCTION;

    match ndir {
        NORTH_JUNCTION => Some(Dir::North),
        SOUTH_JUNCTION => Some(Dir::South),
        WEST_JUNCTION => Some(Dir::West),
        EAST_JUNCTION => Some(Dir::East),
        SOUTHEAST_JUNCTION | SE1 | SE2 => Some(Dir::Southeast),
        SOUTHWEST_JUNCTION | SW1 | SW2 => Some(Dir::Southwest),
        NORTHEAST_JUNCTION | NE1 | NE2 => Some(Dir::Northeast),
        NORTHWEST_JUNCTION | NW1 | NW2 => Some(Dir::Northwest),
        _ => None,
    }
}

// ----------------------------------------------------------------------------
// Bitmask smoothing system

fn bitmask_smooth<'a>(
    output: &mut Vec<Sprite<'a>>,
    objtree: &'a ObjectTree,
    bump: &'a bumpalo::Bump,
    neighborhood: &Neighborhood<'a, '_>,
    source: &Atom<'a>,
    smoothing_junction: i32,
    smooth_flags: i32,
) -> bool {
    let mut diagonal = "";
    if source.istype("/turf/open/floor/") {
        if source.get_var("broken", objtree).to_bool() || source.get_var("burnt", objtree).to_bool() {
            return true;  // use original appearance
        }
    } else if source.istype("/turf/closed/") && (smooth_flags & SMOOTH_DIAGONAL_CORNERS != 0) && reverse_ndir(smoothing_junction).is_some() {
        diagonal_underlay(output, objtree, neighborhood, source, smoothing_junction);
        diagonal = "-d";
    }

    let base_icon_state = source.get_var("base_icon_state", objtree).as_str().unwrap_or("");
    let mut sprite = Sprite {
        icon_state: bumpalo::format!(in bump, "{}-{}{}", base_icon_state, smoothing_junction, diagonal).into_bump_str(),
        .. source.sprite
    };
    if let Some(icon) = source.get_var("smooth_icon", objtree).as_path_str() {
        sprite.icon = icon;
    }
    output.push(sprite);

    false
}
