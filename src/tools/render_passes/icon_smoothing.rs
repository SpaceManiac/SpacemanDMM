//! Port of icon smoothing subsystem.

use dm::constants::Constant;
use minimap::{Sprite, Context, Atom, GetVar, get_atom_list};

// (1 << N) where N is the usual value
const N_NORTH: i32 = 2;
const N_SOUTH: i32 = 4;
const N_EAST: i32 = 16;
const N_WEST: i32 = 256;
const N_NORTHEAST: i32 = 32;
const N_NORTHWEST: i32 = 512;
const N_SOUTHEAST: i32 = 64;
const N_SOUTHWEST: i32 = 1024;

const SMOOTH_TRUE: i32 = 1;  // smooth with exact specified types or just itself
const SMOOTH_MORE: i32 = 2;  // smooth with all subtypes thereof
const SMOOTH_DIAGONAL: i32 = 4;  // smooth diagonally
const SMOOTH_BORDER: i32 = 8;  // smooth with the borders of the map

pub fn handle_smooth<'a>(output: &mut Vec<Sprite<'a>>, ctx: Context<'a>, loc: (u32, u32), atom: Atom<'a>, mask: i32) {
    let smooth_flags = mask & atom.get_var("smooth", ctx.objtree).to_int().unwrap_or(0);
    if smooth_flags & (SMOOTH_TRUE | SMOOTH_MORE) != 0 {
        let adjacencies = calculate_adjacencies(ctx, loc, &atom, smooth_flags);
        if smooth_flags & SMOOTH_DIAGONAL != 0 {
            diagonal_smooth(output, ctx, loc, &atom, adjacencies);
        } else {
            cardinal_smooth(output, ctx, &atom, adjacencies);
        }
    } else {
        output.push(atom.sprite);
    }
}

fn calculate_adjacencies(ctx: Context, loc: (u32, u32), atom: &Atom, flags: i32) -> i32 {
    use dmi::*;
    // TODO: anchored check

    let mut adjacencies = 0;
    let check_one = |direction, flag| {
        if find_type_in_direction(ctx, loc, atom, direction, flags) {
            flag
        } else {
            0
        }
    };

    for &dir in &[SOUTH, NORTH, EAST, WEST] {
        adjacencies |= check_one(dir, 1 << dir);
    }

    if adjacencies & N_NORTH != 0 {
        if adjacencies & N_WEST != 0 {
            adjacencies |= check_one(NORTHWEST, N_NORTHWEST);
        }
        if adjacencies & N_EAST != 0 {
            adjacencies |= check_one(NORTHEAST, N_NORTHEAST);
        }
    }
    if adjacencies & N_SOUTH != 0 {
        if adjacencies & N_WEST != 0 {
            adjacencies |= check_one(SOUTHWEST, N_SOUTHWEST);
        }
        if adjacencies & N_EAST != 0 {
            adjacencies |= check_one(SOUTHEAST, N_SOUTHEAST);
        }
    }

    adjacencies
}

fn find_type_in_direction<'a>(ctx: Context, loc: (u32, u32), source: &Atom, direction: i32, flags: i32) -> bool {
    use std::ptr::eq;

    let (dx, dy) = offset(direction);
    let new_loc = (loc.0 as i32 + dx, loc.1 as i32 + dy);
    let (dim_y, dim_x) = ctx.level.grid.dim();
    if new_loc.0 < 0 || new_loc.1 < 0 || new_loc.0 >= dim_x as i32 || new_loc.1 >= dim_y as i32 {
        return flags & SMOOTH_BORDER != 0;
    }
    let new_loc = (new_loc.0 as u32, new_loc.1 as u32);

    // TODO: make this not call get_atom_list way too many times
    let atom_list = get_atom_list(
        ctx.objtree,
        &ctx.map.dictionary[&ctx.level.grid[ndarray::Dim([new_loc.1 as usize, new_loc.0 as usize])]],
        ctx.render_passes,
        None,
    );
    match source.get_var("canSmoothWith", ctx.objtree) {
        &Constant::List(ref elements) => if flags & SMOOTH_MORE != 0 {
            // smooth with canSmoothWith + subtypes
            for atom in atom_list {
                let mut path = &atom.type_.path[..];
                while !path.is_empty() {
                    if smoothlist_contains(elements, path) {
                        return true;
                    }
                    path = &path[..path.rfind("/").unwrap()];
                }
            }
        } else {
            // smooth only with exact types in canSmoothWith
            for atom in atom_list {
                if smoothlist_contains(elements, &atom.type_.path) {
                    return true;
                }
            }
        },
        _ => {
            // smooth only with the same type
            for atom in atom_list {
                if eq(atom.type_, source.type_) {
                    return true;
                }
            }
        },
    }
    false
}

fn smoothlist_contains(list: &[(Constant, Option<Constant>)], desired: &str) -> bool {
    for &(ref key, _) in list {
        // TODO: be more specific than to_string
        if key.to_string() == desired {
            return true;
        }
    }
    false
}

fn cardinal_smooth<'a>(output: &mut Vec<Sprite<'a>>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: i32) {
    for &(what, f1, n1, f2, n2, f3) in &[
        ("1", N_NORTH, "n", N_WEST, "w", N_NORTHWEST),
        ("2", N_NORTH, "n", N_EAST, "e", N_NORTHEAST),
        ("3", N_SOUTH, "s", N_WEST, "w", N_SOUTHWEST),
        ("4", N_SOUTH, "s", N_EAST, "e", N_SOUTHEAST),
    ] {
        let name = if (adjacencies & f1 != 0) && (adjacencies & f2 != 0) {
            if (adjacencies & f3) != 0 {
                format!("{}-f", what)
            } else {
                format!("{}-{}{}", what, n1, n2)
            }
        } else if adjacencies & f1 != 0 {
            format!("{}-{}", what, n1)
        } else if adjacencies & f2 != 0 {
            format!("{}-{}", what, n2)
        } else {
            format!("{}-i", what)
        };

        let mut sprite = Sprite {
            icon_state: ctx.bump.alloc(name),
            .. source.sprite
        };
        if let Some(icon) = source.get_var("smooth_icon", ctx.objtree).as_path_str() {
            sprite.icon = icon;
        }
        output.push(sprite);
    }
}

fn diagonal_smooth<'a>(output: &mut Vec<Sprite<'a>>, ctx: Context<'a>, loc: (u32, u32), source: &Atom<'a>, adjacencies: i32) {
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
        return cardinal_smooth(output, ctx, source, adjacencies);
    };

    // turf underneath
    if dm::objtree::subpath(&source.type_.path, "/turf/closed/wall/") {
        // BYOND memes
        if source
            .get_var("fixed_underlay", ctx.objtree)
            .index(&Constant::string("space"))
            .is_some()
        {
            output.push(Sprite::from_vars(ctx.objtree, &ctx.objtree.expect("/turf/open/space/basic")));
        } else {
            let dir = flip(reverse_ndir(adjacencies));
            let mut needs_plating = true;
            // check direct, then 45deg left, then 45deg right
            'dirs: for &each in &[dir, left_45(dir), right_45(dir)] {
                let (dx, dy) = offset(each);
                let new_loc = (loc.0 as i32 + dx, loc.1 as i32 + dy);
                let (dim_y, dim_x) = ctx.level.grid.dim();
                if !(new_loc.0 < 0 || new_loc.1 < 0 || new_loc.0 >= dim_x as i32 || new_loc.1 >= dim_y as i32) {
                    let new_loc = (new_loc.0 as u32, new_loc.1 as u32);
                    // TODO: make this not call get_atom_list way too many times
                    let atom_list = get_atom_list(
                        ctx.objtree,
                        &ctx.map.dictionary[&ctx.level.grid[ndarray::Dim([new_loc.1 as usize, new_loc.0 as usize])]],
                        ctx.render_passes,
                        None,
                    );
                    for atom in atom_list {
                        if dm::objtree::subpath(&atom.type_.path, "/turf/open/") {
                            output.push(Sprite::from_vars(ctx.objtree, &atom));
                            needs_plating = false;
                            break 'dirs;
                        }
                    }
                }
            }
            if needs_plating {
                output.push(Sprite::from_vars(ctx.objtree, &ctx.objtree.expect("/turf/open/floor/plating")));
            }
        }
    }

    // the diagonal overlay
    for &each in presets.iter() {
        let mut copy = Sprite {
            icon_state: each,
            .. source.sprite
        };
        if let Some(icon) = source.get_var("smooth_icon", ctx.objtree).as_path_str() {
            copy.icon = icon;
        }
        output.push(copy);
    }
}

fn offset(direction: i32) -> (i32, i32) {
    use dmi::*;
    match direction {
        0 => (0, 0),
        SOUTH => (0, 1),
        NORTH => (0, -1),
        EAST => (1, 0),
        WEST => (-1, 0),
        SOUTHEAST => (1, 1),
        SOUTHWEST => (-1, 1),
        NORTHEAST => (1, -1),
        NORTHWEST => (-1, -1),
        _ => panic!(),
    }
}

fn flip(direction: i32) -> i32 {
    use dmi::*;
    match direction {
        0 => 0,
        SOUTH => NORTH,
        NORTH => SOUTH,
        EAST => WEST,
        WEST => EAST,
        SOUTHEAST => NORTHWEST,
        SOUTHWEST => NORTHEAST,
        NORTHEAST => SOUTHWEST,
        NORTHWEST => SOUTHEAST,
        _ => panic!(),
    }
}

fn reverse_ndir(ndir: i32) -> i32 {
    use dmi::*;
    const NW1: i32 = N_NORTH | N_WEST;
    const NW2: i32 = NW1 | N_NORTHWEST;
    const NE1: i32 = N_NORTH | N_EAST;
    const NE2: i32 = NE1 | N_NORTHEAST;
    const SW1: i32 = N_SOUTH | N_WEST;
    const SW2: i32 = SW1 | N_SOUTHWEST;
    const SE1: i32 = N_SOUTH | N_EAST;
    const SE2: i32 = SE1 | N_SOUTHEAST;

    match ndir {
        N_NORTH => NORTH,
        N_SOUTH => SOUTH,
        N_WEST => WEST,
        N_EAST => EAST,
        N_SOUTHEAST | SE1 | SE2 => SOUTHEAST,
        N_SOUTHWEST | SW1 | SW2 => SOUTHWEST,
        N_NORTHEAST | NE1 | NE2 => NORTHEAST,
        N_NORTHWEST | NW1 | NW2 => NORTHWEST,
        _ => panic!(),
    }
}

fn left_45(dir: i32) -> i32 {
    use dmi::*;
    match dir {
        NORTH => NORTHWEST,
        NORTHEAST => NORTH,
        EAST => NORTHEAST,
        SOUTHEAST => EAST,
        SOUTH => SOUTHEAST,
        SOUTHWEST => SOUTH,
        WEST => SOUTHWEST,
        NORTHWEST => WEST,
        e => e,
    }
}

fn right_45(dir: i32) -> i32 {
    use dmi::*;
    match dir {
        NORTH => NORTHEAST,
        NORTHEAST => EAST,
        EAST => SOUTHEAST,
        SOUTHEAST => SOUTH,
        SOUTH => SOUTHWEST,
        SOUTHWEST => WEST,
        WEST => NORTHWEST,
        NORTHWEST => NORTH,
        e => e,
    }
}
