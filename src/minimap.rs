use std::collections::HashMap;
use std::path::{Path, PathBuf};

use ndarray::{self, Axis};

use objtree::*;
use dmm::{Map, Grid, Prefab};
use dmi::{Image, IconFile};

const TILE_SIZE: u32 = 32;
const CONTRABAND_POSTERS: u32 = 44;
const LEGIT_POSTERS: u32 = 35;

// ----------------------------------------------------------------------------
// Main minimap code

#[derive(Clone, Copy)]
struct Context<'a> {
    objtree: &'a ObjectTree,
    map: &'a Map,
    grid: Grid<'a>,
}

pub fn generate(
    objtree: &ObjectTree,
    map: &Map,
    z: usize,
    icon_cache: &mut HashMap<PathBuf, IconFile>,
) -> Result<Image, ()> {
    use rand::Rng;

    flame!("minimap");
    let grid = map.z_level(z);
    let (len_x, len_y) = grid.dim();
    let ctx = Context { objtree, map, grid };

    // loads atoms from the prefabs on the map and adds overlays and smoothing
    let mut atoms = Vec::new();
    let mut overlays = Vec::new();
    //flame!("collect");
    for (y, row) in grid.axis_iter(Axis(0)).enumerate() {
        for (x, e) in row.iter().enumerate() {
            for mut atom in get_atom_list(objtree, &map.dictionary[e], (x as u32, y as u32)) {
                // icons which differ from their map states
                let p = &atom.type_.path;
                if p == "/obj/structures/table/wood/fancy/black" {
                    atom.set_var("icon", "'icons/obj/smooth_structures/fancy_table_black.dmi'");
                } else if p == "/obj/structures/table/wood/fancy" {
                    atom.set_var("icon", "'icons/obj/smooth_structures/fancy_table.dmi'");
                } else if subtype(p, "/turf/closed/mineral/") {
                    atom.set_var("pixel_x", "-4");
                    atom.set_var("pixel_y", "-4");
                } else if subtype(p, "/obj/structure/bookcase/") {
                    atom.set_var("icon_state", "\"book-0\"");
                } else if subtype(p, "/obj/structure/sign/poster/contraband/random/") {
                    atom.set_var("icon_state", format!("\"poster{}\"", ::rand::thread_rng().gen_range(1, 1 + CONTRABAND_POSTERS)));
                } else if subtype(p, "/obj/structure/sign/poster/official/random/") {
                    atom.set_var("icon_state", format!("\"poster{}_legit\"", ::rand::thread_rng().gen_range(1, 1 + LEGIT_POSTERS)));
                } else if subtype(p, "/obj/structure/sign/poster/random/") {
                    let i = 1 + ::rand::thread_rng().gen_range(0, CONTRABAND_POSTERS + LEGIT_POSTERS);
                    if i <= CONTRABAND_POSTERS {
                        atom.set_var("icon_state", format!("\"poster{}\"", i));
                    } else {
                        atom.set_var("icon_state", format!("\"poster{}_legit\"", i - CONTRABAND_POSTERS));
                    }
                }

                // overlays and underlays
                if subtype(p, "/obj/structure/closet/") {
                    // closet doors
                    let mut door = atom.get_var("icon_door", objtree);
                    if door == "" || door == "null" {
                        door = atom.get_var("icon_state", objtree);
                    }
                    let mut copy = atom.clone();
                    copy.set_var("icon_state", format!("\"{}_door\"", &door[1..door.len()-1]));
                    overlays.push(copy);
                } else if subtype(p, "/obj/machinery/computer/") || subtype(p, "/obj/machinery/power/solar_control/") {
                    // computer screens and keyboards
                    let screen = atom.get_var("icon_screen", objtree);
                    if screen != "" && screen != "null" {
                        let mut copy = atom.clone();
                        copy.set_var("icon_state", screen);
                        overlays.push(copy);
                    }
                    let keyboard = atom.get_var("icon_keyboard", objtree);
                    if keyboard != "" && keyboard != "null" {
                        let mut copy = atom.clone();
                        copy.set_var("icon_state", keyboard);
                        overlays.push(copy);
                    }
                } else if subtype(p, "/obj/structure/transit_tube/") {
                    generate_tube_overlays(&mut overlays, ctx, &atom);
                } else if subtype(p, "/obj/machinery/door/airlock/") {
                    let mut copy = atom.clone();
                    let glass = atom.get_var("glass", objtree);
                    if glass == "" || glass == "FALSE" || glass == "0" {
                        copy.set_var("icon_state", "\"fill_closed\"");
                    } else if glass == "TRUE" || glass == "1" {
                        copy.set_var("icon_state", "\"glass_closed\"");
                        copy.set_var("icon", atom.get_var("overlays_file", objtree));
                    }
                    overlays.push(copy);
                } else if subtype(p, "/obj/machinery/atmospherics/components/unary/") {
                    let aboveground = match atom.get_var("icon_state", objtree) {
                        "\"vent_map\"" => "vent_off",
                        "\"vent_map_on\"" => "vent_out",
                        "\"vent_map_siphon_on\"" => "vent_in",
                        "\"scrub_map\"" => "scrub_off",
                        "\"scrub_map_on\"" => "scrub_on",
                        _ => "",
                    };
                    if !aboveground.is_empty() {
                        let mut copy = atom.clone();
                        copy.set_var("icon_state", format!("{:?}", aboveground));
                        overlays.push(copy);
                        atom.set_var("layer", "-5");
                    }
                } else if subtype(p, "/obj/item/storage/box/") && !subtype(p, "/obj/item/storage/box/papersack/") {
                    let mut copy = atom.clone();
                    copy.set_var("icon_state", atom.get_var("illustration", objtree));
                    overlays.push(copy);
                }

                // smoothing time
                handle_smooth(&mut atoms, ctx, atom, !0);
                atoms.extend(overlays.drain(..));
            }
        }
    }

    // sorts the atom list and renders them onto the output image
    atoms.sort_by_key(|a| layer_of(objtree, a));
    let mut map_image = Image::new_rgba(len_x as u32 * TILE_SIZE, len_y as u32 * TILE_SIZE);
    //flame!("render");
    'atom: for atom in atoms {
        // At this time, space is invisible. Earlier steps need to process it.
        if subtype(&atom.type_.path, "/turf/open/space/") {
            continue;
        }

        let icon = atom.get_var("icon", objtree);
        if icon.is_empty() {
            println!("no icon: {}", atom.type_.path);
            continue
        }
        let icon_state = atom.get_var_or("icon_state", objtree, "\"\"");
        let dir = atom.get_var("dir", objtree);

        let path: &Path = icon[1..icon.len()-1].as_ref();
        let icon_file = icon_cache.entry(path.to_owned())
            .or_insert_with(|| IconFile::from_file(path).unwrap());

        if let Some(mut rect) = icon_file.rect_of(&icon_state[1..icon_state.len()-1], dir) {
            let pixel_x = atom.get_var_or("pixel_x", objtree, "0").parse::<f32>().unwrap() as i32;
            let pixel_y = atom.get_var_or("pixel_y", objtree, "0").parse::<f32>().unwrap() as i32 + icon_file.metadata.height as i32;
            let mut loc = (
                (atom.loc.0 * TILE_SIZE) as i32 + pixel_x,
                (atom.loc.1 * TILE_SIZE + TILE_SIZE) as i32 - pixel_y
            );

            // OOB handling
            if loc.0 < 0 {
                rect.0 += (-loc.0) as u32;
                rect.2 -= (-loc.0) as u32;
                loc.0 = 0;
            }
            while loc.0 + rect.2 as i32 > map_image.info.width as i32 {
                rect.2 -= 1;
                if rect.2 == 0 { continue 'atom }
            }
            if loc.1 < 0 {
                rect.1 += (-loc.1) as u32;
                rect.2 -= (-loc.1) as u32;
                loc.1 = 0;
            }
            while loc.1 + rect.3 as i32 > map_image.info.width as i32 {
                rect.3 -= 1;
                if rect.3 == 0 { continue 'atom }
            }
            let loc = (loc.0 as u32, loc.1 as u32);

            // horrifying color handling
            let color = match atom.get_var("color", objtree) {
                "" | "\"\"" => [255, 255, 255, 255],
                // presented now for your amusement: BYOND
                // pipes
                "25500" => [255, 0, 0, 255],
                "02550" => [0, 255, 0, 255],
                "00255" => [0, 0, 255, 255],
                "640128" => [64, 0, 128, 255],
                "302560" => [30, 255, 0, 255],
                "0256249" => [0, 255, 249, 255],
                "25512925" => [255, 129, 25, 255],
                "2551980" => [255, 19, 80, 255],
                "13043272" => [130, 43, 255, 255], // 272?? What??
                "1280182" => [128, 0, 128, 255],
                "696969" => [69, 69, 69, 255],
                "\"red\"" => [255, 0, 0, 255],
                // comfy chairs
                "2551130" => [255, 113, 0, 255],
                "255253195" => [255, 253, 195, 255],
                "0255255" => [0, 255, 255, 255],
                "167164153" => [167, 164, 153, 255],
                "2552510" => [255, 251, 0, 255],
                "255255255" => [255, 255, 255, 255],
                // HTML color parsing
                color => if color.starts_with("\"#") {
                    assert_eq!(color.len(), 9);
                    let mut sum = 0;
                    for ch in color[2..color.len()-1].chars() {
                        sum = 16 * sum + ch.to_digit(16).unwrap();
                    }
                    [(sum >> 16) as u8, (sum >> 8) as u8, sum as u8, 255]
                } else {
                    panic!("{} -> {}", atom.type_.path, color);
                }
            };

            // the real business
            map_image.composite(&icon_file.image, loc, rect, color);
        } else {
            //println!("Missing icon: type={}, icon={}, icon_state={}", atom.type_.path, icon, icon_state);
        }
    }

    Ok(map_image)
}

fn get_atom_list<'a>(objtree: &'a ObjectTree, prefabs: &'a [Prefab], loc: (u32, u32)) -> Vec<Atom<'a>> {
    flame!("get_atom_list");
    let mut result = Vec::new();

    for fab in prefabs {
        if subtype(&fab.path, "/area/") { continue }
        let spawner = subtype(&fab.path, "/obj/effect/spawner/structure/");
        if subtype(&fab.path, "/obj/effect/spawner/") && !spawner { continue }

        // look up the type
        let atom = match Atom::from_prefab(objtree, fab, loc) {
            Some(x) => x,
            None => {
                println!("Warning: missing {:?}", fab.path);
                continue
            }
        };

        // invisible objects and syndicate balloons are not to show
        if atom.get_var_or("invisibility", objtree, "0").parse::<f32>().unwrap() > 60. {
            continue;
        }
        if atom.get_var("icon", objtree) == "'icons/obj/items_and_weapons.dmi'" &&
            atom.get_var("icon_state", objtree) == "\"syndballoon\""
        {
            continue;
        }

        // convert structure spanwers to their structures
        if spawner {
            // BYOND
            for each in atom.get_var("spawn_list", objtree).split("/obj/") {
                if each.is_empty() { continue }
                result.push(Atom::from_type(objtree, &format!("/obj/{}", each), loc).unwrap());
            }
        } else {
            result.push(atom);
        }
    }

    result
}

// ----------------------------------------------------------------------------
// Atoms and related utilities

#[derive(Debug, Clone)]
struct Atom<'a> {
    type_: &'a Type,
    prefab: &'a Vars,
    vars: Vars,
    loc: (u32, u32),
}

impl<'a> Atom<'a> {
    fn from_prefab(objtree: &'a ObjectTree, fab: &'a Prefab, loc: (u32, u32)) -> Option<Self> {
        objtree.find(&fab.path).map(|type_| Atom {
            type_,
            prefab: &fab.vars,
            vars: Default::default(),
            loc,
        })
    }

    fn from_type(objtree: &'a ObjectTree, path: &str, loc: (u32, u32)) -> Option<Self> {
        objtree.find(path).map(|type_| Atom {
            type_,
            prefab: objtree.blank_vars(),
            vars: Default::default(),
            loc,
        })
    }

    fn get_var(&self, key: &str, objtree: &'a ObjectTree) -> &str {
        self.get_var_or(key, objtree, "")
    }

    fn get_var_or<'b>(&'b self, key: &str, objtree: &'a ObjectTree, default: &'b str) -> &'b str {
        if let Some(v) = self.vars.get(key) {
            return v;
        }
        if let Some(v) = self.prefab.get(key) {
            return v;
        }
        let mut current = Some(self.type_);
        while let Some(t) = current.take() {
            if let Some(v) = t.vars.get(key) {
                return v;
            }
            current = objtree.parent_of(t);
        }
        default
    }

    fn set_var<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.vars.insert(key.into(), value.into());
    }
}

fn subtype(path: &str, parent: &str) -> bool {
    debug_assert!(path.starts_with("/") && parent.starts_with("/") && parent.ends_with("/"));
    path == &parent[..parent.len() - 1] || path.starts_with(parent)
}

fn layer_of(objtree: &ObjectTree, atom: &Atom) -> i32 {
    let p = &atom.type_.path;
    if subtype(p, "/turf/open/floor/plating/") || subtype(p, "/turf/open/space/") {
        -10_000  // under everything
    } else if subtype(p, "/turf/closed/mineral/") {
        -3_000   // above hidden stuff and plating but below walls
    } else if subtype(p, "/turf/open/floor/") || subtype(p, "/turf/closed/") {
        -2_000   // above hidden pipes and wires
    } else if subtype(p, "/turf/") {
        -10_000  // under everything
    } else if subtype(p, "/obj/effect/turf_decal/") {
        -1_000   // above turfs
    } else if subtype(p, "/obj/structure/disposalpipe/") {
        -6_000
    } else if subtype(p, "/obj/machinery/atmospherics/pipe/") && p.contains("hidden") {
        -5_000
    } else if subtype(p, "/obj/structure/cable/") {
        -4_000
    } else if subtype(p, "/obj/structure/lattice/") {
        -8_000
    } else if subtype(p, "/obj/machinery/navbeacon/") {
        -3_000
    } else {
        let layer = atom.get_var("layer", objtree);
        match layer {
            "" => panic!("{}", atom.type_.path),
            "TURF_LAYER" => 2_000,
            "OBJ_LAYER" => 3_000,
            "MOB_LAYER" => 4_000,
            "FLY_LAYER" => 5_000,
            layer => {
                (layer.parse::<f32>().unwrap() * 1000.) as i32
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Icon smoothing subsystem

// (1 << N) where N is the usual value
const N_NORTH: u32 = 2;
const N_SOUTH: u32 = 4;
const N_EAST: u32 = 16;
const N_WEST: u32 = 256;
const N_NORTHEAST: u32 = 32;
const N_NORTHWEST: u32 = 512;
const N_SOUTHEAST: u32 = 64;
const N_SOUTHWEST: u32 = 1024;

const SMOOTH_TRUE: u32 = 1;  // smooth with exact specified types or just itself
const SMOOTH_MORE: u32 = 2;  // smooth with all subtypes thereof
const SMOOTH_DIAGONAL: u32 = 4;  // smooth diagonally
const SMOOTH_BORDER: u32 = 8;  // smooth with the borders of the map

fn handle_smooth<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, atom: Atom<'a>, mask: u32) {
    let mut smooth_flags = 0;
    for ch in atom.get_var("smooth", ctx.objtree).chars() {
        if let Some(digit) = ch.to_digit(10) {
            smooth_flags |= digit;
        }
    }
    smooth_flags &= mask;
    if smooth_flags & (SMOOTH_TRUE | SMOOTH_MORE) != 0 {
        let adjacencies = calculate_adjacencies(ctx, &atom, smooth_flags);
        if smooth_flags & SMOOTH_DIAGONAL != 0 {
            diagonal_smooth(output, ctx, &atom, adjacencies);
        } else {
            cardinal_smooth(output, ctx, &atom, adjacencies);
        }
    } else {
        output.push(atom);
    }
}

fn calculate_adjacencies(ctx: Context, atom: &Atom, flags: u32) -> u32 {
    use dmi::*;
    // TODO: anchored check

    let mut adjacencies = 0;
    let check_one = |direction, flag| {
        if find_type_in_direction(ctx, atom, direction, flags) {
            flag
        } else { 0 }
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

fn find_type_in_direction<'a>(ctx: Context, source: &Atom, direction: u32, flags: u32) -> bool {
    use std::ptr::eq;

    let (dx, dy) = offset(direction);
    let new_loc = (source.loc.0 as i32 + dx, source.loc.1 as i32 + dy);
    let (dim_y, dim_x) = ctx.grid.dim();
    if new_loc.0 < 0 || new_loc.1 < 0 || new_loc.0 >= dim_x as i32 || new_loc.1 >= dim_y as i32 {
        return flags & SMOOTH_BORDER != 0;
    }
    let new_loc = (new_loc.0 as u32, new_loc.1 as u32);

    // TODO: make this not call get_atom_list way too many times
    let atom_list = get_atom_list(ctx.objtree,
        &ctx.map.dictionary[&ctx.grid[ndarray::Dim([new_loc.1 as usize, new_loc.0 as usize])]],
        new_loc);
    let can_smooth_with = source.get_var_or("canSmoothWith", ctx.objtree, "null");
    if can_smooth_with == "null" {
        // smooth only with same type
        for atom in atom_list {
            if eq(atom.type_, source.type_) {
                return true;
            }
        }
    } else if flags & SMOOTH_MORE != 0 {
        // smooth with canSmoothWith + subtypes
        for atom in atom_list {
            let mut path = &atom.type_.path[..];
            while !path.is_empty() {
                if smoothlist_contains(can_smooth_with, path) {
                    return true;
                }
                path = &path[..path.rfind("/").unwrap()];
            }
        }
    } else {
        // smooth only with exact types in canSmoothWith
        for atom in atom_list {
            if smoothlist_contains(can_smooth_with, &atom.type_.path) {
                return true;
            }
        }
    }
    false
}

#[inline]
fn smoothlist_contains(list: &str, path: &str) -> bool {
    !path.is_empty() && (list.ends_with(path) ||
        list.contains(&format!("{}/obj/", path)) ||
        list.contains(&format!("{}/turf/", path)))
}

fn cardinal_smooth<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: u32) {
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

        let mut copy = source.clone();
        copy.set_var("icon_state", format!("{:?}", name));
        let smooth_icon = source.get_var("smooth_icon", ctx.objtree);
        if !smooth_icon.is_empty() {
            copy.set_var("icon", smooth_icon);
        }
        output.push(copy);
    }
}

fn diagonal_smooth<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: u32) {
    let presets = if adjacencies == N_NORTH|N_WEST {
        ["d-se", "d-se-0"]
    } else if adjacencies == N_NORTH|N_EAST {
        ["d-sw", "d-sw-0"]
    } else if adjacencies == N_SOUTH|N_WEST {
        ["d-ne", "d-ne-0"]
    } else if adjacencies == N_SOUTH|N_EAST {
        ["d-nw", "d-nw-0"]
    } else if adjacencies == N_NORTH|N_WEST|N_NORTHWEST {
        ["d-se", "d-se-1"]
    } else if adjacencies == N_NORTH|N_EAST|N_NORTHEAST {
        ["d-sw", "d-sw-1"]
    } else if adjacencies == N_SOUTH|N_WEST|N_SOUTHWEST {
        ["d-ne", "d-ne-1"]
    } else if adjacencies == N_SOUTH|N_EAST|N_SOUTHEAST {
        ["d-nw", "d-nw-1"]
    } else {
        return cardinal_smooth(output, ctx, source, adjacencies);
    };

    // turf underneath
    if subtype(&source.type_.path, "/turf/closed/wall/") {
        // BYOND memes
        if source.get_var("fixed_underlay", ctx.objtree) == "\"space\"1" {
            output.push(Atom::from_type(ctx.objtree, "/turf/open/space/basic", source.loc).unwrap());
        } else {
            let dir = flip(reverse_ndir(adjacencies));
            let mut needs_plating = true;
            // check direct, then 45deg left, then 45deg right
            'dirs: for &each in &[dir, left_45(dir), right_45(dir)] {
                let (dx, dy) = offset(each);
                let new_loc = (source.loc.0 as i32 + dx, source.loc.1 as i32 + dy);
                let (dim_y, dim_x) = ctx.grid.dim();
                if !(new_loc.0 < 0 || new_loc.1 < 0 || new_loc.0 >= dim_x as i32 || new_loc.1 >= dim_y as i32) {
                    let new_loc = (new_loc.0 as u32, new_loc.1 as u32);
                    // TODO: make this not call get_atom_list way too many times
                    let atom_list = get_atom_list(ctx.objtree,
                        &ctx.map.dictionary[&ctx.grid[ndarray::Dim([new_loc.1 as usize, new_loc.0 as usize])]],
                        new_loc);
                    for mut atom in atom_list {
                        if subtype(&atom.type_.path, "/turf/open/") {
                            atom.loc = source.loc;
                            output.push(atom);
                            needs_plating = false;
                            break 'dirs;
                        }
                    }
                }
            }
            if needs_plating {
                output.push(Atom::from_type(ctx.objtree, "/turf/open/floor/plating", source.loc).unwrap());
            }
        }
    }

    // the diagonal overlay
    for each in presets.iter() {
        let mut copy = source.clone();
        copy.set_var("icon_state", format!("{:?}", each));
        let smooth_icon = source.get_var("smooth_icon", ctx.objtree);
        if !smooth_icon.is_empty() {
            copy.set_var("icon", smooth_icon);
        }
        output.push(copy);
    }
}

fn offset(direction: u32) -> (i32, i32) {
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

fn flip(direction: u32) -> u32 {
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

fn reverse_ndir(ndir: u32) -> u32 {
    use dmi::*;
    const NW1: u32 = N_NORTH|N_WEST;
    const NW2: u32 = NW1|N_NORTHWEST;
    const NE1: u32 = N_NORTH|N_EAST;
    const NE2: u32 = NE1|N_NORTHEAST;
    const SW1: u32 = N_SOUTH|N_WEST;
    const SW2: u32 = SW1|N_SOUTHWEST;
    const SE1: u32 = N_SOUTH|N_EAST;
    const SE2: u32 = SE1|N_SOUTHEAST;

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

fn left_45(dir: u32) -> u32 {
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

fn right_45(dir: u32) -> u32 {
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

// ----------------------------------------------------------------------------
// Transit tube smoothing

fn generate_tube_overlays<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, source: &Atom<'a>) {
    use dmi::*;

    let dir = ::dmi::evaluate_dir(source.get_var_or("dir", ctx.objtree, "SOUTH"));
    let p = &source.type_.path;

    let mut fulfill = |items: &[u32]| {
        for &dir in items {
            if dir == NORTHEAST || dir == NORTHWEST || dir == SOUTHEAST || dir == SOUTHWEST {
                if dir & NORTH != 0 {
                    create_tube_overlay(output, ctx, source, dir ^ 3, NORTH);
                    if dir & EAST != 0 {
                        create_tube_overlay(output, ctx, source, dir ^ 12, EAST);
                    } else {
                        create_tube_overlay(output, ctx, source, dir ^ 12, WEST);
                    }
                }
            } else {
                create_tube_overlay(output, ctx, source, dir, 0);
            }
        }
    };

    if subtype(p, "/obj/structure/transit_tube/station/reverse/") {
        fulfill(&match dir {
            NORTH => [EAST],
            SOUTH => [WEST],
            EAST => [SOUTH],
            WEST => [NORTH],
            _ => return,
        })
    } else if subtype(p, "/obj/structure/transit_tube/station/") {
        fulfill(&match dir {
            NORTH | SOUTH => [EAST, WEST],
            EAST | WEST => [NORTH, SOUTH],
            _ => return,
        })
    } else if subtype(p, "/obj/structure/transit_tube/junction/flipped/") {
        fulfill(&match dir {
            NORTH => [NORTH, SOUTHWEST, SOUTHEAST],
            SOUTH => [SOUTH, NORTHEAST, NORTHWEST],
            EAST => [EAST, NORTHWEST, SOUTHWEST],
            WEST => [WEST, SOUTHEAST, NORTHEAST],
            _ => return,
        })
    } else if subtype(p, "/obj/structure/transit_tube/junction/") {
        fulfill(&match dir {
            NORTH => [NORTH, SOUTHEAST, SOUTHWEST],
            SOUTH => [SOUTH, NORTHWEST, NORTHEAST],
            EAST => [EAST, SOUTHWEST, NORTHWEST],
            WEST => [WEST, NORTHEAST, SOUTHEAST],
            _ => return,
        })
    } else if subtype(p, "/obj/structure/transit_tube/curved/flipped/") {
        fulfill(&match dir {
            NORTH => [NORTH, SOUTHEAST],
            SOUTH => [SOUTH, NORTHWEST],
            EAST => [EAST, SOUTHWEST],
            WEST => [NORTHEAST, WEST],
            _ => return,
        })
    } else if subtype(p, "/obj/structure/transit_tube/curved/") {
        fulfill(&match dir {
            NORTH => [NORTH, SOUTHWEST],
            SOUTH => [SOUTH, NORTHEAST],
            EAST => [EAST, NORTHWEST],
            WEST => [SOUTHEAST, WEST],
            _ => return,
        })
    } else if subtype(p, "/obj/structure/transit_tube/diagonal/") {
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

fn create_tube_overlay<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, source: &Atom<'a>, dir: u32, shift: u32) {
    use dmi::*;

    let mut copy = Atom::from_type(ctx.objtree, "/atom", source.loc).unwrap();
    copy.set_var("dir", format!("{}", dir));
    copy.set_var("layer", source.get_var("layer", ctx.objtree));
    copy.set_var("icon", source.get_var("icon", ctx.objtree));
    if shift != 0 {
        copy.set_var("icon_state", "\"decorative_diag\"");
        match shift {
            NORTH => copy.set_var("pixel_y", "32"),
            SOUTH => copy.set_var("pixel_y", "-32"),
            EAST => copy.set_var("pixel_x", "32"),
            WEST => copy.set_var("pixel_x", "-32"),
            _ => {}
        }
    } else {
        copy.set_var("icon_state", "\"decorative\"");
    }
    output.push(copy);
}
