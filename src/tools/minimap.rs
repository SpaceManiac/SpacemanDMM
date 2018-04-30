use std::path::Path;

use ndarray::{self, Axis};

use dm::objtree::*;
use dm::objtree::subpath as subtype;
use dm::constants::Constant;
use dmm::{Map, Grid, Prefab};
use dmi::Image;
use render_passes::RenderPass;
use icon_cache::IconCache;

const TILE_SIZE: u32 = 32;

// ----------------------------------------------------------------------------
// Main minimap code

#[derive(Clone, Copy)]
pub struct Context<'a> {
    pub objtree: &'a ObjectTree,
    pub map: &'a Map,
    pub grid: Grid<'a>,
    pub min: (usize, usize),
    pub max: (usize, usize),
    pub render_passes: &'a [Box<RenderPass>],
}

pub fn generate(
    ctx: Context,
    icon_cache: &IconCache,
) -> Result<Image, ()> {
    flame!("minimap");
    let Context { objtree, map, grid, render_passes, .. } = ctx;

    // transform min/max from bottom-left-based to top-left-based
    // probably doesn't belong here
    let (len_y, _) = ctx.grid.dim();
    let (min_y, max_y) = (len_y - ctx.max.1 - 1, len_y - ctx.min.1 - 1);
    let (len_x, len_y) = (ctx.max.0 - ctx.min.0 + 1, ctx.max.1 - ctx.min.1 + 1);

    // loads atoms from the prefabs on the map and adds overlays and smoothing
    let mut atoms = Vec::new();
    let mut overlays = Vec::new();

    //flame!("collect");
    for (y, row) in grid.axis_iter(Axis(0)).enumerate() {
        if y < min_y || y > max_y { continue }
        for (x, e) in row.iter().enumerate() {
            if x < ctx.min.0 || x > ctx.max.0 { continue }
            for mut atom in get_atom_list(objtree, &map.dictionary[e], (x as u32, y as u32), render_passes) {
                // icons which differ from their map states
                let p = &atom.type_.path;
                if p == "/obj/structure/table/wood/fancy/black" {
                    atom.set_var("icon", Constant::Resource("icons/obj/smooth_structures/fancy_table_black.dmi".into()));
                } else if p == "/obj/structure/table/wood/fancy" {
                    atom.set_var("icon", Constant::Resource("icons/obj/smooth_structures/fancy_table.dmi".into()));
                } else if subtype(p, "/turf/closed/mineral/") {
                    atom.set_var("pixel_x", Constant::Int(-4));
                    atom.set_var("pixel_y", Constant::Int(-4));
                }

                for pass in render_passes {
                    pass.adjust_vars(&mut atom, &objtree);
                }

                // overlays and underlays
                macro_rules! add_overlay {
                    ($icon:expr) => {{
                        let mut copy = atom.clone();
                        copy.set_var("icon_state", Constant::string($icon));
                        overlays.push(copy);
                    }}
                }
                if subtype(p, "/obj/structure/closet/") {
                    // closet doors
                    if atom.get_var("opened", objtree).to_bool() {
                        let var = if atom.get_var("icon_door_override", objtree).to_bool() { "icon_door" } else { "icon_state" };
                        if let &Constant::String(ref door) = atom.get_var(var, objtree) {
                            add_overlay!(format!("{}_open", door));
                        }
                    } else {
                        if let &Constant::String(ref door) = atom.get_var_notnull("icon_door", objtree)
                                .unwrap_or_else(|| atom.get_var("icon_state", objtree)) {
                            add_overlay!(format!("{}_door", door));
                        }
                        if atom.get_var("welded", objtree).to_bool() {
                            add_overlay!("welded");
                        }
                        if atom.get_var("secure", objtree).to_bool() && !atom.get_var("broken", objtree).to_bool() {
                            if atom.get_var("locked", objtree).to_bool() {
                                add_overlay!("locked");
                            } else {
                                add_overlay!("unlocked");
                            }
                        }
                    }
                } else if subtype(p, "/obj/machinery/computer/") || subtype(p, "/obj/machinery/power/solar_control/") {
                    // computer screens and keyboards
                    if let Some(screen) = atom.get_var_notnull("icon_screen", objtree) {
                        let mut copy = atom.clone();
                        copy.set_var("icon_state", screen.clone());
                        overlays.push(copy);
                    }
                    if let Some(keyboard) = atom.get_var_notnull("icon_keyboard", objtree) {
                        let mut copy = atom.clone();
                        copy.set_var("icon_state", keyboard.clone());
                        overlays.push(copy);
                    }
                } else if subtype(p, "/obj/machinery/door/airlock/") {
                    let mut copy = atom.clone();
                    if atom.get_var("glass", objtree).to_bool() {
                        copy.set_var("icon_state", Constant::string("glass_closed"));
                        copy.set_var("icon", atom.get_var("overlays_file", objtree).clone());
                    } else {
                        copy.set_var("icon_state", Constant::string("fill_closed"));
                    }
                    overlays.push(copy);
                } else if subtype(p, "/obj/machinery/atmospherics/components/unary/") {
                    let aboveground = match atom.get_var("icon_state", objtree) {
                        &Constant::String(ref text) => match &**text {
                            "vent_map" => "vent_off",
                            "vent_map_on" => "vent_out",
                            "vent_map_siphon_on" => "vent_in",
                            "scrub_map" => "scrub_off",
                            "scrub_map_on" => "scrub_on",
                            _ => "",
                        }
                        _ => "",
                    };
                    if !aboveground.is_empty() {
                        add_overlay!(aboveground);
                        atom.set_var("layer", Constant::Int(-5));
                    }
                } else if subtype(p, "/obj/machinery/power/apc/") {
                    use dmi::*;
                    // auto-set pixel location
                    match atom.get_var("dir", objtree) {
                        &Constant::Int(NORTH) => atom.set_var("pixel_y", Constant::Int(23)),
                        &Constant::Int(SOUTH) => atom.set_var("pixel_y", Constant::Int(-23)),
                        &Constant::Int(EAST) => atom.set_var("pixel_x", Constant::Int(24)),
                        &Constant::Int(WEST) => atom.set_var("pixel_x", Constant::Int(-25)),
                        _ => {}
                    }
                    // status overlays
                    for &each in ["apcox-1", "apco3-2", "apco0-3", "apco1-3", "apco2-3"].iter() {
                        add_overlay!(each);
                    }
                    // the terminal
                    let mut terminal = Atom::from_type(objtree, "/obj/machinery/power/terminal", atom.loc).unwrap();
                    terminal.copy_var("dir", &atom, objtree);
                    atoms.push(terminal);
                }

                for pass in render_passes {
                    pass.overlays(&mut atom, objtree, &mut atoms, &mut overlays);
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
        for pass in render_passes.iter() {
            if !pass.late_filter(&atom, objtree) {
                continue 'atom;
            }
        }

        let icon = match atom.get_var("icon", objtree) {
            &Constant::Resource(ref path) | &Constant::String(ref path) => path,
            _ => {
                println!("no icon: {}", atom.type_.path);
                continue
            }
        };
        let icon_state = match atom.get_var("icon_state", objtree) {
            &Constant::String(ref string) => string,
            _ => "",
        };
        let dir = atom.get_var("dir", objtree).to_int().unwrap_or(::dmi::SOUTH);

        let path: &Path = icon.as_ref();
        let icon_file = match icon_cache.retrieve_shared(path) {
            Some(icon_file) => icon_file,
            None => continue 'atom,
        };

        if let Some(mut rect) = icon_file.rect_of(&icon_state, dir) {
            let pixel_x = atom.get_var("pixel_x", ctx.objtree).to_int().unwrap_or(0);
            let pixel_y = atom.get_var("pixel_y", ctx.objtree).to_int().unwrap_or(0) +
                icon_file.metadata.height as i32;
            let mut loc = (
                ((atom.loc.0 - ctx.min.0 as u32) * TILE_SIZE) as i32 + pixel_x,
                ((atom.loc.1 + 1 - min_y as u32) * TILE_SIZE) as i32 - pixel_y
            );

            // OOB handling
            if loc.0 < 0 {
                rect.0 += (-loc.0) as u32;
                rect.2 -= (-loc.0) as u32;
                loc.0 = 0;
            }
            while loc.0 + rect.2 as i32 > map_image.size().0 as i32 {
                rect.2 -= 1;
                if rect.2 == 0 { continue 'atom }
            }
            if loc.1 < 0 {
                rect.1 += (-loc.1) as u32;
                rect.3 -= (-loc.1) as u32;
                loc.1 = 0;
            }
            while loc.1 + rect.3 as i32 > map_image.size().1 as i32 {
                rect.3 -= 1;
                if rect.3 == 0 { continue 'atom }
            }
            let loc = (loc.0 as u32, loc.1 as u32);

            // HTML color parsing
            let color = match atom.get_var("color", objtree) {
                &Constant::String(ref color) if color.starts_with("#") => {
                    let mut sum = 0;
                    for ch in color[1..color.len()].chars() {
                        sum = 16 * sum + ch.to_digit(16).unwrap();
                    }
                    if color.len() == 7 {  // #rrggbb
                        [(sum >> 16) as u8, (sum >> 8) as u8, sum as u8, 255]
                    } else if color.len() == 4 {  // #rgb
                        [(0x11 * ((sum >> 8) & 0xf)) as u8, (0x11 * ((sum >> 4) & 0xf)) as u8, (0x11 * (sum & 0xf)) as u8, 255]
                    } else {
                        [255, 255, 255, 255]  // invalid
                    }
                }
                _ => [255, 255, 255, 255],
            };

            // the real business
            map_image.composite(&icon_file.image, loc, rect, color);
        } else {
            //println!("Missing icon: type={}, icon={}, icon_state={}", atom.type_.path, icon, icon_state);
        }
    }

    Ok(map_image)
}

pub fn get_atom_list<'a>(
    objtree: &'a ObjectTree,
    prefabs: &'a [Prefab],
    loc: (u32, u32),
    render_passes: &[Box<RenderPass>],
) -> Vec<Atom<'a>> {
    flame!("get_atom_list");
    let mut result = Vec::new();

    'fab: for fab in prefabs {
        for pass in render_passes {
            if !pass.path_filter(&fab.path) {
                continue 'fab;
            }
        }

        // look up the type
        let atom = match Atom::from_prefab(objtree, fab, loc) {
            Some(x) => x,
            None => {
                println!("Warning: missing {:?}", fab.path);
                continue
            }
        };

        for pass in render_passes {
            if !pass.early_filter(&atom, objtree) {
                continue 'fab;
            }
        }

        // convert structure spanwers to their structures
        for pass in render_passes {
            if pass.expand(&atom, objtree, &mut result) {
                continue 'fab;
            }
        }
        result.push(atom);
    }

    result
}

// ----------------------------------------------------------------------------
// Atoms and related utilities

static NULL: Constant = Constant::Null(None);

#[derive(Debug, Clone)]
pub struct Atom<'a> {
    type_: &'a Type,
    prefab: Option<&'a Vars>,
    vars: Vars,
    pub loc: (u32, u32),
}

impl<'a> Atom<'a> {
    pub fn from_prefab(objtree: &'a ObjectTree, fab: &'a Prefab, loc: (u32, u32)) -> Option<Self> {
        objtree.find(&fab.path).map(|type_| Atom {
            type_: type_.get(),
            prefab: Some(&fab.vars),
            vars: Default::default(),
            loc,
        })
    }

    pub fn from_type(objtree: &'a ObjectTree, path: &str, loc: (u32, u32)) -> Option<Self> {
        objtree.find(path).map(|type_| Atom {
            type_: type_.get(),
            prefab: None,
            vars: Default::default(),
            loc,
        })
    }

    pub fn from_type_ref(type_: &'a Type, loc: (u32, u32)) -> Self {
        Atom {
            type_: type_,
            prefab: None,
            vars: Default::default(),
            loc
        }
    }

    pub fn istype(&self, parent: &str) -> bool {
        subpath(&self.type_.path, parent)
    }

    pub fn get_var(&self, key: &str, objtree: &'a ObjectTree) -> &Constant {
        self.get_var_spec(key, objtree).unwrap_or(&NULL)
    }

    pub fn get_var_notnull(&self, key: &str, objtree: &'a ObjectTree) -> Option<&Constant> {
        match self.get_var_spec(key, objtree) {
            None | Some(&Constant::Null(_)) => None,
            Some(other) => Some(other)
        }
    }

    fn get_var_spec(&self, key: &str, objtree: &'a ObjectTree) -> Option<&Constant> {
        if let Some(v) = self.vars.get(key) {
            return Some(v);
        }
        if let Some(ref prefab) = self.prefab {
            if let Some(v) = prefab.get(key) {
                return Some(v);
            }
        }
        let mut current = Some(self.type_);
        while let Some(t) = current.take() {
            if let Some(v) = t.vars.get(key) {
                return Some(v.value.constant.as_ref().unwrap_or(&NULL));
            }
            current = objtree.parent_of(t);
        }
        None
    }

    pub fn copy_var(&mut self, key: &str, from: &Atom, objtree: &'a ObjectTree) {
        if let Some(var) = from.get_var_notnull(key, objtree) {
            self.set_var(key, var.clone());
        }
    }

    pub fn set_var<K: Into<String>>(&mut self, key: K, value: Constant) {
        self.vars.insert(key.into(), value);
    }
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
    } else if subtype(p, "/obj/machinery/atmospherics/pipe/") && !p.contains("visible") {
        -5_000
    } else if subtype(p, "/obj/structure/cable/") {
        -4_000
    } else if subtype(p, "/obj/machinery/power/terminal/") {
        -3_500
    } else if subtype(p, "/obj/structure/lattice/") {
        -8_000
    } else if subtype(p, "/obj/machinery/navbeacon/") {
        -3_000
    } else {
        match atom.get_var("layer", objtree) {
            &Constant::Int(i) => (i % 1000) * 1000,
            &Constant::Float(f) => ((f % 1000.) * 1000.) as i32,
            other => panic!("not a layer: {:?}", other),
        }
    }
}

// ----------------------------------------------------------------------------
// Icon smoothing subsystem

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

fn handle_smooth<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, atom: Atom<'a>, mask: i32) {
    let smooth_flags = mask & atom.get_var("smooth", ctx.objtree).to_int().unwrap_or(0);
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

fn calculate_adjacencies(ctx: Context, atom: &Atom, flags: i32) -> i32 {
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

fn find_type_in_direction<'a>(ctx: Context, source: &Atom, direction: i32, flags: i32) -> bool {
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
        new_loc, ctx.render_passes);
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

fn cardinal_smooth<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: i32) {
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
        copy.set_var("icon_state", Constant::string(name));
        if let Some(icon) = source.get_var_notnull("smooth_icon", ctx.objtree) {
            copy.set_var("icon", icon.clone());
        }
        output.push(copy);
    }
}

fn diagonal_smooth<'a>(output: &mut Vec<Atom<'a>>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: i32) {
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
        if source.get_var("fixed_underlay", ctx.objtree).index(&Constant::string("space")).is_some() {
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
                        new_loc, ctx.render_passes);
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
    for &each in presets.iter() {
        let mut copy = source.clone();
        copy.set_var("icon_state", Constant::string(each));
        if let Some(icon) = source.get_var_notnull("smooth_icon", ctx.objtree) {
            copy.set_var("icon", icon.clone());
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
    const NW1: i32 = N_NORTH|N_WEST;
    const NW2: i32 = NW1|N_NORTHWEST;
    const NE1: i32 = N_NORTH|N_EAST;
    const NE2: i32 = NE1|N_NORTHEAST;
    const SW1: i32 = N_SOUTH|N_WEST;
    const SW2: i32 = SW1|N_SOUTHWEST;
    const SE1: i32 = N_SOUTH|N_EAST;
    const SE2: i32 = SE1|N_SOUTHEAST;

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

