use std::sync::RwLock;
use std::collections::HashSet;

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
    pub render_passes: &'a [Box<dyn RenderPass>],
    pub errors: &'a RwLock<HashSet<String>>,
}

pub fn generate(ctx: Context, icon_cache: &IconCache) -> Result<Image, ()> {
    let Context {
        objtree,
        map,
        grid,
        render_passes,
        ..
    } = ctx;

    // transform min/max from bottom-left-based to top-left-based
    // probably doesn't belong here
    let (len_y, _) = ctx.grid.dim();
    let (min_y, max_y) = (len_y - ctx.max.1 - 1, len_y - ctx.min.1 - 1);
    let (len_x, len_y) = (ctx.max.0 - ctx.min.0 + 1, ctx.max.1 - ctx.min.1 + 1);

    // loads atoms from the prefabs on the map and adds overlays and smoothing
    let mut sprites = Vec::new();
    let mut underlays = Vec::new();
    let mut overlays = Vec::new();

    for (y, row) in grid.axis_iter(Axis(0)).enumerate() {
        if y < min_y || y > max_y {
            continue;
        }
        for (x, e) in row.iter().enumerate() {
            if x < ctx.min.0 || x > ctx.max.0 {
                continue;
            }
            'atom: for mut atom in get_atom_list(objtree, &map.dictionary[e], (x as u32, y as u32), render_passes, Some(ctx.errors)) {
                for pass in render_passes {
                    pass.adjust_vars(&mut atom, &objtree);
                }
                for pass in render_passes.iter() {
                    // Note that late_filter is NOT called during smoothing lookups.
                    if !pass.late_filter(&atom, objtree) {
                        continue 'atom;
                    }
                }
                let mut sprite = Sprite::from_vars(objtree, &atom);
                for pass in render_passes {
                    pass.adjust_sprite(&atom, &mut sprite, objtree);
                }
                if sprite.icon.is_empty() {
                    println!("no icon: {}", atom.type_.path);
                    continue;
                }
                atom.sprite = sprite;

                // icons which differ from their map states
                let p = &atom.type_.path;
                if p == "/obj/structure/table/wood/fancy/black" {
                    atom.sprite.icon = "icons/obj/smooth_structures/fancy_table_black.dmi";
                } else if p == "/obj/structure/table/wood/fancy" {
                    atom.sprite.icon = "icons/obj/smooth_structures/fancy_table.dmi";
                } else if subtype(p, "/turf/closed/mineral/") {
                    atom.sprite.ofs_x -= 4;
                    atom.sprite.ofs_y -= 4;
                }

                // overlays and underlays
                macro_rules! add_overlay {
                    ($icon:expr) => {{
                        overlays.push(Sprite {
                            icon_state: $icon,
                            .. atom.sprite
                        });
                    }};
                }
                if subtype(p, "/obj/structure/closet/") {
                    // closet doors
                    if atom.get_var("opened", objtree).to_bool() {
                        let var = if atom.get_var("icon_door_override", objtree).to_bool() {
                            "icon_door"
                        } else {
                            "icon_state"
                        };
                        if let &Constant::String(ref door) = atom.get_var(var, objtree) {
                            // TODO: fix leak
                            add_overlay!(Box::leak(format!("{}_open", door).into_boxed_str()));
                        }
                    } else {
                        if let &Constant::String(ref door) = atom
                            .get_var_notnull("icon_door", objtree)
                            .unwrap_or_else(|| atom.get_var("icon_state", objtree))
                        {
                            // TODO: fix leak
                            add_overlay!(Box::leak(format!("{}_door", door).into_boxed_str()));
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
                    if let Some(screen) = atom.get_var("icon_screen", objtree).as_str() {
                        add_overlay!(screen);
                    }
                    if let Some(keyboard) = atom.get_var("icon_keyboard", objtree).as_str() {
                        add_overlay!(keyboard);
                    }
                } else if subtype(p, "/obj/machinery/door/airlock/") {
                    if atom.get_var("glass", objtree).to_bool() {
                        overlays.push(Sprite {
                            icon: atom.get_var("overlays_file", objtree).as_path_str().unwrap_or(""),
                            icon_state: "glass_closed",
                            .. atom.sprite
                        })
                    } else {
                        add_overlay!("fill_closed");
                    }
                } else if subtype(p, "/obj/machinery/atmospherics/components/unary/") {
                    let aboveground = match atom.get_var("icon_state", objtree) {
                        &Constant::String(ref text) => match &**text {
                            "vent_map" => "vent_off",
                            "vent_map_on" => "vent_out",
                            "vent_map_siphon_on" => "vent_in",
                            "scrub_map" => "scrub_off",
                            "scrub_map_on" => "scrub_on",
                            _ => "",
                        },
                        _ => "",
                    };
                    if !aboveground.is_empty() {
                        add_overlay!(aboveground);
                        atom.sprite.layer = -5_000;
                    }
                } else if subtype(p, "/obj/machinery/power/apc/") {
                    use dmi::*;
                    // auto-set pixel location
                    match atom.get_var("dir", objtree) {
                        &Constant::Int(NORTH) => atom.sprite.ofs_y = 23,
                        &Constant::Int(SOUTH) => atom.sprite.ofs_y = -23,
                        &Constant::Int(EAST) => atom.sprite.ofs_x = 24,
                        &Constant::Int(WEST) => atom.sprite.ofs_x = -25,
                        _ => {}
                    }
                    // status overlays
                    for &each in ["apcox-1", "apco3-2", "apco0-3", "apco1-3", "apco2-3"].iter() {
                        add_overlay!(each);
                    }

                    // APC terminals
                    let mut terminal = Sprite::from_vars(objtree, &objtree.expect("/obj/machinery/power/terminal"));
                    terminal.dir = atom.sprite.dir;
                    // TODO: un-hack this
                    ::render_passes::apply_fancy_layer("/obj/machinery/power/terminal", &mut terminal);
                    underlays.push(terminal);
                }

                for pass in render_passes {
                    pass.overlays(&mut atom, objtree, &mut underlays, &mut overlays);
                }

                // smoothing time
                let loc = atom.loc;
                sprites.extend(underlays.drain(..).map(|o| (loc, o)));
                handle_smooth(&mut sprites, ctx, atom, !0);
                sprites.extend(overlays.drain(..).map(|o| (loc, o)));
            }
        }
    }
    drop(underlays);
    drop(overlays);

    // sorts the atom list and renders them onto the output image
    sprites.sort_by_key(|(_, s)| (s.plane, s.layer));

    let mut map_image = Image::new_rgba(len_x as u32 * TILE_SIZE, len_y as u32 * TILE_SIZE);
    for (loc, sprite) in sprites {
        let icon_file = match icon_cache.retrieve_shared(sprite.icon.as_ref()) {
            Some(icon_file) => icon_file,
            None => continue,
        };

        if let Some(rect) = icon_file.rect_of(sprite.icon_state, sprite.dir) {
            let pixel_x = sprite.ofs_x;
            let pixel_y = sprite.ofs_y + icon_file.metadata.height as i32;
            let loc = (
                ((loc.0 - ctx.min.0 as u32) * TILE_SIZE) as i32 + pixel_x,
                ((loc.1 + 1 - min_y as u32) * TILE_SIZE) as i32 - pixel_y,
            );

            if let Some((loc, rect)) = clip((map_image.width, map_image.height), loc, rect) {
                map_image.composite(&icon_file.image, loc, rect, sprite.color);
            }
        } else {
            let key = format!("bad icon: {:?}, state: {:?}", sprite.icon, sprite.icon_state);
            if !ctx.errors.read().unwrap().contains(&key) {
                println!("{}", key);
                ctx.errors.write().unwrap().insert(key);
            }
        }
    }

    Ok(map_image)
}

// OOB handling
fn clip(bounds: (u32, u32), mut loc: (i32, i32), mut rect: (u32, u32, u32, u32)) -> Option<((u32, u32), (u32, u32, u32, u32))> {
    if loc.0 < 0 {
        rect.0 += (-loc.0) as u32;
        match rect.2.checked_sub((-loc.0) as u32) {
            Some(s) => rect.2 = s,
            None => return None,  // out of the viewport
        }
        loc.0 = 0;
    }
    while loc.0 + rect.2 as i32 > bounds.0 as i32 {
        rect.2 -= 1;
        if rect.2 == 0 {
            return None;
        }
    }
    if loc.1 < 0 {
        rect.1 += (-loc.1) as u32;
        match rect.3.checked_sub((-loc.1) as u32) {
            Some(s) => rect.3 = s,
            None => return None,  // out of the viewport
        }
        loc.1 = 0;
    }
    while loc.1 + rect.3 as i32 > bounds.1 as i32 {
        rect.3 -= 1;
        if rect.3 == 0 {
            return None;
        }
    }
    Some(((loc.0 as u32, loc.1 as u32), rect))
}

pub fn get_atom_list<'a>(
    objtree: &'a ObjectTree,
    prefabs: &'a [Prefab],
    loc: (u32, u32),
    render_passes: &[Box<dyn RenderPass>],
    errors: Option<&RwLock<HashSet<String>>>,
) -> Vec<Atom<'a>> {
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
                if let Some(errors) = errors {
                    let key = format!("bad path: {}", fab.path);
                    if !errors.read().unwrap().contains(&key) {
                        println!("{}", key);
                        errors.write().unwrap().insert(key);
                    }
                }
                continue;
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
// Atoms

#[derive(Debug, Clone)]
pub struct Atom<'a> {
    type_: &'a Type,
    prefab: Option<&'a Vars>,
    pub sprite: Sprite<'a>,
    pub loc: (u32, u32),
}

impl<'a> Atom<'a> {
    pub fn from_prefab(objtree: &'a ObjectTree, fab: &'a Prefab, loc: (u32, u32)) -> Option<Self> {
        objtree.find(&fab.path).map(|type_| Atom {
            type_: type_.get(),
            prefab: Some(&fab.vars),
            sprite: Sprite::default(),
            loc,
        })
    }

    pub fn from_type(objtree: &'a ObjectTree, path: &str, loc: (u32, u32)) -> Option<Self> {
        objtree.find(path).map(|type_| Atom {
            type_: type_.get(),
            prefab: None,
            sprite: Sprite::default(),
            loc,
        })
    }

    pub fn from_type_ref(type_: &'a Type, loc: (u32, u32)) -> Self {
        Atom {
            type_: type_,
            prefab: None,
            sprite: Sprite::default(),
            loc,
        }
    }

    pub fn path(&self) -> &str {
        &self.type_.path
    }

    pub fn istype(&self, parent: &str) -> bool {
        subpath(&self.type_.path, parent)
    }

    pub fn set_var<K: Into<String>>(&mut self, _: K, _: Constant) {
        // TODO: remove all references
    }
}

// ----------------------------------------------------------------------------
// Vars abstraction

pub trait GetVar<'a> {
    fn get_path(&self) -> &str;

    fn get_var(&self, key: &str, objtree: &'a ObjectTree) -> &'a Constant {
        self.get_var_inner(key, objtree).unwrap_or(Constant::null())
    }

    fn get_var_notnull(&self, key: &str, objtree: &'a ObjectTree) -> Option<&'a Constant> {
        match self.get_var_inner(key, objtree) {
            None | Some(&Constant::Null(_)) => None,
            Some(other) => Some(other),
        }
    }

    fn get_var_inner(&self, key: &str, objtree: &'a ObjectTree) -> Option<&'a Constant>;
}

impl<'a> GetVar<'a> for Atom<'a> {
    fn get_path(&self) -> &str {
        &self.type_.path
    }

    fn get_var_inner(&self, key: &str, objtree: &'a ObjectTree) -> Option<&'a Constant> {
        if let Some(ref prefab) = self.prefab {
            if let Some(v) = prefab.get(key) {
                return Some(v);
            }
        }
        let mut current = Some(self.type_);
        while let Some(t) = current.take() {
            if let Some(v) = t.vars.get(key) {
                return Some(v.value.constant.as_ref().unwrap_or(Constant::null()));
            }
            current = objtree.parent_of(t);
        }
        None
    }
}

impl<'a> GetVar<'a> for &'a Prefab {
    fn get_path(&self) -> &str {
        &self.path
    }

    fn get_var_inner(&self, key: &str, objtree: &'a ObjectTree) -> Option<&'a Constant> {
        if let Some(v) = self.vars.get(key) {
            return Some(v);
        }
        let mut current = objtree.find(&self.path);
        while let Some(t) = current.take() {
            if let Some(v) = t.get().vars.get(key) {
                return Some(v.value.constant.as_ref().unwrap_or(Constant::null()));
            }
            current = t.parent_type();
        }
        None
    }
}

impl<'a> GetVar<'a> for &'a Type {
    fn get_path(&self) -> &str {
        &self.path
    }

    fn get_var_inner(&self, key: &str, objtree: &'a ObjectTree) -> Option<&'a Constant> {
        let mut current = Some(*self);
        while let Some(t) = current.take() {
            if let Some(v) = t.vars.get(key) {
                return Some(v.value.constant.as_ref().unwrap_or(Constant::null()));
            }
            current = objtree.parent_of(t);
        }
        None
    }
}

impl<'a> GetVar<'a> for TypeRef<'a> {
    fn get_path(&self) -> &str {
        &self.path
    }

    fn get_var_inner(&self, key: &str, _: &'a ObjectTree) -> Option<&'a Constant> {
        let mut current = Some(*self);
        while let Some(t) = current.take() {
            if let Some(v) = t.get().vars.get(key) {
                return Some(v.value.constant.as_ref().unwrap_or(Constant::null()));
            }
            current = t.parent_type();
        }
        None
    }
}

// ----------------------------------------------------------------------------
// Renderer-agnostic sprite structure

/// A Sprite is a fragment of an atom's appearance.
///
/// Every atom has a default sprite, which may be disabled, and a list of
/// overlays.
#[derive(Debug, Clone)]
pub struct Sprite<'s> {
    // filtering
    pub category: u32,  // type

    // visual appearance
    pub icon: &'s str,
    pub icon_state: &'s str,
    pub dir: i32,
    pub color: [u8; 4],  // [r, g, b, a]

    // position
    pub ofs_x: i32,  // pixel_x + pixel_w + step_x
    pub ofs_y: i32,  // pixel_y + pixel_z + step_y

    // sorting
    pub plane: i32,
    pub layer: i32,
}

impl<'s> Sprite<'s> {
    pub fn from_vars<V: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, vars: &V) -> Sprite<'s> {
        let pixel_x = vars.get_var("pixel_x", objtree).to_int().unwrap_or(0);
        let pixel_y = vars.get_var("pixel_y", objtree).to_int().unwrap_or(0);
        let pixel_w = vars.get_var("pixel_w", objtree).to_int().unwrap_or(0);
        let pixel_z = vars.get_var("pixel_z", objtree).to_int().unwrap_or(0);
        let step_x = vars.get_var("step_x", objtree).to_int().unwrap_or(0);
        let step_y = vars.get_var("step_y", objtree).to_int().unwrap_or(0);

        Sprite {
            category: category_of(vars.get_path()),
            icon: vars.get_var("icon", objtree).as_path_str().unwrap_or(""),
            icon_state: vars.get_var("icon_state", objtree).as_str().unwrap_or(""),
            dir: vars.get_var("dir", objtree).to_int().unwrap_or(::dmi::SOUTH),
            color: color_of(objtree, vars),
            ofs_x: pixel_x + pixel_w + step_x,
            ofs_y: pixel_y + pixel_z + step_y,
            plane: plane_of(objtree, vars),
            layer: layer_of(objtree, vars),
        }
    }
}

impl<'s> Default for Sprite<'s> {
    fn default() -> Self {
        Sprite {
            category: 0,
            icon: "",
            icon_state: "",
            dir: 0,
            color: [255, 255, 255, 255],
            ofs_x: 0,
            ofs_y: 0,
            plane: 0,
            layer: 0,
        }
    }
}

fn category_of(path: &str) -> u32 {
    if path.starts_with("/area") {
        1
    } else if path.starts_with("/turf") {
        2
    } else if path.starts_with("/obj") {
        3
    } else if path.starts_with("/mob") {
        4
    } else {
        0
    }
}

fn plane_of<'s, T: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, atom: &T) -> i32 {
    match atom.get_var("plane", objtree) {
        &Constant::Int(i) => i,
        other => {
            eprintln!("not a plane: {:?} on {:?}", other, atom.get_path());
            0
        }
    }
}

fn layer_of<'s, T: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, atom: &T) -> i32 {
    match atom.get_var("layer", objtree) {
        &Constant::Int(i) => (i % 1000) * 1000,
        &Constant::Float(f) => ((f % 1000.) * 1000.) as i32,
        other => {
            eprintln!("not a layer: {:?} on {:?}", other, atom.get_path());
            2_000
        }
    }
}

pub fn color_of<'s, T: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, atom: &T) -> [u8; 4] {
    let alpha = match atom.get_var("alpha", objtree) {
        &Constant::Int(i) if i >= 0 && i <= 255 => i as u8,
        _ => 255,
    };

    match atom.get_var("color", objtree) {
        &Constant::String(ref color) if color.starts_with("#") => {
            let mut sum = 0;
            for ch in color[1..color.len()].chars() {
                sum = 16 * sum + ch.to_digit(16).unwrap_or(0);
            }
            if color.len() == 7 {  // #rrggbb
                [(sum >> 16) as u8, (sum >> 8) as u8, sum as u8, alpha]
            } else if color.len() == 4 {  // #rgb
                [
                    (0x11 * ((sum >> 8) & 0xf)) as u8,
                    (0x11 * ((sum >> 4) & 0xf)) as u8,
                    (0x11 * (sum & 0xf)) as u8,
                    alpha,
                ]
            } else {
                [255, 255, 255, alpha]  // invalid
            }
        }
        &Constant::String(ref color) if color == "red" => [255, 0, 0, alpha],
        &Constant::String(ref color) if color == "green" => [0, 255, 0, alpha],
        &Constant::String(ref color) if color == "blue" => [0, 0, 255, alpha],
        &Constant::String(ref color) if color == "black" => [0, 0, 0, alpha],
        &Constant::String(ref color) if color == "white" => [255, 255, 255, alpha],
        &Constant::String(ref color) if color == "yellow" => [255, 255, 0, alpha],
        &Constant::String(ref color) if color == "cyan" => [0, 255, 255, alpha],
        &Constant::String(ref color) if color == "magenta" => [255, 0, 255, alpha],
        // TODO: color matrix support?
        _ => [255, 255, 255, alpha],
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

fn handle_smooth<'a>(output: &mut Vec<((u32, u32), Sprite<'a>)>, ctx: Context<'a>, atom: Atom<'a>, mask: i32) {
    let smooth_flags = mask & atom.get_var("smooth", ctx.objtree).to_int().unwrap_or(0);
    if smooth_flags & (SMOOTH_TRUE | SMOOTH_MORE) != 0 {
        let adjacencies = calculate_adjacencies(ctx, &atom, smooth_flags);
        if smooth_flags & SMOOTH_DIAGONAL != 0 {
            diagonal_smooth(output, ctx, &atom, adjacencies);
        } else {
            cardinal_smooth(output, ctx, &atom, adjacencies);
        }
    } else {
        output.push((atom.loc, atom.sprite));
    }
}

fn calculate_adjacencies(ctx: Context, atom: &Atom, flags: i32) -> i32 {
    use dmi::*;
    // TODO: anchored check

    let mut adjacencies = 0;
    let check_one = |direction, flag| {
        if find_type_in_direction(ctx, atom, direction, flags) {
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
    let atom_list = get_atom_list(
        ctx.objtree,
        &ctx.map.dictionary[&ctx.grid[ndarray::Dim([new_loc.1 as usize, new_loc.0 as usize])]],
        new_loc,
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

fn cardinal_smooth<'a>(output: &mut Vec<((u32, u32), Sprite<'a>)>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: i32) {
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
            // TODO: fix leak
            icon_state: Box::leak(name.into_boxed_str()),
            .. source.sprite
        };
        if let Some(icon) = source.get_var("smooth_icon", ctx.objtree).as_path_str() {
            // TODO: fix leak
            sprite.icon = Box::leak(icon.to_owned().into_boxed_str());
        }
        output.push((source.loc, sprite));
    }
}

fn diagonal_smooth<'a>(output: &mut Vec<((u32, u32), Sprite<'a>)>, ctx: Context<'a>, source: &Atom<'a>, adjacencies: i32) {
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
    if subtype(&source.type_.path, "/turf/closed/wall/") {
        // BYOND memes
        if source
            .get_var("fixed_underlay", ctx.objtree)
            .index(&Constant::string("space"))
            .is_some()
        {
            output.push((
                source.loc,
                Sprite::from_vars(ctx.objtree, &ctx.objtree.expect("/turf/open/space/basic"))
            ));
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
                    let atom_list = get_atom_list(
                        ctx.objtree,
                        &ctx.map.dictionary[&ctx.grid[ndarray::Dim([new_loc.1 as usize, new_loc.0 as usize])]],
                        new_loc,
                        ctx.render_passes,
                        None,
                    );
                    for atom in atom_list {
                        if subtype(&atom.type_.path, "/turf/open/") {
                            output.push((source.loc, Sprite::from_vars(ctx.objtree, &atom)));
                            needs_plating = false;
                            break 'dirs;
                        }
                    }
                }
            }
            if needs_plating {
                output.push((
                    source.loc,
                    Sprite::from_vars(ctx.objtree, &ctx.objtree.expect("/turf/open/floor/plating"))
                ));
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
            // TODO: fix leak
            copy.icon = Box::leak(icon.to_owned().into_boxed_str());
        }
        output.push((source.loc, copy));
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
