use std::sync::RwLock;
use std::collections::HashSet;

use ndarray::Axis;

use dm::objtree::*;
use dm::objtree::subpath as subtype;
use dm::constants::Constant;
use dmm::{Map, Grid, Prefab};
use dmi::Image;
use render_passes::{RenderPass, icon_smoothing};
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
    pub bump: &'a bumpalo::Bump,
}

pub fn generate(ctx: Context, icon_cache: &IconCache) -> Result<Image, ()> {
    let Context {
        objtree,
        map,
        grid,
        render_passes,
        bump,
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
                for pass in render_passes.iter() {
                    // Note that late_filter is NOT called during smoothing lookups.
                    if !pass.late_filter(&atom, objtree) {
                        continue 'atom;
                    }
                }
                let mut sprite = Sprite::from_vars(objtree, &atom);
                for pass in render_passes {
                    pass.adjust_sprite(&atom, &mut sprite, objtree, bump);
                }
                if sprite.icon.is_empty() {
                    println!("no icon: {}", atom.type_.path);
                    continue;
                }
                atom.sprite = sprite;

                // icons which differ from their map states
                let p = &atom.type_.path;
                if subtype(p, "/turf/closed/mineral/") {
                    atom.sprite.ofs_x -= 4;
                    atom.sprite.ofs_y -= 4;
                }

                for pass in render_passes {
                    pass.overlays(&mut atom, objtree, &mut underlays, &mut overlays, bump);
                }

                // smoothing time
                let loc = atom.loc;
                sprites.extend(underlays.drain(..).map(|o| (loc, o)));
                icon_smoothing::handle_smooth(&mut sprites, ctx, atom, !0);
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
            if !pass.expand(&atom, objtree, &mut result) {
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
    pub(crate) type_: &'a Type,
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

/// Information about when a sprite should be shown or hidden.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct Category {
    raw: u32,
}

impl Category {
    const AREA: Category = Category { raw: 1 };
    const TURF: Category = Category { raw: 2 };
    const OBJ: Category = Category { raw: 3 };
    const MOB: Category = Category { raw: 4 };

    ///
    pub fn from_path(path: &str) -> Category {
        if path.starts_with("/area") {
            Category::AREA
        } else if path.starts_with("/turf") {
            Category::TURF
        } else if path.starts_with("/obj") {
            Category::OBJ
        } else if path.starts_with("/mob") {
            Category::MOB
        } else {
            Category { raw: 0 }
        }
    }

    /// Encode this category for FFI representation.
    pub fn encode(self) -> u32 {
        self.raw
    }
}

/// A guaranteed sortable representation of a `layer` float.
#[derive(Default, Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Layer {
    whole: i16,
    frac: u16,
}

impl Layer {
    /// Encode this layer as an `i32` for FFI representation.
    pub fn encode(self) -> i32 {
        ((self.whole as i32) << 16) | (self.frac as i32)
    }
}

impl From<i16> for Layer {
    fn from(whole: i16) -> Layer {
        Layer { whole, frac: 0 }
    }
}

impl From<i32> for Layer {
    fn from(whole: i32) -> Layer {
        use std::convert::TryFrom;
        Layer { whole: i16::try_from(whole).expect("layer out of range"), frac: 0 }
    }
}

impl From<f32> for Layer {
    fn from(f: f32) -> Layer {
        Layer { whole: f as i16, frac: ((f.fract() + 1.).fract() * 65536.) as u16 }
    }
}

/// A Sprite is a fragment of an atom's appearance.
///
/// Every atom has a default sprite, which may be disabled, and a list of
/// overlays.
#[derive(Debug, Clone)]
pub struct Sprite<'s> {
    // filtering
    pub category: Category,

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
    pub layer: Layer,
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
            category: Category::from_path(vars.get_path()),
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
            category: Category::default(),
            icon: "",
            icon_state: "",
            dir: 0,
            color: [255, 255, 255, 255],
            ofs_x: 0,
            ofs_y: 0,
            plane: 0,
            layer: Layer::default(),
        }
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

pub(crate) fn layer_of<'s, T: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, atom: &T) -> Layer {
    match atom.get_var("layer", objtree) {
        &Constant::Int(i) => Layer::from(i),
        &Constant::Float(f) => Layer::from(f),
        other => {
            eprintln!("not a layer: {:?} on {:?}", other, atom.get_path());
            Layer::from(2)
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
