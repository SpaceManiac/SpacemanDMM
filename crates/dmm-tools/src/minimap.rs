use std::collections::BTreeMap;
use std::sync::RwLock;

use ndarray::Axis;

use crate::dmi::{self, Dir, Image};
use crate::dmm::{Map, Prefab, ZLevel};
use crate::icon_cache::IconCache;
use crate::render_passes::RenderPass;
use dm::constants::Constant;
use dm::objtree::*;

use foldhash::HashSet;

const TILE_SIZE: u32 = 32;

// ----------------------------------------------------------------------------
// Main minimap code

#[derive(Clone, Copy)]
pub struct Context<'a> {
    pub objtree: &'a ObjectTree,
    pub map: &'a Map,
    pub level: ZLevel<'a>,
    pub min: (usize, usize),
    pub max: (usize, usize),
    pub render_passes: &'a [Box<dyn RenderPass>],
    pub errors: &'a RwLock<HashSet<String>>,
    pub bump: &'a bumpalo::Bump,
}

// This should eventually be faliable and not just shrug it's shoulders at errors and log them.
#[allow(clippy::result_unit_err)]
pub fn generate(ctx: Context, icon_cache: &IconCache) -> Result<Image, ()> {
    let Context {
        objtree,
        map,
        level,
        render_passes,
        bump,
        ..
    } = ctx;

    // transform min/max from bottom-left-based to top-left-based
    // probably doesn't belong here
    let (len_y, _) = level.grid.dim();
    let (min_y, max_y) = (len_y - ctx.max.1 - 1, len_y - ctx.min.1 - 1);
    let (len_x, len_y) = (ctx.max.0 - ctx.min.0 + 1, ctx.max.1 - ctx.min.1 + 1);

    // create atom arrays from the map dictionary
    let mut atoms = BTreeMap::new();
    for (key, prefabs) in map.dictionary.iter() {
        atoms.insert(key, get_atom_list(objtree, prefabs, render_passes, ctx.errors));
    }

    // loads atoms from the prefabs on the map and adds overlays and smoothing
    let mut sprites = Vec::new();
    let mut underlays = Vec::new();
    let mut overlays = Vec::new();

    for (y, row) in level.grid.axis_iter(Axis(0)).enumerate() {
        if y < min_y || y > max_y {
            continue;
        }
        for (x, e) in row.iter().enumerate() {
            if x < ctx.min.0 || x > ctx.max.0 {
                continue;
            }

            let loc = (x as u32, y as u32);

            'atom: for atom in atoms.get(e).expect("bad key").iter() {
                for pass in render_passes.iter() {
                    // Note that late_filter is NOT called during smoothing lookups.
                    if !pass.late_filter(atom, objtree) {
                        continue 'atom;
                    }
                }
                let mut sprite = Sprite::from_vars(objtree, atom);
                for pass in render_passes {
                    pass.adjust_sprite(atom, &mut sprite, objtree, bump);
                }
                if sprite.icon.is_empty() {
                    println!("no icon: {}", atom.type_.path);
                    continue;
                }
                let atom = Atom { sprite, .. *atom };

                for pass in render_passes {
                    pass.overlays(&atom, objtree, &mut underlays, &mut overlays, bump);
                }

                // smoothing time
                let mut neighborhood = [&[][..]; 9];
                for (i, (dx, dy)) in [
                    (-1,  1), (0,  1), (1,  1),
                    (-1,  0), (0,  0), (1,  0),
                    (-1, -1), (0, -1), (1, -1),
                ].iter().enumerate() {
                    let new_x = x as i32 + dx;
                    let new_y = y as i32 - dy;
                    let (dim_y, dim_x) = ctx.level.grid.dim();
                    if new_x < 0 || new_y < 0 || new_x >= dim_x as i32 || new_y >= dim_y as i32 {
                        continue;
                    }
                    neighborhood[i] = &atoms[&ctx.level.grid[(new_y as usize, new_x as usize)]][..];
                }
                let neighborhood = Neighborhood::new(neighborhood);

                let mut normal_appearance = true;
                for pass in render_passes {
                    if !pass.neighborhood_appearance(&atom, objtree, &neighborhood, &mut underlays, bump) {
                        normal_appearance = false;
                    }
                }
                if normal_appearance {
                    underlays.push(atom.sprite.clone());
                }

                sprites.extend(underlays.drain(..).map(|o| (loc, o)));
                sprites.extend(overlays.drain(..).map(|o| (loc, o)));
            }
        }
    }
    drop(underlays);
    drop(overlays);

    // Drop sprites rejected by any render pass.
    sprites.retain(|(_, sprite)| {
        for pass in render_passes.iter() {
            if !pass.sprite_filter(sprite) {
                return false;
            }
        }
        true
    });

    // Sort the sprite list by depth.
    sprites.sort_by_key(|(_, s)| (s.plane, s.layer));

    // Composite the sorted sprites onto the output image.
    let mut map_image = Image::new_rgba(len_x as u32 * TILE_SIZE, len_y as u32 * TILE_SIZE);
    for ((x, y), sprite) in sprites {
        let icon_file = match icon_cache.retrieve_shared(sprite.icon.as_ref()) {
            Some(icon_file) => icon_file,
            None => continue,
        };

        if let Some(rect) = icon_file.rect_of(&sprite.icon_state.into(), sprite.dir) {
            let pixel_x = sprite.ofs_x;
            let pixel_y = sprite.ofs_y + icon_file.metadata.height as i32;
            let loc = (
                ((x - ctx.min.0 as u32) * TILE_SIZE) as i32 + pixel_x,
                ((y + 1 - min_y as u32) * TILE_SIZE) as i32 - pixel_y,
            );

            if let Some((loc, rect)) = clip((map_image.width, map_image.height), loc, rect) {
                map_image.composite(&icon_file.image, loc, rect, sprite.color);
            }
        } else {
            let key = format!("bad icon: {:?}, state: {:?}", sprite.icon, sprite.icon_state);
            if !ctx.errors.read().unwrap().contains(&key) {
                eprintln!("{key}");
                ctx.errors.write().unwrap().insert(key);
            }
        }
    }

    Ok(map_image)
}

// OOB handling
fn clip(bounds: dmi::Coordinate, mut loc: (i32, i32), mut rect: dmi::Rect) -> Option<(dmi::Coordinate, dmi::Rect)> {
    if loc.0 < 0 {
        rect.0 += (-loc.0) as u32;
        rect.2 = rect.2.checked_sub((-loc.0) as u32)?;
        loc.0 = 0;
    }
    let overhang = loc.0 + rect.2 as i32 - bounds.0 as i32;
    if overhang > 0 {
        rect.2 = rect.2.checked_sub(overhang as u32)?;
    }
    if loc.1 < 0 {
        rect.1 += (-loc.1) as u32;
        rect.3 = rect.3.checked_sub((-loc.1) as u32)?;
        loc.1 = 0;
    }
    let overhang = loc.1 + rect.3 as i32 - bounds.1 as i32;
    if overhang > 0 {
        rect.3 = rect.3.checked_sub(overhang as u32)?;
    }
    Some(((loc.0 as u32, loc.1 as u32), rect))
}

fn get_atom_list<'a>(
    objtree: &'a ObjectTree,
    prefabs: &'a [Prefab],
    render_passes: &[Box<dyn RenderPass>],
    errors: &RwLock<HashSet<String>>,
) -> Vec<Atom<'a>> {
    let mut result = Vec::new();

    'fab: for fab in prefabs {
        for pass in render_passes {
            if !pass.path_filter(&fab.path) {
                continue 'fab;
            }
        }

        // look up the type
        let atom = match Atom::from_prefab(objtree, fab) {
            Some(x) => x,
            None => {
                let key = format!("bad path: {}", fab.path);
                if !errors.read().unwrap().contains(&key) {
                    println!("{key}");
                    errors.write().unwrap().insert(key);
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
    type_: &'a Type,
    prefab: Option<&'a Vars>,
    pub sprite: Sprite<'a>,
}

impl<'a> Atom<'a> {
    pub fn from_prefab(objtree: &'a ObjectTree, fab: &'a Prefab) -> Option<Self> {
        objtree.find(&fab.path).map(|type_| Atom {
            type_: type_.get(),
            prefab: Some(&fab.vars),
            sprite: Sprite::default(),
        })
    }

    pub fn istype(&self, parent: &str) -> bool {
        ispath(&self.type_.path, parent)
    }
}

impl<'a> From<&'a Type> for Atom<'a> {
    fn from(type_: &'a Type) -> Self {
        Atom {
            type_,
            prefab: None,
            sprite: Sprite::default(),
        }
    }
}

impl<'a> From<TypeRef<'a>> for Atom<'a> {
    fn from(type_ref: TypeRef<'a>) -> Self {
        Atom::from(type_ref.get())
    }
}

pub struct Neighborhood<'objtree, 'atoms> {
    // 0 1 2
    // 3 4 5
    // 6 7 8
    inner: [&'atoms [Atom<'objtree>]; 9],
}

impl<'a, 'b> Neighborhood<'a, 'b> {
    pub fn new(inner: [&'b [Atom<'a>]; 9]) -> Self {
        Neighborhood { inner }
    }

    pub fn center(&self) -> &'b [Atom<'a>] {
        self.inner[4]
    }

    pub fn offset(&self, dir: Dir) -> &'b [Atom<'a>] {
        self.inner[match dir {
            Dir::North => 1,
            Dir::South => 7,
            Dir::East => 5,
            Dir::West => 3,
            Dir::Northeast => 2,
            Dir::Northwest => 0,
            Dir::Southeast => 8,
            Dir::Southwest => 6,
        }]
    }
}

// ----------------------------------------------------------------------------
// Vars abstraction

pub trait GetVar<'a> {
    fn get_path(&self) -> &str;

    fn get_var(&self, key: &str, objtree: &'a ObjectTree) -> &'a Constant {
        self.get_var_inner(key, objtree).unwrap_or_else(Constant::null)
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
        if let Some(prefab) = self.prefab {
            if let Some(v) = prefab.get(key) {
                return Some(v);
            }
        }
        let mut current = Some(self.type_);
        while let Some(t) = current.take() {
            if let Some(v) = t.vars.get(key) {
                return Some(v.value.constant.as_ref().unwrap_or_else(Constant::null));
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
                return Some(v.value.constant.as_ref().unwrap_or_else(Constant::null));
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
                return Some(v.value.constant.as_ref().unwrap_or_else(Constant::null));
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
                return Some(v.value.constant.as_ref().unwrap_or_else(Constant::null));
            }
            current = t.parent_type();
        }
        None
    }
}

// ----------------------------------------------------------------------------
// Renderer-agnostic sprite structure

/// A guaranteed sortable representation of a `layer` float.
#[derive(Default, Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Layer {
    whole: i16,
    frac: u16,
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
        Layer { whole: f.floor() as i16, frac: ((f.fract() + 1.).fract() * 65536.) as u16 }
    }
}

#[cfg(feature = "gfx_core")]
impl gfx_core::shade::BaseTyped for Layer {
    fn get_base_type() -> gfx_core::shade::BaseType {
        i32::get_base_type()
    }
}

/// A Sprite is a fragment of an atom's appearance.
///
/// Every atom has a default sprite, which may be disabled, and a list of
/// overlays.
#[derive(Debug, Clone)]
pub struct Sprite<'s> {
    // visual appearance
    pub icon: &'s str,
    pub icon_state: &'s str,
    pub dir: Dir,
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
            icon: vars.get_var("icon", objtree).as_path_str().unwrap_or(""),
            icon_state: vars.get_var("icon_state", objtree).as_str().unwrap_or(""),
            dir: vars.get_var("dir", objtree).to_int().and_then(Dir::from_int).unwrap_or_default(),
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
            icon: "",
            icon_state: "",
            dir: Dir::default(),
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
        Constant::Float(i) => *i as i32,
        other => {
            eprintln!("not a plane: {:?} on {:?}", other, atom.get_path());
            0
        }
    }
}

pub(crate) fn layer_of<'s, T: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, atom: &T) -> Layer {
    match atom.get_var("layer", objtree) {
        &Constant::Float(f) => Layer::from(f),
        other => {
            eprintln!("not a layer: {:?} on {:?}", other, atom.get_path());
            Layer::from(2)
        }
    }
}

pub fn color_of<'s, T: GetVar<'s> + ?Sized>(objtree: &'s ObjectTree, atom: &T) -> [u8; 4] {
    let alpha = match atom.get_var("alpha", objtree) {
        &Constant::Float(i) if (0. ..=255.).contains(&i) => i as u8,
        _ => 255,
    };

    match *atom.get_var("color", objtree) {
        Constant::String(ref color) if color.starts_with('#') => {
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
        Constant::String(ref color) => match html_color(color) {
            Some([r, g, b]) => [r, g, b, alpha],
            None => [255, 255, 255, alpha],
        }
        // TODO: color matrix support?
        _ => [255, 255, 255, alpha],
    }
}

fn html_color(name: &str) -> Option<[u8; 3]> {
    Some(match name {
        // from "tags (text)" in the DM reference
        "black" => [0, 0, 0],
        "silver" => [0xc0, 0xc0, 0xc0],
        "gray" | "grey" => [0x80, 0x80, 0x80],
        "white" => [0xff, 0xff, 0xff],
        "maroon" => [0x80, 0, 0],
        "red" => [0xff, 0, 0],
        "purple" => [0x80, 0, 0x80],
        "fuchsia" | "magenta" => [0xff, 0, 0xff],
        "green" => [0, 0xc0, 0],
        "lime" => [0, 0xff, 0],
        "olive" | "gold" => [0x80, 0x80, 0],
        "yellow" => [0xff, 0xff, 0],
        "navy" => [0, 0, 0x80],
        "blue" => [0, 0, 0xff],
        "teal" => [0, 0x80, 0x80],
        "aqua" | "cyan" => [0, 0xff, 0xff],
        _ => return None,
    })
}
