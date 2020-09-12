use dm::objtree::*;
use dm::constants::Constant;
use crate::minimap::{Atom, GetVar, Sprite, Layer, Neighborhood};

mod transit_tube;
mod random;
mod structures;
mod icon_smoothing;
mod icon_smoothing_2020;
mod smart_cables;

pub use self::transit_tube::TransitTube;
pub use self::random::Random;
pub use self::structures::{GravityGen, Spawners};
pub use self::icon_smoothing::IconSmoothing as IconSmoothing2016;
pub use self::icon_smoothing_2020::IconSmoothing;
pub use self::smart_cables::SmartCables;

/// A map rendering pass.
///
/// These methods are applied to any given atom in roughly the order they
/// appear here.
#[allow(unused_variables)]
pub trait RenderPass: Sync {
    /// Filter atoms based solely on their typepath.
    fn path_filter(&self,
        path: &str,
    ) -> bool { true }

    /// Filter atoms at the beginning of the process.
    ///
    /// Return `false` to discard the atom.
    fn early_filter(&self,
        atom: &Atom,
        objtree: &ObjectTree,
    ) -> bool { true }

    /// Expand atoms, such as spawners into the atoms they spawn.
    ///
    /// Return `false` to discard the original atom.
    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool { true }

    fn adjust_sprite<'a>(&self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        objtree: &'a ObjectTree,
        bump: &'a bumpalo::Bump,  // TODO: kind of a hacky way to pass this
    ) {}

    /// Apply overlays and underlays to an atom, in the form of pseudo-atoms.
    fn overlays<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        underlays: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
        bump: &'a bumpalo::Bump,  // TODO: kind of a hacky way to pass this
    ) {}

    fn neighborhood_appearance<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        neighborhood: &Neighborhood<'a, '_>,
        output: &mut Vec<Sprite<'a>>,
        bump: &'a bumpalo::Bump,  // TODO: kind of a hacky way to pass this
    ) -> bool { true }

    /// Filter atoms at the end of the process, after they have been taken into
    /// account by their neighbors.
    fn late_filter(&self,
        atom: &Atom,
        objtree: &ObjectTree,
    ) -> bool { true }

    fn sprite_filter(&self,
        sprite: &Sprite,
    ) -> bool { true }
}

pub struct RenderPassInfo {
    pub name: &'static str,
    pub desc: &'static str,
    pub default: bool,
    pub new: fn() -> Box<dyn RenderPass>,
}

macro_rules! pass {
    ($typ:ty, $name:expr, $desc:expr, $def:expr) => (RenderPassInfo {
        name: $name,
        desc: $desc,
        default: $def,
        new: || Box::new(<$typ>::default())
    })
}

pub const RENDER_PASSES: &[RenderPassInfo] = &[
    pass!(HideSpace, "hide-space", "Do not render space tiles, instead leaving transparency.", true),
    pass!(HideAreas, "hide-areas", "Do not render area icons.", true),
    pass!(HideInvisible, "hide-invisible", "Do not render invisible or ephemeral objects such as mapping helpers.", true),
    pass!(Random, "random", "Replace random spawners with one of their possibilities.", true),
    pass!(Pretty, "pretty", "Add the minor cosmetic overlays for various objects.", true),
    pass!(Spawners, "spawners", "Replace object spawners with their spawned objects.", true),
    pass!(Overlays, "overlays", "Add overlays and underlays to atoms which usually have them.", true),
    pass!(TransitTube, "transit-tube", "Add overlays to connect transit tubes together.", true),
    pass!(GravityGen, "gravity-gen", "Expand the gravity generator to the full structure.", true),
    pass!(Wires, "only-powernet", "Render only power cables.", false),
    pass!(Pipes, "only-pipenet", "Render only atmospheric pipes.", false),
    pass!(FancyLayers, "fancy-layers", "Layer atoms according to in-game rules.", true),
    pass!(IconSmoothing2016, "icon-smoothing-2016", "Emulate the icon smoothing subsystem (xxalpha, 2016).", false),
    pass!(IconSmoothing, "icon-smoothing", "Emulate the icon smoothing subsystem (Rohesie, 2020).", true),
    pass!(SmartCables, "smart-cables", "Handle smart cable layout.", true),
];

pub fn configure(include: &str, exclude: &str) -> Vec<Box<dyn RenderPass>> {
    let include: Vec<&str> = include.split(",").collect();
    let exclude: Vec<&str> = exclude.split(",").collect();
    let include_all = include.iter().any(|&name| name == "all");
    let exclude_all = exclude.iter().any(|&name| name == "all");

    let mut output = Vec::new();
    for pass in RENDER_PASSES {
        let included = if include.iter().any(|&name| name == pass.name) {
            true
        } else if exclude.iter().any(|&name| name == pass.name) {
            false
        } else if include_all {
            true
        } else if exclude_all {
            false
        } else {
            pass.default
        };
        if included {
            output.push((pass.new)());
        }
    }
    output
}

fn add_to<'a>(target: &mut Vec<Sprite<'a>>, atom: &Atom<'a>, icon_state: &'a str) {
    target.push(Sprite {
        icon_state,
        .. atom.sprite
    });
}

#[derive(Default)]
pub struct HideSpace;
impl RenderPass for HideSpace {
    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        if atom.istype("/turf/template_noop/") {
            output.push(Atom::from(objtree.expect("/turf/open/space")));
            false
        } else {
            true
        }
    }

    fn late_filter(&self, atom: &Atom, _: &ObjectTree) -> bool {
        !atom.istype("/turf/open/space/")
    }

    fn sprite_filter(&self, sprite: &Sprite) -> bool {
        sprite.icon != "icons/turf/space.dmi"
    }
}

#[derive(Default)]
pub struct HideAreas;
impl RenderPass for HideAreas {
    fn path_filter(&self, path: &str) -> bool {
        !subpath(path, "/area/")
    }
}

#[derive(Default)]
pub struct HideInvisible;
impl RenderPass for HideInvisible {
    fn path_filter(&self, path: &str) -> bool {
        !subpath(path, "/obj/effect/spawner/xmastree/")
    }

    fn early_filter(&self, atom: &Atom, objtree: &ObjectTree) -> bool {
        // invisible objects and syndicate balloons are not to show
        if atom.get_var("invisibility", objtree).to_float().unwrap_or(0.) > 60. ||
            atom.istype("/obj/effect/mapping_helpers/")
        {
            return false;
        }
        if atom.get_var("icon", objtree) == "icons/obj/items_and_weapons.dmi" &&
            atom.get_var("icon_state", objtree) == "syndballoon" &&
            !atom.istype("/obj/item/toy/syndicateballoon/")
        {
            return false;
        }
        true
    }
}

#[derive(Default)]
pub struct Overlays;
impl RenderPass for Overlays {
    fn adjust_sprite<'a>(&self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        objtree: &'a ObjectTree,
        _: &'a bumpalo::Bump,
    ) {
        use crate::dmi::Dir;

        if atom.istype("/obj/machinery/power/apc/") {
            // auto-set pixel location
            match atom.get_var("dir", objtree).to_int().and_then(Dir::from_int) {
                Some(Dir::North) => sprite.ofs_y = 23,
                Some(Dir::South) => sprite.ofs_y = -23,
                Some(Dir::East) => sprite.ofs_x = 24,
                Some(Dir::West) => sprite.ofs_x = -25,
                _ => {}
            }
        }
    }

    fn overlays<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        underlays: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
        bump: &'a bumpalo::Bump,
    ) {
        // overlays and underlays
        if atom.istype("/turf/closed/indestructible/fakeglass/") {
            underlays.push(Sprite {
                icon: "icons/turf/floors.dmi",
                icon_state: "plating",
                .. atom.sprite
            });
            underlays.push(Sprite {
                icon: "icons/obj/structures.dmi",
                icon_state: "grille",
                .. atom.sprite
            });
        } else if atom.istype("/obj/structure/closet/") {
            // closet doors
            if atom.get_var("opened", objtree).to_bool() {
                let var = if atom.get_var("icon_door_override", objtree).to_bool() {
                    "icon_door"
                } else {
                    "icon_state"
                };
                if let &Constant::String(ref door) = atom.get_var(var, objtree) {
                    add_to(overlays, atom, bumpalo::format!(in bump, "{}_open", door).into_bump_str());
                }
            } else {
                if let &Constant::String(ref door) = atom
                    .get_var_notnull("icon_door", objtree)
                    .unwrap_or_else(|| atom.get_var("icon_state", objtree))
                {
                    add_to(overlays, atom, bumpalo::format!(in bump, "{}_door", door).into_bump_str());
                }
                if atom.get_var("welded", objtree).to_bool() {
                    add_to(overlays, atom, "welded");
                }
                if atom.get_var("secure", objtree).to_bool() && !atom.get_var("broken", objtree).to_bool() {
                    if atom.get_var("locked", objtree).to_bool() {
                        add_to(overlays, atom, "locked");
                    } else {
                        add_to(overlays, atom, "unlocked");
                    }
                }
            }
        } else if atom.istype("/obj/machinery/computer/") || atom.istype("/obj/machinery/power/solar_control/") {
            // computer screens and keyboards
            if let Some(screen) = atom.get_var("icon_screen", objtree).as_str() {
                add_to(overlays, atom, screen);
            }
            if let Some(keyboard) = atom.get_var("icon_keyboard", objtree).as_str() {
                add_to(overlays, atom, keyboard);
            }
        } else if atom.istype("/obj/machinery/door/airlock/") {
            if atom.get_var("glass", objtree).to_bool() {
                if let Some(overlays_file) = atom.get_var("overlays_file", objtree).as_path_str() {
                    overlays.push(Sprite {
                        icon: overlays_file,
                        icon_state: "glass_closed",
                        .. atom.sprite
                    })
                }
            } else {
                add_to(overlays, atom, "fill_closed");
            }
        } else if atom.istype("/obj/machinery/power/apc/") {
            // status overlays
            for &each in ["apcox-1", "apco3-2", "apco0-3", "apco1-3", "apco2-3"].iter() {
                add_to(overlays, atom, each);
            }

            // APC terminals
            let mut terminal = Sprite::from_vars(objtree, &objtree.expect("/obj/machinery/power/terminal"));
            terminal.dir = atom.sprite.dir;
            // TODO: un-hack this
            apply_fancy_layer("/obj/machinery/power/terminal", &mut terminal);
            underlays.push(terminal);
        }
    }
}

#[derive(Default)]
pub struct Pretty;
impl RenderPass for Pretty {
    fn adjust_sprite<'a>(&self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        _: &'a ObjectTree,
        _: &'a bumpalo::Bump,
    ) {
        if atom.istype("/obj/structure/bookcase/") {
            sprite.icon_state = "book-0";
        }
    }

    fn overlays<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
        _: &bumpalo::Bump,
    ) {
        if atom.istype("/obj/item/storage/box/") && !atom.istype("/obj/item/storage/box/papersack/") {
            if let Some(icon_state) = atom.get_var("illustration", objtree).as_str() {
                overlays.push(Sprite {
                    icon_state,
                    .. atom.sprite
                });
            }
        } else if atom.istype("/obj/machinery/firealarm/") {
            add_to(overlays, atom, "fire_overlay");
            add_to(overlays, atom, "fire_0");
            add_to(overlays, atom, "fire_off");
        } else if atom.istype("/obj/structure/tank_dispenser/") {
            if let &Constant::Int(oxygen) = atom.get_var("oxygentanks", objtree) {
                match oxygen {
                    4..=std::i32::MAX => add_to(overlays, atom, "oxygen-4"),
                    3 => add_to(overlays, atom, "oxygen-3"),
                    2 => add_to(overlays, atom, "oxygen-2"),
                    1 => add_to(overlays, atom, "oxygen-1"),
                    _ => {}
                }
            }
            if let &Constant::Int(plasma) = atom.get_var("plasmatanks", objtree) {
                match plasma {
                    5..=std::i32::MAX => add_to(overlays, atom, "plasma-5"),
                    4 => add_to(overlays, atom, "plasma-4"),
                    3 => add_to(overlays, atom, "plasma-3"),
                    2 => add_to(overlays, atom, "plasma-2"),
                    1 => add_to(overlays, atom, "plasma-1"),
                    _ => {}
                }
            }
        }
    }
}

#[derive(Default)]
pub struct Wires;
impl RenderPass for Wires {
    fn late_filter(&self, atom: &Atom, _: &ObjectTree) -> bool {
        atom.istype("/obj/structure/cable/")
    }
}

#[derive(Default)]
pub struct Pipes;
impl RenderPass for Pipes {
    fn late_filter(&self, atom: &Atom, _: &ObjectTree) -> bool {
        atom.istype("/obj/machinery/atmospherics/pipe/")
    }
}

#[derive(Default)]
pub struct FancyLayers;
impl RenderPass for FancyLayers {
    fn adjust_sprite<'a>(&self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        objtree: &'a ObjectTree,
        _: &'a bumpalo::Bump,
    ) {
        apply_fancy_layer(atom.get_path(), sprite);

        // dual layering of vents 1: hide original sprite underfloor
        if atom.istype("/obj/machinery/atmospherics/components/unary/") {
            if unary_aboveground(atom, objtree).is_some() {
                sprite.layer = Layer::from(-5);
            }
        }
    }

    fn overlays<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _underlays: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
        _bump: &'a bumpalo::Bump,
    ) {
        // dual layering of vents 2: add abovefloor overlay
        if atom.istype("/obj/machinery/atmospherics/components/unary/") {
            if let Some(aboveground) = unary_aboveground(atom, objtree) {
                overlays.push(Sprite {
                    icon_state: aboveground,
                    // use original layer, not modified layer above
                    layer: crate::minimap::layer_of(objtree, atom),
                    .. atom.sprite
                });
            }
        }
    }
}

fn unary_aboveground(atom: &Atom, objtree: &ObjectTree) -> Option<&'static str> {
    Some(match atom.get_var("icon_state", objtree) {
        &Constant::String(ref text) => match &**text {
            "vent_map-1" | "vent_map-2" | "vent_map-3" | "vent_map-4" => "vent_off",
            "vent_map_on-1" | "vent_map_on-2" | "vent_map_on-3" | "vent_map_on-4" => "vent_out",
            "vent_map_siphon_on-1" | "vent_map_siphon_on-2" | "vent_map_siphon_on-3" | "vent_map_siphon_on-4" => "vent_in",
            "scrub_map-1" | "scrub_map-2" | "scrub_map-3" | "scrub_map-4" => "scrub_off",
            "scrub_map_on-1" | "scrub_map_on-2" | "scrub_map_on-3" | "scrub_map_on-4" => "scrub_on",
            _ => return None,
        },
        _ => return None,
    })
}

fn fancy_layer_for_path(p: &str) -> Option<Layer> {
    Some(if subpath(p, "/turf/open/floor/plating/") || subpath(p, "/turf/open/space/") {
        Layer::from(-10)  // under everything
    } else if subpath(p, "/turf/closed/mineral/") {
        Layer::from(-3)   // above hidden stuff and plating but below walls
    } else if subpath(p, "/turf/open/floor/") || subpath(p, "/turf/closed/") {
        Layer::from(-2)   // above hidden pipes and wires
    } else if subpath(p, "/turf/") {
        Layer::from(-10)  // under everything
    } else if subpath(p, "/obj/effect/turf_decal/") {
        Layer::from(-1)   // above turfs
    } else if subpath(p, "/obj/structure/disposalpipe/") {
        Layer::from(-6)
    } else if subpath(p, "/obj/machinery/atmospherics/pipe/") && !p.contains("visible") {
        Layer::from(-5)
    } else if subpath(p, "/obj/structure/cable/") {
        Layer::from(-4)
    } else if subpath(p, "/obj/machinery/power/terminal/") {
        Layer::from(-3.5)
    } else if subpath(p, "/obj/structure/lattice/") {
        Layer::from(-8)
    } else if subpath(p, "/obj/machinery/navbeacon/") {
        Layer::from(-3)
    } else {
        return None
    })
}

fn apply_fancy_layer(path: &str, sprite: &mut Sprite) {
    sprite.plane = 0;
    if let Some(layer) = fancy_layer_for_path(path) {
        sprite.layer = layer;
    }
}
