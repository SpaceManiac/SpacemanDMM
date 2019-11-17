use dm::objtree::*;
use dm::constants::Constant;
use minimap::{Atom, GetVar, Sprite, Layer};

pub mod transit_tube;
pub mod random;
pub mod structures;
pub mod icon_smoothing;

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
    /// Return `true` to consume the original atom.
    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool { false }

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
    ) {}

    /// Filter atoms at the end of the process, after they have been taken into
    /// account by their neighbors.
    fn late_filter(&self,
        atom: &Atom,
        objtree: &ObjectTree,
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
    pass!(random::Random, "random", "Replace random spawners with one of their possibilities.", true),
    pass!(Pretty, "pretty", "Add the minor cosmetic overlays for various objects.", true),
    pass!(structures::Spawners, "spawners", "Replace object spawners with their spawned objects.", true),
    pass!(FakeGlass, "fake-glass", "Add underlays to fake glass turfs.", true),
    pass!(transit_tube::TransitTube, "transit-tube", "Add overlays to connect transit tubes together.", true),
    pass!(structures::GravityGen, "gravity-gen", "Expand the gravity generator to the full structure.", true),
    pass!(Wires, "only-powernet", "Render only power cables.", false),
    pass!(Pipes, "only-pipenet", "Render only atmospheric pipes.", false),
    pass!(FancyLayers, "fancy-layers", "Layer atoms according to in-game rules.", true),
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
            output.push(Atom::from_type(objtree, "/turf/open/space", atom.loc).unwrap());
            true
        } else {
            false
        }
    }

    fn late_filter(&self, atom: &Atom, _: &ObjectTree) -> bool {
        !atom.istype("/turf/open/space/")
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
pub struct FakeGlass;
impl RenderPass for FakeGlass {
    fn overlays<'a>(
        &self,
        atom: &Atom<'a>,
        _objtree: &'a ObjectTree,
        underlays: &mut Vec<Sprite<'a>>,
        _overlays: &mut Vec<Sprite<'a>>,
    ) {
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
        _: &'a ObjectTree,
        _: &'a bumpalo::Bump,
    ) {
        apply_fancy_layer(atom.get_path(), sprite)
    }
}

fn fancy_layer_for_path(p: &str) -> Option<Layer> {
    use dm::objtree::subpath as subtype;
    Some(if subtype(p, "/turf/open/floor/plating/") || subtype(p, "/turf/open/space/") {
        Layer::from(-10)  // under everything
    } else if subtype(p, "/turf/closed/mineral/") {
        Layer::from(-3)   // above hidden stuff and plating but below walls
    } else if subtype(p, "/turf/open/floor/") || subtype(p, "/turf/closed/") {
        Layer::from(-2)   // above hidden pipes and wires
    } else if subtype(p, "/turf/") {
        Layer::from(-10)  // under everything
    } else if subtype(p, "/obj/effect/turf_decal/") {
        Layer::from(-1)   // above turfs
    } else if subtype(p, "/obj/structure/disposalpipe/") {
        Layer::from(-6)
    } else if subtype(p, "/obj/machinery/atmospherics/pipe/") && !p.contains("visible") {
        Layer::from(-5)
    } else if subtype(p, "/obj/structure/cable/") {
        Layer::from(-4)
    } else if subtype(p, "/obj/machinery/power/terminal/") {
        Layer::from(-3.5)
    } else if subtype(p, "/obj/structure/lattice/") {
        Layer::from(-8)
    } else if subtype(p, "/obj/machinery/navbeacon/") {
        Layer::from(-3)
    } else {
        return None
    })
}

pub fn apply_fancy_layer(path: &str, sprite: &mut Sprite) {
    sprite.plane = 0;
    if let Some(layer) = fancy_layer_for_path(path) {
        sprite.layer = layer;
    }
}
