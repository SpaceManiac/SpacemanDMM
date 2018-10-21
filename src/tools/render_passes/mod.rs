use dm::constants::Constant;
use dm::objtree::*;
use minimap::{Atom, GetVar};

pub mod random;
pub mod structures;
pub mod transit_tube;

/// A map rendering pass.
///
/// These methods are applied to any given atom in roughly the order they
/// appear here.
#[allow(unused_variables)]
pub trait RenderPass: Sync {
    /// Filter atoms based solely on their typepath.
    fn path_filter(&self, path: &str) -> bool {
        true
    }

    /// Filter atoms at the beginning of the process.
    ///
    /// Return `false` to discard the atom.
    fn early_filter(&self, atom: &Atom, objtree: &ObjectTree) -> bool {
        true
    }

    /// Expand atoms, such as spawners into the atoms they spawn.
    ///
    /// Return `true` to consume the original atom.
    fn expand<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        false
    }

    /// Adjust the variables of an atom.
    fn adjust_vars<'a>(&self, atom: &mut Atom<'a>, objtree: &'a ObjectTree) {}

    /// Apply overlays and underlays to an atom, in the form of pseudo-atoms.
    fn overlays<'a>(
        &self,
        atom: &mut Atom<'a>,
        objtree: &'a ObjectTree,
        underlays: &mut Vec<Atom<'a>>,
        overlays: &mut Vec<Atom<'a>>,
    ) {
    }

    /// Filter atoms at the end of the process.
    ///
    /// Will act on adjusted atoms and pseudo-atoms from `adjust_vars` and
    /// `overlays`. Return `true` to keep and `false` to discard.
    fn late_filter(&self, atom: &Atom, objtree: &ObjectTree) -> bool {
        true
    }
}

pub struct RenderPassInfo {
    pub name: &'static str,
    pub desc: &'static str,
    pub default: bool,
    pub new: fn() -> Box<RenderPass>,
}

macro_rules! pass {
    ($typ:ty, $name:expr, $desc:expr, $def:expr) => {
        RenderPassInfo {
            name: $name,
            desc: $desc,
            default: $def,
            new: || Box::new(<$typ>::default()),
        }
    };
}

pub const RENDER_PASSES: &[RenderPassInfo] = &[
    pass!(
        HideSpace,
        "hide-space",
        "Do not render space tiles, instead leaving transparency.",
        true
    ),
    pass!(HideAreas, "hide-areas", "Do not render area icons.", true),
    pass!(
        HideInvisible,
        "hide-invisible",
        "Do not render invisible or ephemeral objects such as mapping helpers.",
        true
    ),
    pass!(
        random::Random,
        "random",
        "Replace random spawners with one of their possibilities.",
        true
    ),
    pass!(
        Pretty,
        "pretty",
        "Add the minor cosmetic overlays for various objects.",
        true
    ),
    pass!(
        structures::Spawners,
        "spawners",
        "Replace object spawners with their spawned objects.",
        true
    ),
    pass!(
        FakeGlass,
        "fake-glass",
        "Add underlays to fake glass turfs.",
        true
    ),
    pass!(
        transit_tube::TransitTube,
        "transit-tube",
        "Add overlays to connect transit tubes together.",
        true
    ),
    pass!(
        structures::GravityGen,
        "gravity-gen",
        "Expand the gravity generator to the full structure.",
        true
    ),
    pass!(Wires, "only-powernet", "Render only power cables.", false),
    pass!(
        Pipes,
        "only-pipenet",
        "Render only atmospheric pipes.",
        false
    ),
];

pub fn configure(include: &str, exclude: &str) -> Vec<Box<RenderPass>> {
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

fn add_to<'a, S: Into<String>>(target: &mut Vec<Atom<'a>>, atom: &Atom<'a>, icon: S) {
    let mut copy = atom.clone();
    copy.set_var("icon_state", Constant::string(icon));
    target.push(copy);
}

#[derive(Default)]
pub struct HideSpace;
impl RenderPass for HideSpace {
    fn expand<'a>(
        &self,
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
        if atom
            .get_var("invisibility", objtree)
            .to_float()
            .unwrap_or(0.)
            > 60.
            || atom.istype("/obj/effect/mapping_helpers/")
        {
            return false;
        }
        if atom
            .get_var("icon", objtree)
            .eq_resource("icons/obj/items_and_weapons.dmi")
            && atom.get_var("icon_state", objtree).eq_string("syndballoon")
            && !atom.istype("/obj/item/toy/syndicateballoon/")
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
        atom: &mut Atom<'a>,
        _objtree: &'a ObjectTree,
        underlays: &mut Vec<Atom<'a>>,
        _overlays: &mut Vec<Atom<'a>>,
    ) {
        if atom.istype("/turf/closed/indestructible/fakeglass/") {
            let mut copy = atom.clone();
            copy.set_var("icon", Constant::string("icons/turf/floors.dmi"));
            copy.set_var("icon_state", Constant::string("plating"));
            underlays.push(copy);
            copy = atom.clone();
            copy.set_var("icon", Constant::string("icons/obj/structures.dmi"));
            copy.set_var("icon_state", Constant::string("grille"));
            underlays.push(copy);
        }
    }
}

#[derive(Default)]
pub struct Pretty;
impl RenderPass for Pretty {
    fn adjust_vars<'a>(&self, atom: &mut Atom<'a>, _: &'a ObjectTree) {
        if atom.istype("/obj/structure/bookcase/") {
            atom.set_var("icon_state", Constant::string("book-0"));
        }
    }

    fn overlays<'a>(
        &self,
        atom: &mut Atom<'a>,
        objtree: &'a ObjectTree,
        _: &mut Vec<Atom<'a>>,
        overlays: &mut Vec<Atom<'a>>,
    ) {
        if atom.istype("/obj/item/storage/box/") && !atom.istype("/obj/item/storage/box/papersack/")
        {
            let mut copy = atom.clone();
            copy.set_var("icon_state", atom.get_var("illustration", objtree).clone());
            overlays.push(copy);
        } else if atom.istype("/obj/machinery/firealarm/") {
            add_to(overlays, atom, "overlay_0");
            add_to(overlays, atom, "overlay_clear");
        } else if atom.istype("/obj/structure/tank_dispenser/") {
            if let &Constant::Int(oxygen) = atom.get_var("oxygentanks", objtree) {
                if oxygen >= 4 {
                    add_to(overlays, atom, "oxygen-4");
                } else if oxygen > 0 {
                    add_to(overlays, atom, format!("oxygen-{}", oxygen));
                }
            }
            if let &Constant::Int(plasma) = atom.get_var("plasmatanks", objtree) {
                if plasma >= 5 {
                    add_to(overlays, atom, "plasma-5");
                } else if plasma > 0 {
                    add_to(overlays, atom, format!("plasma-{}", plasma));
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
