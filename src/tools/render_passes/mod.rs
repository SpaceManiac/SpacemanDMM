use dm::objtree::*;
use dm::constants::Constant;
use minimap::Atom;

/// A map rendering pass.
///
/// These methods are applied to any given atom in roughly the order they
/// appear here.
#[allow(unused_variables)]
pub trait RenderPass {
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

    /// Adjust the variables of an atom.
    fn adjust_vars<'a>(&self,
        atom: &mut Atom<'a>,
        objtree: &'a ObjectTree,
    ) {}

    /// Apply overlays and underlays to an atom, in the form of pseudo-atoms.
    fn overlays<'a>(&self,
        atom: &mut Atom<'a>,
        objtree: &'a ObjectTree,
        underlays: &mut Vec<Atom<'a>>,
        overlays: &mut Vec<Atom<'a>>,
    ) {}

    /// Filter atoms at the end of the process.
    ///
    /// Will act on adjusted atoms and pseudo-atoms from `adjust_vars` and
    /// `overlays`. Return `true` to keep and `false` to discard.
    fn late_filter(&self,
        atom: &Atom,
        objtree: &ObjectTree,
    ) -> bool { true }
}

pub struct RenderPassInfo {
    pub name: &'static str,
    pub desc: &'static str,
    pub default: bool,
    pub new: fn() -> Box<RenderPass>,
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
        if atom.get_var("invisibility", objtree).to_float().unwrap_or(0.) > 60. {
            return false;
        }
        if atom.get_var("icon", objtree).eq_resource("icons/obj/items_and_weapons.dmi") &&
            atom.get_var("icon_state", objtree).eq_string("syndballoon") &&
            !atom.istype("/obj/item/toy/syndicateballoon/")
        {
            return false;
        }
        true
    }
}

#[derive(Default)]
pub struct Spawners;
impl RenderPass for Spawners {
    fn path_filter(&self, path: &str) -> bool {
        subpath(path, "/obj/effect/spawner/structure/") || !subpath(path, "/obj/effect/spawner/")
    }

    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        if !atom.istype("/obj/effect/spawner/structure/") {
            return false;
        }
        match atom.get_var("spawn_list", objtree) {
            &Constant::List(ref elements) => {
                for &(ref key, _) in elements {
                    // TODO: use a more civilized lookup method
                    let mut type_key = String::new();
                    let reference;
                    match key {
                        &Constant::String(ref s) => reference = s,
                        &Constant::Prefab(ref fab) => {
                            for each in fab.path.iter() {
                                use std::fmt::Write;
                                let _ = write!(type_key, "{}{}", each.0, each.1);
                            }
                            reference = &type_key;
                        }
                        _ => continue,
                    }
                    output.push(Atom::from_type(objtree, reference, atom.loc).unwrap());
                }
                true  // don't include the original atom
            }
            _ => { false }  // TODO: complain?
        }
    }
}

#[derive(Default)]
pub struct Random;
impl RenderPass for Random {
    fn adjust_vars<'a>(&self,
        atom: &mut Atom<'a>,
        objtree: &'a ObjectTree,
    ) {
        use rand::Rng;

        const CONTRABAND_POSTERS: u32 = 44;
        const LEGIT_POSTERS: u32 = 35;

        if atom.istype("/obj/structure/sign/poster/contraband/random/") {
            atom.set_var("icon_state", Constant::string(format!("poster{}", ::rand::thread_rng().gen_range(1, 1 + CONTRABAND_POSTERS))));
        } else if atom.istype("/obj/structure/sign/poster/official/random/") {
            atom.set_var("icon_state", Constant::string(format!("poster{}_legit", ::rand::thread_rng().gen_range(1, 1 + LEGIT_POSTERS))));
        } else if atom.istype("/obj/structure/sign/poster/random/") {
            let i = 1 + ::rand::thread_rng().gen_range(0, CONTRABAND_POSTERS + LEGIT_POSTERS);
            if i <= CONTRABAND_POSTERS {
                atom.set_var("icon_state", Constant::string(format!("poster{}", i)));
            } else {
                atom.set_var("icon_state", Constant::string(format!("poster{}_legit", i - CONTRABAND_POSTERS)));
            }
        } else if atom.istype("/obj/structure/sign/barsign/") {
            if let Some(root) = objtree.find("/datum/barsign") {
                let mut signs = Vec::new();
                for child in root.children(objtree) {
                    if let Some(v) = child.vars.get("hidden") {
                        if !v.value.constant.as_ref().map_or(false, |c| c.to_bool()) {
                            continue
                        }
                    }
                    if let Some(icon) = child.vars.get("icon") {
                        if let Some(c) = icon.value.constant.as_ref() {
                            signs.push(c.clone());
                        }
                    }
                }
                if let Some(c) = ::rand::thread_rng().choose(&signs) {
                    atom.set_var("icon_state", c.clone());
                }
            }
        }
    }
}

#[derive(Default)]
pub struct Pretty;
impl RenderPass for Pretty {
    fn adjust_vars<'a>(&self,
        atom: &mut Atom<'a>,
        _: &'a ObjectTree,
    ) {
        if atom.istype("/obj/structure/bookcase/") {
            atom.set_var("icon_state", Constant::string("book-0"));
        }
    }

    fn overlays<'a>(&self,
        atom: &mut Atom<'a>,
        objtree: &'a ObjectTree,
        _: &mut Vec<Atom<'a>>,
        overlays: &mut Vec<Atom<'a>>,
    ) {
        if atom.istype("/obj/item/storage/box/") && !atom.istype("/obj/item/storage/box/papersack/") {
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
