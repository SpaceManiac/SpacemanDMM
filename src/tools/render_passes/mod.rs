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
