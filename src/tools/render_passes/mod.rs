use dm::objtree::*;
use minimap::Atom;

/// A map rendering pass.
///
/// These methods are applied to any given atom in roughly the order they
/// appear here.
#[allow(unused_variables)]
pub trait RenderPass {
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
    fn expand(&self,
        atom: &Atom,
        objtree: &ObjectTree,
        output: &mut Vec<Atom>,
    ) -> bool { false }

    /// Adjust the variables of an atom.
    fn adjust_vars(&self,
        atom: &mut Atom,
        objtree: &ObjectTree,
    ) {}

    /// Apply overlays and underlays to an atom, in the form of pseudo-atoms.
    fn overlays(&self,
        atom: &mut Atom,
        objtree: &ObjectTree,
        underlays: &mut Vec<Atom>,
        overlays: &mut Vec<Atom>,
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
