use dm::objtree::*;
use minimap::Atom;

#[allow(unused_variables)]
pub trait RenderPass {
    fn final_filter(&self, atom: &Atom, objtree: &ObjectTree) -> bool { true }
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

pub fn configure(include: &[String], exclude: &[String]) -> Vec<Box<RenderPass>> {
    let include_all = include.iter().any(|name| name == "all");
    let exclude_all = exclude.iter().any(|name| name == "all");

    let mut output = Vec::new();
    for pass in RENDER_PASSES {
        let included = if include.iter().any(|name| name == pass.name) {
            true
        } else if exclude.iter().any(|name| name == pass.name) {
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
    fn final_filter(&self, atom: &Atom, _: &ObjectTree) -> bool {
        !atom.istype("/turf/open/space/")
    }
}
