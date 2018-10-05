//! Placement and editing tools which appear in the workbench.

use imgui::*;

use dm::dmi;
use dm::objtree::ObjectTree;
use dmm_tools::dmm::Prefab;

use {History, Environment};

mod place;

pub struct Tool {
    pub name: &'static str,
    pub help: &'static str,
    pub objtree: bool,
    pub icon: ToolIcon,
    pub behavior: Box<ToolBehavior>,
}

pub enum ToolIcon {
    None,
    Dmi {
        icon: String,
        icon_state: String,
        dir: i32,
    },
    EmbeddedPng {
        data: &'static [u8],
    },
    Loaded {
        tex: ImTexture,
        uv0: ImVec2,
        uv1: ImVec2
    },
}

#[allow(unused_variables)]
pub trait ToolBehavior {
    fn settings(&mut self, ui: &Ui, env: &Environment) {
    }

    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
    }

    fn pick(&mut self, prefab: &Prefab) {
    }
}

impl Tool {
    fn new<B: ToolBehavior + 'static>(name: &'static str, behavior: B) -> Tool {
        Tool {
            name,
            help: "",
            objtree: false,
            icon: ToolIcon::None,
            behavior: Box::new(behavior),
        }
    }

    fn help(self, help: &'static str) -> Self {
        Tool { help, ..self }
    }

    fn show_objtree(self) -> Self {
        Tool { objtree: true, ..self }
    }

    fn dmi(self, icon: String, icon_state: String) -> Self {
        Tool { icon: ToolIcon::Dmi { icon, icon_state, dir: dmi::SOUTH }, ..self }
    }

    fn png(self, data: &'static [u8]) -> Self {
        Tool { icon: ToolIcon::EmbeddedPng { data }, ..self }
    }

    fn build(self, tools: &mut Vec<Tool>) {
        tools.push(self);
    }
}

impl ToolIcon {
    pub fn prepare(
        &mut self,
        renderer: &mut ::ImRenderer,
        environment: Option<&Environment>,
        map_renderer: &mut ::map_renderer::MapRenderer,
    ) {
        let temp = ::std::mem::replace(self, ToolIcon::None);
        *self = ::prepare_tool_icon(renderer, environment, map_renderer, temp);
    }
}

pub fn configure(_objtree: &ObjectTree) -> Vec<Tool> {
    let mut tools = Vec::new();
    Tool::new("Place", place::Place::default())
        .help("Click to add an instance to the tile.")
        .show_objtree()
        .png(include_bytes!("../res/pencil.png"))
        .build(&mut tools);
    Tool::new("Rectangle", Dummy)
        .help("Click and drag to fill a rectangular area.")
        .show_objtree()
        .png(include_bytes!("../res/resize.png"))
        .build(&mut tools);
    Tool::new("Select", Dummy)
        .help("Click and drag to select a region. Drag again to move it.")
        .png(include_bytes!("../res/select.png"))
        .build(&mut tools);
    tools
}

struct Dummy;
impl ToolBehavior for Dummy {
    fn settings(&mut self, ui: &Ui, _: &Environment) {
        ui.text(im_str!("Not yet implemented."));
    }
}
