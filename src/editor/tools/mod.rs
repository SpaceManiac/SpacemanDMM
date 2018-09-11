//! Placement and editing tools which appear in the workbench.
#![allow(dead_code)]  // WIP

use imgui::*;

use dm::objtree::ObjectTree;

pub enum ToolIcon {
    None,
    Dmi {
        icon: String,
        icon_state: String,
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

pub struct Tool {
    pub name: &'static str,
    pub objtree: bool,
    pub icon: ToolIcon,
    pub behavior: Box<ToolBehavior>,
}

pub trait ToolBehavior {
    fn settings(&mut self, ui: &Ui) {
        let _ = ui;
    }
}

impl Tool {
    fn new<B: ToolBehavior + 'static>(name: &'static str, behavior: B) -> Tool {
        Tool {
            name,
            objtree: false,
            icon: ToolIcon::None,
            behavior: Box::new(behavior),
        }
    }

    fn show_objtree(self) -> Self {
        Tool { objtree: true, ..self }
    }

    fn dmi(self, icon: String, icon_state: String) -> Self {
        Tool { icon: ToolIcon::Dmi { icon, icon_state }, ..self }
    }

    fn png(self, data: &'static [u8]) -> Self {
        Tool { icon: ToolIcon::EmbeddedPng { data }, ..self }
    }

    fn build(self, tools: &mut Vec<Tool>) {
        tools.push(self);
    }
}

pub fn configure(_objtree: &ObjectTree) -> Vec<Tool> {
    let mut tools = Vec::new();
    Tool::new("Place", Place)
        .show_objtree()
        .png(include_bytes!("../res/pencil.png"))
        .build(&mut tools);
    Tool::new("Rectangle", Rectangle)
        .show_objtree()
        .png(include_bytes!("../res/resize.png"))
        .build(&mut tools);
    Tool::new("Select", Select)
        .show_objtree()
        .png(include_bytes!("../res/select.png"))
        .build(&mut tools);
    tools
}

// ----------------------------------------------------------------------------
// Basic tools that are impossible to do without

struct Place;
impl ToolBehavior for Place {
}

struct Rectangle;
impl ToolBehavior for Rectangle {
}

struct Select;
impl ToolBehavior for Select {
}
