//! Placement and editing tools which appear in the workbench.

use imgui::*;

use dm::objtree::ObjectTree;

pub enum ToolIcon {
    None,
    Dmi(String, String),
    Loaded(ImTexture, ImVec2, ImVec2),
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

    fn dmi(self, file: String, state: String) -> Self {
        Tool { icon: ToolIcon::Dmi(file, state), ..self }
    }

    fn build(self, tools: &mut Vec<Tool>) {
        tools.push(self);
    }
}

pub fn configure(_objtree: &ObjectTree) -> Vec<Tool> {
    let mut tools = Vec::new();
    Tool::new("Place", Place)
        .show_objtree()
        .dmi("icons/obj/device.dmi".to_owned(), "analyzer".to_owned())
        .build(&mut tools);
    Tool::new("Rectangle", Rectangle)
        .show_objtree()
        .dmi("icons/obj/device.dmi".to_owned(), "spectrometer".to_owned())
        .build(&mut tools);
    Tool::new("Select", Select)
        .show_objtree()
        .dmi("icons/obj/device.dmi".to_owned(), "health".to_owned())
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
