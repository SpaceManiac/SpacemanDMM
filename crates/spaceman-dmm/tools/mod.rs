//! Placement and editing tools which appear in the workbench.

use std::path::PathBuf;

use imgui::*;

use dm::dmi;
use dm::objtree::ObjectTree;
use dmm_tools::dmm::Prefab;

use crate::{History, Environment, ImRenderer};

mod place;

pub const NO_TINT: [f32; 4] = [1.0, 1.0, 1.0, 1.0];

pub struct Tool {
    pub name: &'static str,
    pub help: &'static str,
    pub objtree: bool,
    pub icon: ToolIcon,
    pub behavior: Box<dyn ToolBehavior>,
}

pub enum ToolIcon {
    None,
    Dmi {
        icon: PathBuf,
        icon_state: String,
        tint: [f32; 4],
        dir: dmi::Dir,
    },
    EmbeddedPng {
        data: &'static [u8],
    },
    Loaded {
        tex: TextureId,
        uv0: [f32; 2],
        uv1: [f32; 2],
        tint: Option<[f32; 4]>,
    },
}

#[allow(unused_variables)]
pub trait ToolBehavior {
    fn settings(&mut self, ui: &Ui, env: &Environment, ctx: &mut IconCtx) {}

    // Called on mouse down.
    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {}

    // Called when mouse moves over a new tile.
    fn drag(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {}

    fn pick(&mut self, env: &Environment, prefab: &Prefab) {}
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

    fn dmi(self, icon: PathBuf, icon_state: String) -> Self {
        Tool {
            icon: ToolIcon::Dmi {
                icon,
                icon_state,
                tint: NO_TINT,
                dir: dmi::Dir::South,
            },
            ..self
        }
    }

    fn png(self, data: &'static [u8]) -> Self {
        Tool {
            icon: ToolIcon::EmbeddedPng { data },
            ..self
        }
    }

    fn build(self, tools: &mut Vec<Tool>) {
        tools.push(self);
    }
}

impl ToolIcon {
    pub fn from_atom(env: &Environment, prefab: &Prefab) -> Option<ToolIcon> {
        let (_, ty) = env.find_closest_type(&prefab.path);
        let ty = ty?;

        let icon = prefab.vars.get("icon")
            .or_else(|| ty.get_value("icon")?.constant.as_ref())?
            .as_path()?
            .to_owned();
        let icon_state = prefab.vars.get("icon_state")
            .or_else(|| ty.get_value("icon_state")
                .and_then(|v| v.constant.as_ref()))
            .and_then(|v| v.as_str())
            .unwrap_or("").to_owned();
        let dir = prefab.vars.get("dir")
            .or_else(|| ty.get_value("dir")
                .and_then(|v| v.constant.as_ref()))
            .and_then(|v| v.to_int())
            .and_then(dmi::Dir::from_int)
            .unwrap_or(dmi::Dir::default());
        let color = dmm_tools::minimap::color_of(&env.objtree, &prefab);
        let tint = [
            color[0] as f32 / 255.0, color[1] as f32 / 255.0,
            color[2] as f32 / 255.0, color[3] as f32 / 255.0,
        ].into();

        Some(ToolIcon::Dmi {
            icon,
            icon_state,
            dir,
            tint,
        })
    }

    pub fn prepare(
        &mut self,
        environment: Option<&Environment>,
        ctx: &mut IconCtx,
    ) -> &mut Self {
        let temp = std::mem::replace(self, ToolIcon::None);
        *self = crate::prepare_tool_icon(ctx.renderer, environment, ctx.map_renderer, temp);
        self
    }
}

pub struct IconCtx<'a> {
    renderer: &'a mut ImRenderer,
    map_renderer: &'a mut crate::map_renderer::MapRenderer,
}

impl<'a> IconCtx<'a> {
    pub fn new(renderer: &'a mut ImRenderer, map_renderer: &'a mut crate::map_renderer::MapRenderer) -> Self {
        IconCtx { renderer, map_renderer }
    }
}

pub fn configure(_objtree: &ObjectTree) -> Vec<Tool> {
    let mut tools = Vec::new();
    Tool::new("Place", place::Place::default())
        .help("Click to add an instance to the tile.")
        .show_objtree()
        .png(include_bytes!("../../res/pencil.png"))
        .build(&mut tools);
    Tool::new("Rectangle", Dummy)
        .help("Click and drag to fill a rectangular area.")
        .show_objtree()
        .png(include_bytes!("../../res/resize.png"))
        .build(&mut tools);
    Tool::new("Select", Dummy)
        .help("Click and drag to select a region. Drag again to move it.")
        .png(include_bytes!("../../res/select.png"))
        .build(&mut tools);
    tools
}

struct Dummy;
impl ToolBehavior for Dummy {
    fn settings(&mut self, ui: &Ui, _: &Environment, _: &mut IconCtx) {
        ui.text(im_str!("Not yet implemented."));
    }
}
