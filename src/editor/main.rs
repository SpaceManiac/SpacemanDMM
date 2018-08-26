//! The map editor proper, with a GUI and everything.

extern crate glutin;
#[macro_use] extern crate gfx;
extern crate gfx_window_glutin;
extern crate gfx_device_gl;
#[macro_use] extern crate imgui;
extern crate imgui_gfx_renderer;
extern crate lodepng;

extern crate dreammaker as dm;
extern crate dmm_tools;

mod support;
mod dmi;
mod map_renderer;

use std::sync::mpsc;

use imgui::*;

use dm::objtree::{ObjectTree, TypeRef};
use dmm_tools::dmm::Map;

use gfx_device_gl::{Factory, Resources, CommandBuffer};
type Encoder = gfx::Encoder<Resources, CommandBuffer>;
type ColorFormat = gfx::format::Rgba8;
type DepthFormat = gfx::format::DepthStencil;
type RenderTargetView = gfx::handle::RenderTargetView<Resources, ColorFormat>;
type Texture = gfx::handle::ShaderResourceView<Resources, [f32; 4]>;

fn main() {
    support::run("SpacemanDMM".to_owned(), [0.25, 0.25, 0.5, 1.0]);
}

pub struct EditorScene {
    map_renderer: map_renderer::GliumTest,
    objtree: Option<ObjectTree>,

    maps: Vec<EditorMap>,
    map_current: usize,

    objtree_rx: mpsc::Receiver<ObjectTree>,
    dmm_rx: mpsc::Receiver<Map>,

    ui_style_editor: bool,
}

impl EditorScene {
    fn new(factory: &mut Factory, view: &RenderTargetView) -> Self {
        let (objtree_tx, objtree_rx) = mpsc::channel();
        std::thread::spawn(move || {
            let context = dm::Context::default();
            let env = dm::detect_environment("tgstation.dme")
                .expect("error detecting .dme")
                .expect("no .dme found");
            let objtree = context.parse_environment(&env)
                .expect("i/o error opening .dme");
            let _ = objtree_tx.send(objtree);
        });

        let (dmm_tx, dmm_rx) = mpsc::channel();
        std::thread::spawn(move || {
            let map = Map::from_file("_maps/map_files/debug/runtimestation.dmm".as_ref())
                .expect("error loading .dmm");
            let _ = dmm_tx.send(map);
        });

        EditorScene {
            map_renderer: map_renderer::GliumTest::new(factory, view),
            objtree: None,
            maps: Vec::new(),
            map_current: 0,

            objtree_rx,
            dmm_rx,

            ui_style_editor: false,
        }
    }

    fn render(&mut self, factory: &mut Factory, encoder: &mut Encoder, view: &RenderTargetView) {
        self.map_renderer.paint(factory, encoder, view);
    }

    fn run_ui(&mut self, ui: &Ui) -> bool {
        if let Ok(objtree) = self.objtree_rx.try_recv() {
            if let Some(map) = self.maps.get(self.map_current) {
                self.map_renderer.prepare(&objtree, &map.dmm, map.dmm.z_level(0));
            }
            self.objtree = Some(objtree);
        }
        if self.maps.is_empty() {
            if let Ok(dmm) = self.dmm_rx.try_recv() {
                if let Some(objtree) = self.objtree.as_ref() {
                    self.map_renderer.prepare(&objtree, &dmm, dmm.z_level(0));
                }
                self.map_current = self.maps.len();
                self.maps.push(EditorMap { dmm });
            }
        }

        ui.main_menu_bar(|| {
            ui.menu(im_str!("Window")).build(|| {
                ui.menu_item(im_str!("Style Editor"))
                    .selected(&mut self.ui_style_editor)
                    .build();
            })
        });
        if self.ui_style_editor {
            ui.window(im_str!("Style Editor"))
                .opened(&mut self.ui_style_editor)
                .build(|| ui.show_default_style_editor());
        }

        ui.window(im_str!("Object Tree"))
            .size((300.0, 600.0), ImGuiCond::FirstUseEver)
            .build(|| {
                if let Some(objtree) = self.objtree.as_ref() {
                    let root = objtree.root();
                    root_node(ui, root, "area");
                    root_node(ui, root, "turf");
                    root_node(ui, root, "obj");
                    root_node(ui, root, "mob");
                } else {
                    ui.text(im_str!("The object tree is loading..."));
                }
            });

        ui.window(im_str!("Maps"))
            .position((410.0, 60.0), ImGuiCond::FirstUseEver)
            .size((300.0, 200.0), ImGuiCond::FirstUseEver)
            .build(|| {
                for map in self.maps.iter() {
                    if ui.collapsing_header(im_str!("runtimestation.dmm")).default_open(true).build() {
                        ui.text(im_str!("key length: {}", map.dmm.key_length));
                        ui.text(im_str!("dictionary size: {}", map.dmm.dictionary.len()));
                        for z in 0..map.dmm.dim_z() {
                            ui.small_button(im_str!("z = {}", z + 1));
                        }
                    }
                }
            });
        true
    }
}

struct EditorMap {
    dmm: Map,
}

fn root_node(ui: &Ui, ty: TypeRef, name: &str) {
    if let Some(child) = ty.child(name) {
        tree_node(ui, child);
    }
}

fn tree_node(ui: &Ui, ty: TypeRef) {
    let mut children = ty.children();
    if children.is_empty() {
        ui.tree_node(im_str!("{}", ty.name)).leaf(true).build(||{});
    } else {
        children.sort_by_key(|t| &t.get().name);
        ui.tree_node(im_str!("{}", ty.name)).build(|| {
            for child in children {
                tree_node(ui, child);
            }
        });
    }
}
