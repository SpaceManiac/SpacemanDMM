//! The map editor proper, with a GUI and everything.

#[macro_use] extern crate glium;
#[macro_use] extern crate imgui;
extern crate imgui_glium_renderer;
extern crate dreammaker as dm;
extern crate dmm_tools;

mod support;
mod map_renderer;

use std::sync::mpsc;

pub use glium::glutin;
use imgui::*;

use dm::objtree::{ObjectTree, TypeRef};
use dmm_tools::dmm::Map;

fn main() {
    support::run("SpacemanDMM".to_owned(), [0.25, 0.25, 0.5, 1.0]);
}

pub struct EditorScene {
    map_renderer: map_renderer::GliumTest,
    objtree: Option<ObjectTree>,
    dmm: Option<Map>,

    objtree_rx: mpsc::Receiver<ObjectTree>,
    dmm_rx: mpsc::Receiver<Map>,
}

impl EditorScene {
    fn new(display: &glium::Display) -> Self {
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
            map_renderer: map_renderer::GliumTest::new(display),
            objtree: None,
            dmm: None,

            objtree_rx,
            dmm_rx,
        }
    }

    fn render<S: glium::Surface>(&mut self, target: &mut S) {
        self.map_renderer.paint(target);
    }

    fn run_ui(&mut self, ui: &Ui) -> bool {
        if self.objtree.is_none() {
            self.objtree = self.objtree_rx.try_recv().ok();
        }
        if self.dmm.is_none() {
            self.dmm = self.dmm_rx.try_recv().ok();
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

        if let Some(dmm) = self.dmm.as_ref() {
            ui.window(im_str!("Map"))
                .position((410.0, 60.0), ImGuiCond::FirstUseEver)
                .size((300.0, 200.0), ImGuiCond::FirstUseEver)
                .build(|| {
                    ui.text(im_str!("key length: {}", dmm.key_length));
                    ui.text(im_str!("dictionary size: {}", dmm.dictionary.len()));
                });
        }
        true
    }
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
