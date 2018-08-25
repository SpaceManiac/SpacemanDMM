//! The map editor proper, with a GUI and everything.

extern crate glium;
#[macro_use] extern crate imgui;
extern crate imgui_glium_renderer;
extern crate dreammaker as dm;

mod support;

use std::sync::mpsc;

pub use glium::glutin;
use imgui::*;

const CLEAR_COLOR: [f32; 4] = [0.1, 0.1, 0.1, 1.0];

fn main() {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let context = dm::Context::default();
        let env = dm::detect_environment("tgstation.dme")
            .expect("error detecting .dme")
            .expect("no .dme found");
        let objtree = context.parse_environment(&env)
            .expect("i/o error opening .dme");
        let _ = tx.send(objtree);
    });

    let mut objtree = None;
    support::run("SpacemanDMM".to_owned(), CLEAR_COLOR, |ui: &Ui| -> bool {
        if objtree.is_none() {
            objtree = rx.try_recv().ok();
        }

        ui.window(im_str!("Object Tree"))
            .size((300.0, 600.0), ImGuiCond::FirstUseEver)
            .build(|| {
                if let Some(objtree) = objtree.as_ref() {
                    let root = objtree.root();
                    root_node(ui, root, "area");
                    root_node(ui, root, "turf");
                    root_node(ui, root, "obj");
                    root_node(ui, root, "mob");
                } else {
                    ui.text(im_str!("The object tree is loading..."));
                }
            });
        true
    });
}

fn root_node(ui: &Ui, ty: dm::objtree::TypeRef, name: &str) {
    if let Some(child) = ty.child(name) {
        tree_node(ui, child);
    }
}

fn tree_node(ui: &Ui, ty: dm::objtree::TypeRef) {
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
