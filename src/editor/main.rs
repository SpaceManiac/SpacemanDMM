//! The map editor proper, with a GUI and everything.
#![cfg_attr(not(debug_assertions), windows_subsystem="windows")]

extern crate glutin;
#[macro_use] extern crate gfx;
extern crate gfx_window_glutin;
extern crate gfx_device_gl;
#[macro_use] extern crate imgui;
extern crate imgui_gfx_renderer;
extern crate lodepng;
extern crate ndarray;
extern crate nfd;

extern crate dreammaker as dm;
extern crate dmm_tools;

mod support;
mod dmi;
mod map_renderer;
mod tasks;
mod tools;

use std::path::{Path, PathBuf};
use std::borrow::Cow;

use imgui::*;

use dm::objtree::{ObjectTree, TypeRef};
use dmm_tools::dmm::Map;

use glutin::VirtualKeyCode as Key;
use gfx_device_gl::{Factory, Resources, CommandBuffer};
type Encoder = gfx::Encoder<Resources, CommandBuffer>;
type ColorFormat = gfx::format::Rgba8;
type DepthFormat = gfx::format::DepthStencil;
type RenderTargetView = gfx::handle::RenderTargetView<Resources, ColorFormat>;
type Texture = gfx::handle::ShaderResourceView<Resources, [f32; 4]>;

use tasks::Task;

fn main() {
    support::run("SpacemanDMM".to_owned(), [0.25, 0.25, 0.5, 1.0]);
}

pub struct EditorScene {
    factory: Factory,
    map_renderer: map_renderer::MapRenderer,
    environment: Option<PathBuf>,
    objtree: Option<ObjectTree>,

    tools: Vec<tools::Tool>,
    tool_current: usize,

    maps: Vec<EditorMap>,
    map_current: usize,

    tasks: Vec<Task<TaskResult>>,
    errors: Vec<Box<std::error::Error>>,
    last_errors: usize,
    counter: usize,

    ui_lock_windows: bool,
    ui_style_editor: bool,
    ui_imgui_metrics: bool,
    ui_debug: bool,
    ui_errors: bool,
}

impl EditorScene {
    fn new(factory: &mut Factory, view: &RenderTargetView) -> Self {
        let mut ed = EditorScene {
            factory: factory.clone(),
            map_renderer: map_renderer::MapRenderer::new(factory, view),
            environment: None,
            objtree: None,

            tools: tools::configure(&ObjectTree::default()),
            tool_current: 0,

            maps: Vec::new(),
            map_current: 0,

            tasks: Vec::new(),
            errors: Vec::new(),
            last_errors: 0,
            counter: 0,

            ui_lock_windows: true,
            ui_style_editor: false,
            ui_imgui_metrics: false,
            ui_debug: true,
            ui_errors: false,
        };
        if let Ok(Some(env)) = dm::detect_environment("tgstation.dme") {
            ed.load_environment(env);
        }
        ed.load_map("_maps/map_files/debug/runtimestation.dmm".into());
        ed
    }

    fn mouse_wheel(&mut self, ctrl: bool, shift: bool, _alt: bool, _x: f32, y: f32) {
        let (axis, mut mul) = if ctrl { (0, -1.0) } else { (1, 1.0) };
        if shift {
            mul *= 8.0;
        }

        self.map_renderer.center[axis] += 4.0 * 32.0 * mul * y / self.map_renderer.zoom;
    }

    fn render(&mut self, factory: &mut Factory, encoder: &mut Encoder, view: &RenderTargetView) {
        let mut tasks = std::mem::replace(&mut self.tasks, Vec::new());
        tasks.retain(|task| task.poll(|res| match res {
            Err(e) => {
                self.errors.push(e);
            }
            Ok(TaskResult::ObjectTree(objtree)) => {
                self.tools = tools::configure(&objtree);
                self.map_renderer.icons.clear();
                if let Some(map) = self.maps.get_mut(self.map_current) {
                    map.rendered = Some(self.map_renderer.prepare(
                        factory,
                        &objtree,
                        &map.dmm,
                        map.dmm.z_level(map.z_current)));
                    self.map_renderer.recenter(&map.dmm);
                }
                self.objtree = Some(objtree);
            }
            Ok(TaskResult::Map(path, dmm)) => {
                let mut rendered = None;
                if let Some(objtree) = self.objtree.as_ref() {
                    rendered = Some(self.map_renderer.prepare(factory, &objtree, &dmm, dmm.z_level(0)));
                }
                self.map_current = self.maps.len();
                self.maps.push(EditorMap { path, dmm, z_current: 0, rendered });
            }
        }));
        tasks.extend(std::mem::replace(&mut self.tasks, Vec::new()));
        self.tasks = tasks;

        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(rendered) = map.rendered.as_mut() {
                rendered.paint(&mut self.map_renderer, encoder, view);
            }
        }
    }

    fn run_ui(&mut self, ui: &Ui) -> bool {
        let mut continue_running = true;
        let mut window_positions_cond = match self.ui_lock_windows {
            false => ImGuiCond::FirstUseEver,
            true => ImGuiCond::Always,
        };

        ui.main_menu_bar(|| {
            ui.menu(im_str!("File")).build(|| {
                if ui.menu_item(im_str!("Open environment"))
                    .shortcut(im_str!("Ctrl+Shift+O"))
                    .build() { self.open_environment(); }
                ui.menu(im_str!("Recent environments")).enabled(false).build(|| {
                    // TODO
                });
                if ui.menu_item(im_str!("Update environment"))
                    .shortcut(im_str!("Ctrl+U"))
                    .build() { self.reload_objtree(); }
                ui.separator();
                ui.menu_item(im_str!("New"))
                    .shortcut(im_str!("Ctrl+N"))
                    .enabled(false)
                    .build();
                if ui.menu_item(im_str!("Open"))
                    .shortcut(im_str!("Ctrl+O"))
                    .build() { self.open_map(); }
                if ui.menu_item(im_str!("Close"))
                    .shortcut(im_str!("Ctrl+W"))
                    .build() { self.close_map(); }
                ui.separator();
                if ui.menu_item(im_str!("Save"))
                    .shortcut(im_str!("Ctrl+S"))
                    .build() { self.save_map(); }
                if ui.menu_item(im_str!("Save As"))
                    .shortcut(im_str!("Ctrl+Shift+S"))
                    .build() { self.save_map_as(false); }
                if ui.menu_item(im_str!("Save Copy As"))
                    .build() { self.save_map_as(true); }
                ui.menu_item(im_str!("Save All"))
                    .enabled(false)
                    .build();
                ui.separator();
                if ui.menu_item(im_str!("Exit"))
                    .shortcut(im_str!("Alt+F4"))
                    .build()
                {
                    continue_running = false;
                }
            });
            ui.menu(im_str!("Edit")).build(|| {
                ui.menu_item(im_str!("Undo"))
                    .shortcut(im_str!("Ctrl+Z"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Redo"))
                    .shortcut(im_str!("Ctrl+Shift+Z"))
                    .enabled(false)
                    .build();
                ui.separator();
                ui.menu_item(im_str!("Cut"))
                    .shortcut(im_str!("Ctrl+X"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Copy"))
                    .shortcut(im_str!("Ctrl+C"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Paste"))
                    .shortcut(im_str!("Ctrl+V"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Delete"))
                    .shortcut(im_str!("Del"))
                    .enabled(false)
                    .build();
                ui.separator();
                ui.menu_item(im_str!("Select All"))
                    .shortcut(im_str!("Ctrl+A"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Select None"))
                    .shortcut(im_str!("Ctrl+Shift+A"))
                    .enabled(false)
                    .build();
            });
            ui.menu(im_str!("Zoom")).build(|| {
                for &zoom in [0.5, 1.0, 2.0, 4.0].iter() {
                    let mut selected = self.map_renderer.zoom == zoom;
                    if ui.menu_item(im_str!("{}%", 100.0 * zoom)).selected(&mut selected).build() {
                        self.map_renderer.zoom = zoom;
                    }
                }
            });
            ui.menu(im_str!("Layers")).build(|| {
                ui.menu_item(im_str!("Customize filters..."))
                    .enabled(false)
                    .build();
                ui.separator();
                if ui.menu_item(im_str!("Area"))
                    .shortcut(im_str!("Ctrl+1"))
                    .selected(&mut self.map_renderer.layers[1])
                    .build() { self.rerender_map(); }
                if ui.menu_item(im_str!("Turf"))
                    .shortcut(im_str!("Ctrl+2"))
                    .selected(&mut self.map_renderer.layers[2])
                    .build() { self.rerender_map(); }
                if ui.menu_item(im_str!("Obj"))
                    .shortcut(im_str!("Ctrl+3"))
                    .selected(&mut self.map_renderer.layers[3])
                    .build() { self.rerender_map(); }
                if ui.menu_item(im_str!("Mob"))
                    .shortcut(im_str!("Ctrl+4"))
                    .selected(&mut self.map_renderer.layers[4])
                    .build() { self.rerender_map(); }
            });
            ui.menu(im_str!("Window")).build(|| {
                ui.menu_item(im_str!("Lock positions"))
                    .selected(&mut self.ui_lock_windows)
                    .build();
                if ui.menu_item(im_str!("Reset positions")).enabled(!self.ui_lock_windows).build() {
                    window_positions_cond = ImGuiCond::Always;
                }
                ui.separator();
                ui.menu_item(im_str!("Errors"))
                    .selected(&mut self.ui_errors)
                    .build();
                ui.menu_item(im_str!("Debug Window"))
                    .selected(&mut self.ui_debug)
                    .build();
                ui.menu_item(im_str!("Style Editor"))
                    .selected(&mut self.ui_style_editor)
                    .build();
                ui.menu_item(im_str!("ImGui Metrics"))
                    .selected(&mut self.ui_imgui_metrics)
                    .build();
            });
            ui.menu(im_str!("Help")).build(|| {
                ui.menu_item(im_str!("About SpacemanDMM"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Open-source licenses"))
                    .enabled(false)
                    .build();
                ui.menu(im_str!("ImGui help")).build(|| {
                    ui.show_user_guide();
                });
            });

            const SPINNER: &[&str] = &["|", "/", "-", "\\"];
            self.counter = self.counter.wrapping_add(1);
            for (i, task) in self.tasks.iter().enumerate() {
                ui.separator();
                ui.text(im_str!("{} {}", SPINNER[(self.counter / 10 + i) % SPINNER.len()], task.name()));
            }

            if self.errors.len() > self.last_errors {
                ui.separator();
                if ui.menu_item(im_str!("{} errors", self.errors.len() - self.last_errors)).build() {
                    self.ui_errors = true;
                }
            }
        });
        if self.ui_style_editor {
            ui.window(im_str!("Style Editor"))
                .opened(&mut self.ui_style_editor)
                .build(|| ui.show_default_style_editor());
        }
        if self.ui_imgui_metrics {
            ui.show_metrics_window(&mut self.ui_imgui_metrics);
        }

        ui.window(im_str!("Tools"))
            .position((10.0, 30.0), window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size((300.0, 100.0), ImGuiCond::FirstUseEver)
            .resizable(!self.ui_lock_windows)
            .build(|| {
                for (i, tool) in self.tools.iter().enumerate() {
                    if ui.small_button(im_str!("{}", tool.name)) {
                        self.tool_current = i;
                    }
                    if ui.is_item_hovered() {
                        ui.tooltip_text("Test tooltip.");
                    }
                }
            });

        if let Some(tool) = self.tools.get_mut(self.tool_current) {
            // TODO: figure out how to give these all the same ID.
            ui.window(im_str!("{}", tool.name))
                .position((10.0, 140.0), window_positions_cond)
                .movable(!self.ui_lock_windows)
                .size((300.0, 190.0), ImGuiCond::FirstUseEver)
                .build(|| {
                    tool.behavior.settings(ui);
                });
        }

        ui.window(im_str!("Object Tree"))
            .position((10.0, 340.0), window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size((300.0, 400.0), ImGuiCond::FirstUseEver)
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
            .position((ui.frame_size().logical_size.0 as f32 - 310.0, 30.0), window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size((300.0, 200.0), window_positions_cond)
            .resizable(!self.ui_lock_windows)
            .build(|| {
                for (map_idx, map) in self.maps.iter_mut().enumerate() {
                    if ui.collapsing_header(im_str!("{}##map_{}", file_name(&map.path), map.path.display())).default_open(true).build() {
                        ui.text(im_str!("keys: {} / length: {}",
                            map.dmm.dictionary.len(),
                            map.dmm.key_length));
                        for z in 0..map.dmm.dim_z() {
                            if ui.small_button(im_str!("z = {}##map_{}_{}", z + 1, map_idx, z)) {
                                self.map_current = map_idx;
                                map.z_current = z;
                            }
                        }
                    }
                }
            });

        if self.ui_debug {
            let mut ui_debug = self.ui_debug;
            ui.window(im_str!("Debug"))
                .position((320.0, 30.0), ImGuiCond::FirstUseEver)
                .size((300.0, 100.0), ImGuiCond::FirstUseEver)
                .opened(&mut ui_debug)
                .build(|| {
                    ui.text(im_str!("zoom = {}, center = {:?}", self.map_renderer.zoom, self.map_renderer.center));
                    ui.text(im_str!("map = {}, maps[{}], icons[{}]", self.map_current, self.maps.len(), self.map_renderer.icons.len()));
                    if let Some(map) = self.maps.get(self.map_current) {
                        if let Some(rendered) = map.rendered.as_ref() {
                            ui.text(im_str!("draw_calls[{}], atoms[{}]", rendered.draw_calls(), rendered.atoms_len));
                            ui.text(im_str!("timings: {:?}", rendered.duration));
                        }
                    }
                });
            self.ui_debug = ui_debug;
        }

        if self.ui_errors {
            self.last_errors = self.errors.len();
            let mut ui_errors = self.ui_errors;
            ui.window(im_str!("Errors"))
                .position((320.0, 140.0), ImGuiCond::FirstUseEver)
                .size((300.0, 400.0), ImGuiCond::FirstUseEver)
                .horizontal_scrollbar(true)
                .opened(&mut ui_errors)
                .build(|| {
                    ui.text(im_str!("{:#?}", self.errors));
                });
            self.ui_errors = ui_errors;
        }

        continue_running
    }

    #[deny(unreachable_patterns)]
    fn chord(&mut self, ctrl: bool, shift: bool, alt: bool, key: Key) {
        use Key::*;
        macro_rules! k {
            (@[$ctrl:pat, $shift:pat, $alt:pat] $k:pat) => {
                ($ctrl, $shift, $alt, $k)
            };
            (@[$ctrl:pat, $shift:pat, $alt:pat] Ctrl + $($rest:tt)*) => {
                k!(@[true, $shift, $alt] $($rest)*)
            };
            (@[$ctrl:pat, $shift:pat, $alt:pat] Shift + $($rest:tt)*) => {
                k!(@[$ctrl, true, $alt] $($rest)*)
            };
            (@[$ctrl:pat, $shift:pat, $alt:pat] Alt + $($rest:tt)*) => {
                k!(@[$ctrl, $shift, true] $($rest)*)
            };
            (@$($rest:tt)*) => { error };
            ($($rest:tt)*) => {
                k!(@[false, false, false] $($rest)*)
            };
        }
        match (ctrl, shift, alt, key) {
            // File
            k!(Ctrl + Shift + O) => self.open_environment(),
            k!(Ctrl + U) => self.reload_objtree(),
            k!(Ctrl + O) => self.open_map(),
            k!(Ctrl + W) => self.close_map(),
            k!(Ctrl + S) => self.save_map(),
            k!(Ctrl + Shift + S) => self.save_map_as(false),
            // Layers
            k!(Ctrl + Key1) => self.toggle_layer(1),
            k!(Ctrl + Key2) => self.toggle_layer(2),
            k!(Ctrl + Key3) => self.toggle_layer(3),
            k!(Ctrl + Key4) => self.toggle_layer(4),
            // misc
            k!(Ctrl + R) => self.rerender_map(),
            k!(Ctrl + Equals) |
            k!(Ctrl + Add) => if self.map_renderer.zoom < 16.0 { self.map_renderer.zoom *= 2.0 },
            k!(Ctrl + Subtract) |
            k!(Ctrl + Minus) => if self.map_renderer.zoom > 0.0625 { self.map_renderer.zoom *= 0.5 },
            _ => {}
        }
    }

    // -----------------------------------------------------------------------

    fn reload_objtree(&mut self) {
        if let Some(env) = self.environment.as_ref().cloned() {
            self.load_environment(env);
        }
    }

    fn open_environment(&mut self) {
        if let Ok(nfd::Response::Okay(fname)) = nfd::open_file_dialog(Some("dme"), None) {
            self.load_environment(fname.into());
        }
    }

    fn load_environment(&mut self, path: PathBuf) {
        self.environment = Some(path.to_owned());
        self.tasks.push(Task::spawn(format!("Loading {}", file_name(&path)), move || {
            let context = dm::Context::default();
            Ok(TaskResult::ObjectTree(context.parse_environment(&path)?))
        }));
    }

    fn open_map(&mut self) {
        match nfd::open_file_multiple_dialog(Some("dmm"), None) {
            Ok(nfd::Response::Okay(fname)) => {
                self.load_map(fname.into());
            }
            Ok(nfd::Response::OkayMultiple(fnames)) => {
                for each in fnames {
                    // TODO: order these?
                    self.load_map(each.into());
                }
            }
            _ => {}
        }
    }

    fn load_map(&mut self, path: PathBuf) {
        for (i, map) in self.maps.iter().enumerate() {
            // TODO: consider using same_file here
            if map.path == path {
                self.map_current = i;
                return;
            }
        }
        self.tasks.push(Task::spawn(format!("Loading {}", file_name(&path)), move || {
            let map = Map::from_file(&path)?;
            Ok(TaskResult::Map(path, map))
        }));
    }

    fn close_map(&mut self) {
        // TODO: prompt to save if dirty
        if self.map_current + 1 == self.maps.len() {
            self.maps.remove(self.map_current);
            self.map_current = self.map_current.saturating_sub(1);
        } else if self.map_current + 1 < self.maps.len() {
            self.maps.remove(self.map_current);
        }
    }

    fn save_map(&mut self) {
        if let Some(map) = self.maps.get(self.map_current) {
            self.errors.extend(map.dmm.to_file(&map.path).err().map(From::from));
        }
    }

    fn save_map_as(&mut self, copy: bool) {
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Ok(nfd::Response::Okay(fname)) = nfd::open_save_dialog(Some("dmm"), None) {
                let path = PathBuf::from(fname);
                self.errors.extend(map.dmm.to_file(&path).err().map(From::from));
                if !copy {
                    map.path = path;
                }
            }
        }
    }

    fn toggle_layer(&mut self, which: usize) {
        // TODO: make this faster
        self.map_renderer.layers[which] = !self.map_renderer.layers[which];
        self.rerender_map();
    }

    fn rerender_map(&mut self) {
        if let Some(objtree) = self.objtree.as_ref() {
            if let Some(map) = self.maps.get_mut(self.map_current) {
                map.rendered = Some(self.map_renderer.prepare(
                    &mut self.factory,
                    &objtree,
                    &map.dmm,
                    map.dmm.z_level(map.z_current)));
            }
        }
    }
}

struct EditorMap {
    path: PathBuf,
    dmm: Map,
    z_current: usize,
    rendered: Option<map_renderer::RenderedMap>,
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

fn file_name(path: &Path) -> Cow<str> {
    path.file_name().map_or("".into(), |s| s.to_string_lossy())
}

enum TaskResult {
    ObjectTree(ObjectTree),
    Map(PathBuf, Map),
}
