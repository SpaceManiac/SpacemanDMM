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
extern crate divrem;
#[macro_use] extern crate serde_derive;
extern crate serde;
extern crate toml;
extern crate petgraph;
extern crate gfx_gl as gl;
extern crate weak_table;
extern crate slice_of_array;

extern crate dreammaker as dm;
extern crate dmm_tools;

mod support;
mod dmi;
mod map_repr;
mod map_renderer;
mod tasks;
mod tools;
mod config;
mod history;

use std::path::{Path, PathBuf};
use std::sync::{mpsc, Arc};
use std::borrow::Cow;

use imgui::*;

use dm::objtree::{ObjectTree, TypeRef};
use dmm_tools::dmm::{Map, Prefab};

use history::History;

use glutin::VirtualKeyCode as Key;
use gfx_device_gl::{Factory, Resources, CommandBuffer};
type Encoder = gfx::Encoder<Resources, CommandBuffer>;
type ColorFormat = gfx::format::Rgba8;
type DepthFormat = gfx::format::DepthStencil;
type RenderTargetView = gfx::handle::RenderTargetView<Resources, ColorFormat>;
type DepthStencilView = gfx::handle::DepthStencilView<Resources, DepthFormat>;
type Texture = gfx::handle::ShaderResourceView<Resources, [f32; 4]>;

use tasks::Task;

const RED_TEXT: &[(ImGuiCol, [f32; 4])] = &[(ImGuiCol::Text, [1.0, 0.25, 0.25, 1.0])];
const GREEN_TEXT: &[(ImGuiCol, [f32; 4])] = &[(ImGuiCol::Text, [0.25, 1.0, 0.25, 1.0])];

fn main() {
    support::run("SpacemanDMM".to_owned(), [0.25, 0.25, 0.5, 1.0]);
}

pub struct EditorScene {
    factory: Factory,
    target: RenderTargetView,
    depth: DepthStencilView,

    config: config::Config,
    map_renderer: map_renderer::MapRenderer,
    environment: Option<Environment>,

    tools: Vec<tools::Tool>,
    tool_current: usize,

    maps: Vec<EditorMap>,
    map_current: usize,
    new_map: Option<NewMap>,

    last_mouse_pos: (i32, i32),
    target_tile: Option<(u32, u32)>,
    context_tile: Option<(u32, u32)>,

    tasks: Vec<Task<TaskResult>>,
    errors: Vec<Box<std::error::Error>>,
    last_errors: usize,
    counter: usize,
    uid: usize,

    ui_lock_windows: bool,
    ui_style_editor: bool,
    ui_imgui_metrics: bool,
    ui_debug_mode: bool,
    ui_debug_window: bool,
    ui_errors: bool,
    ui_extra_vars: bool,
}

impl EditorScene {
    fn new(factory: &mut Factory, target: &RenderTargetView, depth: &DepthStencilView) -> Self {
        let mut ed = EditorScene {
            factory: factory.clone(),
            target: target.clone(),
            depth: depth.clone(),

            config: config::Config::load(),
            map_renderer: map_renderer::MapRenderer::new(factory, target),
            environment: None,

            tools: tools::configure(&ObjectTree::default()),
            tool_current: 0,

            maps: Vec::new(),
            map_current: 0,
            new_map: None,

            last_mouse_pos: (0, 0),
            target_tile: None,
            context_tile: None,

            tasks: Vec::new(),
            errors: Vec::new(),
            last_errors: 0,
            counter: 0,
            uid: 0,

            ui_lock_windows: true,
            ui_style_editor: false,
            ui_imgui_metrics: false,
            ui_debug_mode: cfg!(debug_assertions),
            ui_debug_window: true,
            ui_errors: false,
            ui_extra_vars: false,
        };
        ed.finish_init();
        ed
    }

    fn finish_init(&mut self) {
        // parse command-line arguments:
        // - use the specified DME, or autodetect one from the first DMM
        // - preload all maps specified belonging to that DME
        let mut loading_env = false;
        for arg in std::env::args_os() {
            let path = PathBuf::from(arg);

            if path.extension() == Some("dme".as_ref()) {
                // only one DME may be specified
                if !loading_env {
                    self.load_environment(path);
                    loading_env = true;
                }
            } else if path.extension() == Some("dmm".as_ref()) {
                // determine the corresponding DME
                if !loading_env {
                    if let Some(env) = detect_environment(&path) {
                        self.load_environment(env);
                        loading_env = true;
                    }
                }

                self.load_map(path);
            }
        }

        // Open most recent `.dme` by default.
        if !loading_env {
            if let Some(env_path) = self.config.recent.first().cloned() {
                self.load_environment(env_path);
            }
        }
    }

    fn update_render_target(&mut self, target: &RenderTargetView, depth: &DepthStencilView) {
        self.target = target.clone();
        self.depth = depth.clone();
    }

    fn render(&mut self, encoder: &mut Encoder) {
        let mut tasks = std::mem::replace(&mut self.tasks, Vec::new());
        tasks.retain(|task| task.poll(|res| match res {
            Err(e) => {
                self.errors.push(e);
            }
            Ok(TaskResult::ObjectTree(environment)) => {
                self.config.make_recent(&environment.path);
                self.config.save();
                self.tools = tools::configure(&environment.objtree);
                self.map_renderer.icons = Arc::new(dmi::IconCache::new(
                    &environment.path.parent().expect("invalid environment file path")));
                self.environment = Some(environment);
                for map in self.maps.iter_mut() {
                    for z in map.rendered.iter_mut() {
                        *z = None;
                    }
                }
            }
        }));
        tasks.extend(std::mem::replace(&mut self.tasks, Vec::new()));
        self.tasks = tasks;

        // TODO: error handling
        for map in self.maps.iter_mut() {
            map.state = match std::mem::replace(&mut map.state, MapState::Invalid) {
                MapState::Loading(rx) => if let Ok(val) = rx.try_recv() {
                    MapState::Pending(val)
                } else {
                    MapState::Loading(rx)
                },
                MapState::Pending(map) => if let Some(env) = self.environment.as_ref() {
                    let (tx, rx) = mpsc::channel();
                    let base = Arc::new(map);
                    let icons = self.map_renderer.icons.clone();
                    let objtree = env.objtree.clone();
                    let base2 = base.clone();
                    std::thread::spawn(move || {
                        let _ = tx.send(map_repr::AtomMap::new(&base2, &icons, &objtree));
                    });
                    MapState::Preparing(base, rx)
                } else {
                    MapState::Pending(map)
                },
                MapState::Preparing(base, rx) => if let Ok(val) = rx.try_recv() {
                    let merge_base = match Arc::try_unwrap(base) {
                        Ok(map) => map,
                        Err(arc) => (*arc).clone(),
                    };
                    let (x, y, z) = val.dim_xyz();
                    map.center = [x as f32 * 16.0, y as f32 * 16.0];
                    map.rendered.clear();
                    for _ in 0..z {
                        map.rendered.push(None);
                    }
                    MapState::Active { merge_base, hist: History::new("Loaded".to_owned(), val) }
                } else {
                    MapState::Preparing(base, rx)
                },
                other => other,
            };
        }

        self.render_map(false);
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(rendered) = map.rendered.get_mut(map.z_current).and_then(|x| x.as_mut()) {
                rendered.paint(&mut self.map_renderer, map.center, &mut self.factory, encoder, &self.target);
            }
        }
    }

    fn prepare_textures(&mut self, renderer: &mut imgui_gfx_renderer::Renderer<Resources>) {
        use tools::ToolIcon;

        for tool in self.tools.iter_mut() {
            tool.icon = match std::mem::replace(&mut tool.icon, ToolIcon::None) {
                ToolIcon::Dmi { icon, icon_state } => if self.environment.is_some() {
                    if let Some(id) = self.map_renderer.icons.get_index(icon.as_ref()) {
                        let icon = self.map_renderer.icons.get_icon(id);
                        if let Some([u1, v1, u2, v2]) = icon.uv_of(&icon_state, 2) {
                            let tex = self.map_renderer.icon_textures.retrieve(
                                &mut self.factory,
                                &self.map_renderer.icons,
                                id,
                            ).clone();
                            let samp = self.map_renderer.sampler.clone();
                            ToolIcon::Loaded {
                                tex: renderer.textures().insert((tex, samp)),
                                uv0: (u1, v1).into(),
                                uv1: (u2, v2).into(),
                            }
                        } else {
                            ToolIcon::None
                        }
                    } else {
                        ToolIcon::None
                    }
                } else {
                    ToolIcon::Dmi { icon, icon_state }
                },
                ToolIcon::EmbeddedPng { data } => if let Ok(tex) = dmi::texture_from_bytes(&mut self.factory, data) {
                    let samp = self.map_renderer.sampler.clone();
                    ToolIcon::Loaded {
                        tex: renderer.textures().insert((tex, samp)),
                        uv0: (0.0, 0.0).into(),
                        uv1: (1.0, 1.0).into(),
                    }
                } else {
                    ToolIcon::None
                },
                other => other,
            };
        }
    }

    fn run_ui(&mut self, ui: &Ui) -> bool {
        #[cfg(not(target_os = "macos"))]
        macro_rules! ctrl_shortcut {
            ($rest:expr) => (im_str!("Ctrl+{}", $rest))
        }
        #[cfg(target_os = "macos")]
        macro_rules! ctrl_shortcut {
            ($rest:expr) => (im_str!("Cmd+{}", $rest))
        }

        let mut continue_running = true;
        let mut window_positions_cond = match self.ui_lock_windows {
            false => ImGuiCond::FirstUseEver,
            true => ImGuiCond::Always,
        };

        ui.main_menu_bar(|| {
            ui.menu(im_str!("File")).build(|| {
                if ui.menu_item(im_str!("Open environment"))
                    .shortcut(ctrl_shortcut!("Shift+O"))
                    .build() { self.open_environment(); }
                ui.menu(im_str!("Recent environments")).enabled(!self.config.recent.is_empty()).build(|| {
                    let mut clicked = None;
                    for (i, path) in self.config.recent.iter().enumerate() {
                        if ui.menu_item(im_str!("{}", path.display()))
                            .shortcut(im_str!("{}", i + 1))
                            .build()
                        {
                            clicked = Some(path.to_owned());
                        }
                    }
                    if let Some(clicked) = clicked {
                        self.load_environment(clicked);
                    }
                });
                if ui.menu_item(im_str!("Update environment"))
                    .shortcut(ctrl_shortcut!("U"))
                    .build() { self.reload_objtree(); }
                ui.separator();
                if ui.menu_item(im_str!("New"))
                    .shortcut(ctrl_shortcut!("N"))
                    .build() { self.new_map(); }
                if ui.menu_item(im_str!("Open"))
                    .shortcut(ctrl_shortcut!("O"))
                    .build() { self.open_map(); }
                if ui.menu_item(im_str!("Close"))
                    .shortcut(ctrl_shortcut!("W"))
                    .build() { self.close_map(); }
                ui.separator();
                if ui.menu_item(im_str!("Save"))
                    .shortcut(ctrl_shortcut!("S"))
                    .build() { self.save_map(); }
                if ui.menu_item(im_str!("Save As"))
                    .shortcut(ctrl_shortcut!("Shift+S"))
                    .build() { self.save_map_as(false); }
                if ui.menu_item(im_str!("Save Copy As"))
                    .build() { self.save_map_as(true); }
                if ui.menu_item(im_str!("Save All"))
                    .build() { self.save_all(); }
                ui.separator();
                if ui.menu_item(im_str!("Exit"))
                    .shortcut(im_str!("Alt+F4"))
                    .build()
                {
                    continue_running = false;
                }
            });
            let (mut can_undo, mut can_redo) = (false, false);
            if let Some(map) = self.maps.get(self.map_current) {
                if let Some(hist) = map.state.hist() {
                    can_undo = hist.can_undo();
                    can_redo = hist.can_redo();
                }
            }
            ui.menu(im_str!("Edit")).build(|| {
                if ui.menu_item(im_str!("Undo"))
                    .shortcut(ctrl_shortcut!("Z"))
                    .enabled(can_undo)
                    .build() { self.undo(); }
                if ui.menu_item(im_str!("Redo"))
                    .shortcut(ctrl_shortcut!("Shift+Z"))
                    .enabled(can_redo)
                    .build() { self.redo(); }
                ui.separator();
                ui.menu_item(im_str!("Cut"))
                    .shortcut(ctrl_shortcut!("X"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Copy"))
                    .shortcut(ctrl_shortcut!("C"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Paste"))
                    .shortcut(ctrl_shortcut!("V"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Delete"))
                    .shortcut(im_str!("Del"))
                    .enabled(false)
                    .build();
                ui.separator();
                ui.menu_item(im_str!("Select All"))
                    .shortcut(ctrl_shortcut!("A"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Select None"))
                    .shortcut(ctrl_shortcut!("Shift+A"))
                    .enabled(false)
                    .build();
            });
            ui.menu(im_str!("Options")).build(|| {
                ui.menu_item(im_str!("Frame areas"))
                    .enabled(false)
                    .build();
                ui.menu_item(im_str!("Show extra variables"))
                    .selected(&mut self.ui_extra_vars)
                    .build();
                ui.separator();
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
                ui.menu_item(im_str!("Area"))
                    .shortcut(ctrl_shortcut!("1"))
                    .selected(&mut self.map_renderer.layers[1])
                    .build();
                ui.menu_item(im_str!("Turf"))
                    .shortcut(ctrl_shortcut!("2"))
                    .selected(&mut self.map_renderer.layers[2])
                    .build();
                ui.menu_item(im_str!("Obj"))
                    .shortcut(ctrl_shortcut!("3"))
                    .selected(&mut self.map_renderer.layers[3])
                    .build();
                ui.menu_item(im_str!("Mob"))
                    .shortcut(ctrl_shortcut!("4"))
                    .selected(&mut self.map_renderer.layers[4])
                    .build();
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
            });
            if self.ui_debug_mode {
                ui.menu(im_str!("Debug")).build(|| {
                    ui.menu_item(im_str!("Debug Window"))
                        .selected(&mut self.ui_debug_window)
                        .build();
                    ui.menu_item(im_str!("Style Editor"))
                        .selected(&mut self.ui_style_editor)
                        .build();
                    ui.menu_item(im_str!("ImGui Metrics"))
                        .selected(&mut self.ui_imgui_metrics)
                        .build();
                });
            }
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
            let mut i = 0;
            macro_rules! spinner {
                () => (SPINNER[(self.counter / 10 + { i += 1; i }) % SPINNER.len()])
            }

            for task in self.tasks.iter() {
                ui.separator();
                ui.text(im_str!("{} {}", spinner!(), task.name()));
            }
            for map in self.maps.iter() {
                if let Some(path) = map.path.as_ref() {
                    match map.state {
                        MapState::Loading(..) => {
                            ui.separator();
                            ui.text(im_str!("{} Loading {}", spinner!(), file_name(path)));
                        }
                        MapState::Preparing(..) => {
                            ui.separator();
                            ui.text(im_str!("{} Preparing {}", spinner!(), file_name(path)));
                        }
                        _ => {}
                    }
                }
            }

            if self.errors.len() > self.last_errors {
                ui.separator();
                if ui.menu_item(im_str!("{} errors", self.errors.len() - self.last_errors)).build() {
                    self.ui_errors = true;
                }
            }
        });

        ui.window(im_str!("Tools"))
            .position((10.0, 30.0), window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size((300.0, 300.0), ImGuiCond::FirstUseEver)
            .resizable(!self.ui_lock_windows)
            .build(|| {
                let (width, _) = ui.get_window_size();
                let count = std::cmp::max(((width - 16.0) / 42.0).floor() as usize, 1);
                for (i, tool) in self.tools.iter().enumerate() {
                    if i % count != 0 {
                        ui.same_line(0.0);
                    }

                    let col = if i == self.tool_current {
                        ui.imgui().style().colors[ImGuiCol::FrameBgActive as usize]
                    } else {
                        ui.imgui().style().colors[ImGuiCol::FrameBg as usize]
                    };
                    let clicked;
                    if let tools::ToolIcon::Loaded { tex, uv0, uv1 } = tool.icon {
                        ui.image(tex, (32.0, 32.0))
                            .uv0(uv0)
                            .uv1(uv1)
                            .border_col(col)
                            .tint_col(ui.imgui().style().colors[ImGuiCol::Text as usize])
                            .build();
                        clicked = ui.is_item_hovered() && ui.imgui().is_mouse_clicked(ImMouseButton::Left);
                    } else {
                        clicked = ui.button(im_str!("{}", tool.name), (32.0, 32.0));
                    }
                    if clicked {
                        self.tool_current = i;
                    }
                    if ui.is_item_hovered() {
                        ui.tooltip_text(&tool.name);
                    }
                }

                if let Some(tool) = self.tools.get_mut(self.tool_current) {
                    ui.separator();
                    if tool.help.is_empty() {
                        ui.text_wrapped(im_str!("{}", tool.name));
                    } else {
                        ui.text_wrapped(im_str!("{} - {}", tool.name, tool.help));
                    }
                    tool.behavior.settings(ui);
                }
            });

        ui.window(im_str!("Object Tree"))
            .position((10.0, 340.0), window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size((300.0, 400.0), ImGuiCond::FirstUseEver)
            .build(|| {
                if let Some(env) = self.environment.as_ref() {
                    let root = env.objtree.root();
                    root_node(ui, root, "area");
                    root_node(ui, root, "turf");
                    root_node(ui, root, "obj");
                    root_node(ui, root, "mob");
                } else {
                    ui.text(im_str!("The object tree is loading..."));
                }
            });

        let logical_size = ui.frame_size().logical_size;
        ui.window(im_str!("Maps"))
            .position((logical_size.0 as f32 - 230.0, 30.0), window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size((220.0, logical_size.1 as f32 - 40.0), window_positions_cond)
            .resizable(!self.ui_lock_windows)
            .build(|| {
                for (map_idx, map) in self.maps.iter_mut().enumerate() {
                    let title = match map.path {
                        Some(ref path) => format!("{}##map_{}", file_name(path), path.display()),
                        None => format!("Untitled##{}", map_idx),
                    };
                    if ui.collapsing_header(&ImString::from(title)).default_open(true).build() {
                        if let Some(dmm) = map.state.base_dmm() {
                            // TODO: use more up-to-date info?
                            ui.text(im_str!("{:?}; {}-keys: {}",
                                dmm.dim_xyz(),
                                dmm.key_length,
                                dmm.dictionary.len()));
                        }
                        if let Some(hist) = map.state.hist() {
                            let world = hist.current();
                            for z in 0..world.dim_xyz().2 {
                                if ui.small_button(im_str!("z = {}##map_{}_{}", z + 1, map_idx, z)) {
                                    self.map_current = map_idx;
                                    map.z_current = z as usize;
                                }
                            }
                        }
                    }
                }
            });

        if !ui.want_capture_mouse() {
            if ui.imgui().is_mouse_clicked(ImMouseButton::Left) {
                println!("lmb");
                if let Some(env) = self.environment.as_ref() {
                    println!("env");
                    if let Some(map) = self.maps.get_mut(self.map_current) {
                        println!("map");
                        let z = map.z_current as u32;
                        if let Some(hist) = map.state.hist_mut() {
                            println!("hist");
                            if let Some((x, y)) = self.target_tile {
                                println!("target tile");
                                // TODO: cloning these here is likely a bad idea
                                let icons = self.map_renderer.icons.clone();
                                let objtree = env.objtree.clone();
                                hist.edit("TODO".to_owned(), move |world| {
                                    println!("edit");
                                    let pop = world.add_pop(&Prefab {
                                        path: "/obj/item/lighter".to_owned(),
                                        vars: Default::default(),
                                    }, &icons, &objtree);
                                    world.add_instance((x, y, z), pop);
                                });
                            }
                        }
                    }
                }
            }
            if ui.imgui().is_mouse_clicked(ImMouseButton::Right) {
                if let Some(tile) = self.target_tile {
                    self.context_tile = Some(tile);
                    ui.open_popup(im_str!("context"));
                }
            }
        }
        if let Some((x, y)) = self.context_tile {
            let mut open = false;
            ui.popup(im_str!("context"), || {
                open = true;

                if let Some(map) = self.maps.get_mut(self.map_current) {
                    let z = map.z_current as u32;
                    let edit_atoms = &mut map.edit_atoms;
                    if let Some(hist) = map.state.hist() {
                        let current = hist.current();
                        for (inst, fab) in current.iter_instances((x, y, z)) {
                            let mut color_vars = RED_TEXT;
                            if let Some(env) = self.environment.as_ref() {
                                if env.objtree.find(&fab.path).is_some() {
                                    color_vars = &[];
                                }
                            }

                            ui.with_style_and_color_vars(&[], color_vars, || {
                                if ui.menu_item(im_str!("{}", fab.path)).build() {
                                    edit_atoms.push(EditAtom {
                                        inst,
                                        filter: ImString::with_capacity(128),
                                        fab: fab.clone(),
                                    });
                                }
                            });
                        }
                    }
                }
            });
            if !open {
                self.context_tile = None;
            }
        }

        self.new_map = self.new_map.take().and_then(|mut new_map| {
            let mut opened = true;
            let mut closed = false;

            ui.window(im_str!("New Map"))
                .opened(&mut opened)
                .resizable(false)
                .build(|| {
                    ui.input_int(im_str!("X"), &mut new_map.x).build();
                    ui.input_int(im_str!("Y"), &mut new_map.y).build();
                    ui.input_int(im_str!("Z"), &mut new_map.z).build();

                    if !new_map.created {
                        if ui.button(im_str!("New"), (80.0, 20.0)) {
                            new_map.created = true;
                        }
                    } else {
                        ui.button(im_str!("Wait..."), (80.0, 20.0));
                    }
                    ui.same_line(0.0);
                    if ui.button(im_str!("Cancel"), (80.0, 20.0)) {
                        closed = true;
                    }
                });
            new_map.x = std::cmp::min(std::cmp::max(new_map.x, 1), 512);
            new_map.y = std::cmp::min(std::cmp::max(new_map.y, 1), 512);
            new_map.z = std::cmp::min(std::cmp::max(new_map.z, 1), 32);

            if let Some(env) = self.environment.as_ref() {
                if new_map.created {
                    self.map_current = self.maps.len();
                    let mut rendered = Vec::new();
                    for _ in 0..new_map.z {
                        rendered.push(None);
                    }
                    let dmm = Map::new(new_map.x as usize, new_map.y as usize, new_map.z as usize,
                        env.turf.clone(), env.area.clone());
                    let atom_map = map_repr::AtomMap::new(&dmm, &self.map_renderer.icons, &env.objtree);
                    let desc = format!("New {}x{}x{} map", new_map.x, new_map.y, new_map.z);
                    self.maps.push(EditorMap {
                        path: None,
                        state: MapState::Active { merge_base: dmm, hist: History::new(desc, atom_map) },
                        z_current: 0,
                        center: [new_map.x as f32 * 16.0, new_map.y as f32 * 16.0],
                        rendered,
                        edit_atoms: Vec::new(),
                        uid: self.uid,
                    });
                    self.uid += 1;
                    opened = false;
                }
            }
            if opened && !closed { Some(new_map) } else { None }
        });

        for map in self.maps.iter_mut() {
            let env = self.environment.as_ref();
            let extra_vars = self.ui_extra_vars;
            let uid = map.uid;
            map.edit_atoms.retain_mut(|edit| {
                let mut keep = true;
                let mut keep2 = true;

                let EditAtom { ref mut fab, ref mut filter, .. } = edit;
                ui.window(im_str!("{}##{}/{:?}", fab.path, uid, edit.inst))
                    .opened(&mut keep)
                    .position(ui.imgui().mouse_pos(), ImGuiCond::Appearing)
                    .size((350.0, 500.0), ImGuiCond::FirstUseEver)
                    .horizontal_scrollbar(true)
                    .menu_bar(true)
                    .build(|| {
                        ui.menu_bar(|| {
                            if ui.menu_item(im_str!("Apply")).build() {
                                // TODO: actually apply
                                keep2 = false;
                            }
                            ui.separator();
                            ui.menu(im_str!("Filter...")).build(|| {
                                ui.input_text(im_str!(""), filter).build();
                                if ui.menu_item(im_str!("Clear")).build() {
                                    filter.clear();
                                }
                            });
                        });

                        // find the "best" type by chopping the path if needed
                        let mut ty = None;
                        let mut red_paths = true;
                        if let Some(env) = env {
                            let mut path = fab.path.as_str();
                            ty = env.objtree.find(path);
                            red_paths = ty.is_none();
                            while ty.is_none() && !path.is_empty() {
                                match path.rfind("/") {
                                    Some(idx) => path = &path[..idx],
                                    None => break,
                                }
                                ty = env.objtree.find(path);
                            }
                        }

                        // loop through instance vars, that type, parent types
                        // to find the longest var name for the column width
                        let mut max_len = 0;
                        for key in fab.vars.keys() {
                            max_len = std::cmp::max(max_len, key.len());
                        }
                        let mut search_ty = ty;
                        while let Some(search) = search_ty {
                            if search.is_root() {
                                break;
                            }
                            for (key, var) in search.vars.iter() {
                                if let Some(decl) = var.declaration.as_ref() {
                                    if !extra_vars && !decl.var_type.is_normal() {
                                        continue;
                                    }
                                    max_len = std::cmp::max(max_len, key.len());
                                }
                            }
                            search_ty = search.parent_type();
                        }
                        let offset = (max_len + 4) as f32 * 7.0;

                        // show the instance variables - everything which is
                        // actually set is right at the top
                        ui.text(im_str!("Instance variables ({})", fab.vars.len()));
                        for (name, value) in fab.vars.iter() {
                            if !name.contains(filter.to_str()) {
                                continue;
                            }
                            // TODO: red instead of green if invalid var
                            ui.with_style_and_color_vars(&[], GREEN_TEXT, || {
                                ui.text(im_str!("  {}", name));
                            });
                            ui.same_line(offset);
                            ui.text(im_str!("{}", value));
                        }

                        // show the red path on error
                        if red_paths {
                            ui.separator();
                            ui.with_style_and_color_vars(&[], RED_TEXT, || {
                                ui.text(im_str!("{}", &fab.path));
                            });
                        }

                        // show all the parent variables you could edit
                        let mut search_ty = ty;
                        while let Some(search) = search_ty {
                            if search.is_root() {
                                break;
                            }
                            ui.separator();
                            ui.text(im_str!("{} ({})", &search.path, search.vars.len()));

                            for (name, var) in search.vars.iter() {
                                if !name.contains(filter.to_str()) {
                                    continue;
                                }
                                if let Some(decl) = var.declaration.as_ref() {
                                    let mut prefix = " ";
                                    if !decl.var_type.is_normal() {
                                        if !extra_vars {
                                            continue;
                                        }
                                        prefix = "-";
                                    }
                                    ui.text(im_str!("{} {}", prefix, name));
                                    if prefix == "-" && ui.is_item_hovered() {
                                        ui.tooltip_text("/tmp, /static, or /const");
                                    }
                                    // search_ty is seeded with ty and must be Some to get here
                                    if let Some(value) = ty.unwrap().get_value(name) {
                                        if let Some(c) = value.constant.as_ref() {
                                            ui.same_line(offset);
                                            ui.text(im_str!("{}", c));
                                        }
                                    }
                                }
                            }

                            search_ty = search.parent_type();
                        }
                    });
                keep && keep2
            });
        }

        if self.ui_debug_mode {
            if self.ui_debug_window {
                let mut opened = self.ui_debug_window;
                ui.window(im_str!("Debug"))
                    .position((320.0, 30.0), ImGuiCond::FirstUseEver)
                    .always_auto_resize(true)
                    .opened(&mut opened)
                    .build(|| {
                        ui.text(im_str!("maps[{}], icons[{}], map = {}, zoom = {}",
                            self.maps.len(), self.map_renderer.icons.len(),
                            self.map_current, self.map_renderer.zoom));
                        if let Some(env) = self.environment.as_ref() {
                            ui.text(im_str!("turf = {}", env.turf));
                            ui.text(im_str!("area = {}", env.area));
                        }
                        if let Some(map) = self.maps.get(self.map_current) {
                            ui.text(im_str!("center = {:?}", map.center));
                            if let Some(rendered) = map.rendered.get(map.z_current).and_then(|x| x.as_ref()) {
                                ui.text(im_str!("draw_calls[{}], pops[{}], atoms[{}]", rendered.draw_calls(), rendered.pops_len, rendered.atoms_len));
                                ui.text(im_str!("timings: {:?}", rendered.duration));
                            }
                        }
                        if let Some((x, y)) = self.target_tile {
                            ui.text(im_str!("target: {}, {}", x, y));
                        }
                    });
                self.ui_debug_window = opened;
            }
            if self.ui_style_editor {
                ui.window(im_str!("Style Editor"))
                    .opened(&mut self.ui_style_editor)
                    .build(|| ui.show_default_style_editor());
            }
            if self.ui_imgui_metrics {
                ui.show_metrics_window(&mut self.ui_imgui_metrics);
            }
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

    fn mouse_moved(&mut self, (x, y): (i32, i32)) {
        self.last_mouse_pos = (x, y);
        self.target_tile = self.tile_under((x, y));
    }

    fn tile_under(&self, (x, y): (i32, i32)) -> Option<(u32, u32)> {
        if let Some(map) = self.maps.get(self.map_current) {
            if let Some(hist) = map.state.hist() {
                let (w, h, _, _) = self.target.get_dimensions();
                let (cx, cy) = (w / 2, h / 2);
                let tx = ((map.center[0].round() + (x as f32 - cx as f32) / self.map_renderer.zoom) / 32.0).floor() as i32;
                let ty = ((map.center[1].round() + (cy as f32 - y as f32) / self.map_renderer.zoom) / 32.0).floor() as i32;
                let (dim_x, dim_y, _) = hist.current().dim_xyz();
                if tx >= 0 && ty >= 0 && tx < dim_x as i32 && ty < dim_y as i32 {
                    return Some((tx as u32, ty as u32))
                }
            }
        }
        None
    }

    fn mouse_wheel(&mut self, ctrl: bool, shift: bool, alt: bool, _x: f32, y: f32) {
        if alt {
            if y > 0.0 && self.map_renderer.zoom < 16.0 {
                self.map_renderer.zoom *= 2.0;
            } else if y < 0.0 && self.map_renderer.zoom > 0.0625 {
                self.map_renderer.zoom *= 0.5;
            }
            self.target_tile = self.tile_under(self.last_mouse_pos);
            return;
        }
        let (axis, mut mul) = if ctrl { (0, -1.0) } else { (1, 1.0) };
        if shift {
            mul *= 8.0;
        }

        if let Some(map) = self.maps.get_mut(self.map_current) {
            map.center[axis] += 4.0 * 32.0 * mul * y / self.map_renderer.zoom;
            map.clamp_center();
        }
        self.target_tile = self.tile_under(self.last_mouse_pos);
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
            k!(Ctrl + N) => self.new_map(),
            k!(Ctrl + O) => self.open_map(),
            k!(Ctrl + W) => self.close_map(),
            k!(Ctrl + S) => self.save_map(),
            k!(Ctrl + Shift + S) => self.save_map_as(false),
            // Edit
            k!(Ctrl + Z) => self.undo(),
            k!(Ctrl + Shift + Z) | k!(Ctrl + Y) => self.redo(),
            // Layers
            k!(Ctrl + Key1) => self.toggle_layer(1),
            k!(Ctrl + Key2) => self.toggle_layer(2),
            k!(Ctrl + Key3) => self.toggle_layer(3),
            k!(Ctrl + Key4) => self.toggle_layer(4),
            // misc
            k!(Ctrl + R) => self.render_map(true),
            k!(Ctrl + Equals) |
            k!(Ctrl + Add) => if self.map_renderer.zoom < 16.0 { self.map_renderer.zoom *= 2.0 },
            k!(Ctrl + Subtract) |
            k!(Ctrl + Minus) => if self.map_renderer.zoom > 0.0625 { self.map_renderer.zoom *= 0.5 },
            k!(Ctrl + Tab) => self.tab_between_maps(1),
            k!(Ctrl + Shift + Tab) => self.tab_between_maps(-1),
            k!(F3) => self.ui_debug_mode = !self.ui_debug_mode,
            _ => {}
        }
    }

    // -----------------------------------------------------------------------

    fn reload_objtree(&mut self) {
        if let Some(env) = self.environment.as_ref().map(|e| &e.path).cloned() {
            self.load_environment(env);
        }
    }

    fn open_environment(&mut self) {
        if let Ok(nfd::Response::Okay(fname)) = nfd::open_file_dialog(Some("dme"), None) {
            self.load_environment(fname.into());
        }
    }

    fn load_environment(&mut self, path: PathBuf) {
        self.tasks.push(Task::spawn(format!("Loading {}", file_name(&path)), move || {
            use dm::constants::Constant;
            use dm::ast::PathOp;

            fn format_path(path: &[(PathOp, String)]) -> String {
                let mut s = String::new();
                for &(_, ref part) in path.iter() {
                    s.push_str("/");
                    s.push_str(part);
                }
                s
            }

            let context = dm::Context::default();
            let objtree = context.parse_environment(&path)?;

            let (mut turf, mut area) = ("/turf".to_owned(), "/area".to_owned());
            if let Some(world) = objtree.find("/world") {
                if let Some(turf_var) = world.get_value("turf") {
                    if let Some(Constant::Prefab(ref prefab)) = turf_var.constant {
                        turf = format_path(&prefab.path);
                    }
                }
                if let Some(area_var) = world.get_value("area") {
                    if let Some(Constant::Prefab(ref prefab)) = area_var.constant {
                        area = format_path(&prefab.path);
                    }
                }
            }

            Ok(TaskResult::ObjectTree(Environment {
                path,
                objtree: Arc::new(objtree),
                turf,
                area,
            }))
        }));
    }

    fn new_map(&mut self) {
        self.new_map = Some(NewMap { x: 32, y: 32, z: 1, created: false });
    }

    fn open_map(&mut self) {
        match nfd::open_file_multiple_dialog(Some("dmm"), None) {
            Ok(nfd::Response::Okay(fname)) => {
                self.load_map(fname.into());
            }
            Ok(nfd::Response::OkayMultiple(fnames)) => {
                for each in fnames {
                    self.load_map(each.into());
                }
            }
            _ => {}
        }
    }

    fn load_map(&mut self, path: PathBuf) {
        for (i, map) in self.maps.iter().enumerate() {
            // TODO: consider using same_file here
            if map.path.as_ref() == Some(&path) {
                self.map_current = i;
                return;
            }
        }

        let (tx, rx) = mpsc::channel();

        self.map_current = self.maps.len();
        self.maps.push(EditorMap {
            state: MapState::Loading(rx),
            path: Some(path.clone()),
            z_current: 0,
            center: [0.0, 0.0],
            rendered: Vec::new(),
            edit_atoms: Vec::new(),
            uid: self.uid,
        });
        self.uid += 1;

        std::thread::spawn(move || {
            let _ = tx.send(Map::from_file(&path).expect("TODO: proper error handling"));
        });
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
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist() {
                if map.path.is_none() {
                    if let Ok(nfd::Response::Okay(fname)) = nfd::open_save_dialog(Some("dmm"), None) {
                        map.path = Some(PathBuf::from(fname));
                    } else {
                        return;
                    }
                }
                self.errors.extend(hist.current().save(None).to_file(map.path.as_ref().unwrap()).err().map(From::from));
            }
        }
    }

    fn save_map_as(&mut self, copy: bool) {
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist() {
                if let Ok(nfd::Response::Okay(fname)) = nfd::open_save_dialog(Some("dmm"), None) {
                    let path = PathBuf::from(fname);
                    self.errors.extend(hist.current().save(None).to_file(&path).err().map(From::from));
                    if !copy {
                        map.path = Some(path);
                    }
                }
            }
        }
    }

    fn save_all(&mut self) {
        let current = self.map_current;
        self.map_current = 0;
        while self.map_current < self.maps.len() {
            self.save_map();
            self.map_current += 1;
        }
        self.map_current = current;
    }

    fn toggle_layer(&mut self, which: usize) {
        self.map_renderer.layers[which] = !self.map_renderer.layers[which];
    }

    fn render_map(&mut self, force: bool) {
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist() {
                if map.rendered[map.z_current].is_some() && !force {
                    return;
                }
                map.rendered[map.z_current] = Some(self.map_renderer.render(
                    hist.current(),
                    map.z_current as u32,
                    &mut self.factory,
                ));
            }
        }
    }

    fn tab_between_maps(&mut self, offset: isize) {
        if self.maps.is_empty() {
            return;
        }
        self.map_current = (self.map_current as isize + self.maps.len() as isize + offset) as usize % self.maps.len();
    }

    fn undo(&mut self) {
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist_mut() {
                hist.undo();
                // TODO: rerender
            }
        }
    }

    fn redo(&mut self) {
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist_mut() {
                hist.redo();
                // TODO: rerender
            }
        }
    }
}

struct Environment {
    path: PathBuf,
    objtree: Arc<ObjectTree>,
    turf: String,
    area: String,
}

struct EditorMap {
    path: Option<PathBuf>,
    z_current: usize,
    center: [f32; 2],
    state: MapState,
    rendered: Vec<Option<map_renderer::RenderedMap>>,
    edit_atoms: Vec<EditAtom>,
    uid: usize,
}

enum MapState {
    Invalid,
    Loading(mpsc::Receiver<Map>),
    Pending(Map),
    Preparing(Arc<Map>, mpsc::Receiver<map_repr::AtomMap>),
    Active {
        merge_base: Map,
        hist: History<map_repr::AtomMap>,
    }
}

impl EditorMap {
    fn clamp_center(&mut self) {
        if let Some(hist) = self.state.hist() {
            let (x, y, _) = hist.current().dim_xyz();
            self.center[0] = self.center[0].min(x as f32 * 32.0).max(0.0);
            self.center[1] = self.center[1].min(y as f32 * 32.0).max(0.0);
        }
    }
}

impl MapState {
    fn base_dmm(&self) -> Option<&Map> {
        match self {
            MapState::Pending(ref map) => Some(map),
            MapState::Preparing(ref map, _) => Some(map),
            MapState::Active { ref merge_base, .. } => Some(merge_base),
            _ => None,
        }
    }

    fn hist(&self) -> Option<&History<map_repr::AtomMap>> {
        match self {
            MapState::Active { ref hist, .. } => Some(hist),
            _ => None,
        }
    }

    fn hist_mut(&mut self) -> Option<&mut History<map_repr::AtomMap>> {
        match self {
            MapState::Active { ref mut hist, .. } => Some(hist),
            _ => None,
        }
    }
}

struct NewMap {
    x: i32,
    y: i32,
    z: i32,
    created: bool,
}

#[derive(Debug)]
struct EditAtom {
    inst: map_repr::InstanceId,
    filter: ImString,
    fab: Prefab,
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
    ObjectTree(Environment),
}

trait RetainMut<T> {
    fn retain_mut<F: FnMut(&mut T) -> bool>(&mut self, f: F);
}

impl<T> RetainMut<T> for Vec<T> {
    fn retain_mut<F: FnMut(&mut T) -> bool>(&mut self, mut f: F) {
        let len = self.len();
        let mut del = 0;
        for i in 0..len {
            if !f(&mut self[i]) {
                del += 1;
            } else if del > 0 {
                self.swap(i - del, i);
            }
        }
        if del > 0 {
            self.truncate(len - del);
        }
    }
}

fn detect_environment(path: &Path) -> Option<PathBuf> {
    let mut current = path.parent();
    while let Some(dir) = current {
        let read_dir = match std::fs::read_dir(dir) {
            Ok(r) => r,
            Err(_) => return None,
        };
        for entry in read_dir {
            let entry = match entry {
                Ok(e) => e,
                Err(_) => return None,
            };
            let path = entry.path();
            if path.extension() == Some("dme".as_ref()) {
                return Some(path);
            }
        }
        current = dir.parent();
    }
    None
}
