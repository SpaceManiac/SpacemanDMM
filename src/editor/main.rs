//! The map editor proper, with a GUI and everything.
#![cfg_attr(not(debug_assertions), windows_subsystem="windows")]
#![allow(dead_code)]  // TODO: remove when this is not a huge WIP

extern crate glutin;
#[macro_use] extern crate gfx;
extern crate gfx_window_glutin;
extern crate gfx_device_gl;
extern crate imgui;
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

use glutin::VirtualKeyCode as Key;
use gfx_device_gl::{Factory, Resources, CommandBuffer};
type Encoder = gfx::Encoder<Resources, CommandBuffer>;
type ColorFormat = gfx::format::Rgba8;
type DepthFormat = gfx::format::DepthStencil;
type RenderTargetView = gfx::handle::RenderTargetView<Resources, ColorFormat>;
type DepthStencilView = gfx::handle::DepthStencilView<Resources, DepthFormat>;
type Texture = gfx::handle::ShaderResourceView<Resources, [f32; 4]>;
type ImRenderer = imgui_gfx_renderer::Renderer<ColorFormat, Resources>;

use dmi::IconCache;
type History = history::History<map_repr::AtomMap, Environment>;

const RED_TEXT: &[(StyleColor, [f32; 4])] = &[(StyleColor::Text, [1.0, 0.25, 0.25, 1.0])];
const GREEN_TEXT: &[(StyleColor, [f32; 4])] = &[(StyleColor::Text, [0.25, 1.0, 0.25, 1.0])];
const THUMBNAIL_SIZE: u16 = 186;

fn main() {
    support::run("SpacemanDMM".to_owned(), [0.25, 0.25, 0.5, 1.0]);
}

// ---------------------------------------------------------------------------
// Data structures

pub struct EditorScene {
    factory: Factory,
    target: RenderTargetView,
    depth: DepthStencilView,

    config: config::Config,
    map_renderer: map_renderer::MapRenderer,
    environment: Option<Environment>,
    loading_env: Option<LoadingEnvironment>,

    tools: Vec<tools::Tool>,
    tool_current: usize,

    maps: Vec<EditorMap>,
    map_current: usize,
    new_map: Option<NewMap>,

    last_mouse_pos: (i32, i32),
    target_tile: Option<(u32, u32)>,
    context_tile: Option<(u32, u32)>,

    errors: Vec<String>,
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
    stacked_rendering: bool,
    stacked_inverted: bool,

    mouse_drag_pos: Option<(i32, i32)>
}

pub struct Environment {
    path: PathBuf,
    objtree: Arc<ObjectTree>,
    icons: Arc<IconCache>,
    turf: String,
    area: String,
}

struct LoadingEnvironment {
    path: PathBuf,
    rx: mpsc::Receiver<Result<Environment, tasks::Err>>,
}

struct EditorMap {
    path: Option<PathBuf>,
    z_current: usize,
    center: [f32; 2],
    state: MapState,
    rendered: Vec<Option<map_renderer::RenderedMap>>,
    edit_atoms: Vec<EditInstance>,
    uid: usize,
}

enum MapState {
    Invalid,
    Loading(mpsc::Receiver<Map>),
    Pending(Arc<Map>),
    Preparing(Arc<Map>, mpsc::Receiver<map_repr::AtomMap>),
    Refreshing {
        merge_base: Arc<Map>,
        hist: History,
        rx: mpsc::Receiver<map_repr::AtomMap>,
    },
    Active {
        merge_base: Arc<Map>,
        hist: History,
    },
}

struct NewMap {
    x: i32,
    y: i32,
    z: i32,
    created: bool,
}

struct EditInstance {
    inst: map_repr::InstanceId,
    base: EditPrefab,
}

struct EditPrefab {
    filter: ImString,
    fab: Prefab,
}

// ---------------------------------------------------------------------------
// Editor scene, including rendering and UI

impl EditorScene {
    fn new(factory: &mut Factory, target: &RenderTargetView, depth: &DepthStencilView) -> Self {
        let mut ed = EditorScene {
            factory: factory.clone(),
            target: target.clone(),
            depth: depth.clone(),

            config: config::Config::load(),
            map_renderer: map_renderer::MapRenderer::new(factory, target),
            environment: None,
            loading_env: None,

            tools: tools::configure(&ObjectTree::default()),
            tool_current: 0,

            maps: Vec::new(),
            map_current: 0,
            new_map: None,

            last_mouse_pos: (0, 0),
            target_tile: None,
            context_tile: None,

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
            stacked_rendering: false,
            stacked_inverted: false,
            mouse_drag_pos: None,
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

    fn finish_loading_env(&mut self, environment: Environment) {
        self.config.make_recent(&environment.path);
        self.config.save();
        self.tools = tools::configure(&environment.objtree);
        self.map_renderer.icons = environment.icons.clone();
        self.map_renderer.icon_textures.clear();
        for map in self.maps.iter_mut() {
            for z in map.rendered.iter_mut() {
                *z = None;
            }
            // need to re-render maps if the environment has changed
            map.state = match std::mem::replace(&mut map.state, MapState::Invalid) {
                MapState::Pending(base) |
                MapState::Preparing(base, _) => {
                    let (tx, rx) = mpsc::channel();
                    let icons = environment.icons.clone();
                    let objtree = environment.objtree.clone();
                    let base2 = base.clone();
                    std::thread::spawn(move || {
                        let _ = tx.send(map_repr::AtomMap::new(&base2, &icons, &objtree));
                    });
                    MapState::Preparing(base, rx)
                },
                MapState::Active { merge_base, hist } => {
                    let mut new_map = hist.current().clone();
                    let icons = environment.icons.clone();
                    let objtree = environment.objtree.clone();
                    let (tx, rx) = mpsc::channel();
                    std::thread::spawn(move || {
                        new_map.refresh_pops(&icons, &objtree);
                        let _ = tx.send(new_map);
                    });
                    MapState::Refreshing { merge_base, hist, rx }
                },
                other => other,
            };
        }
        self.environment = Some(environment);
    }

    fn render(&mut self, encoder: &mut Encoder) {
        if let Some(loading) = self.loading_env.take() {
            match loading.rx.try_recv() {
                Ok(Ok(env)) => self.finish_loading_env(env),
                Ok(Err(e)) => {
                    self.errors.push(format!("Error(s) loading environment: {}", e));
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    self.errors.push("BUG: environment loading crashed".to_owned());
                }
                Err(mpsc::TryRecvError::Empty) => {
                    self.loading_env = Some(loading);
                }
            }
        }

        // TODO: error handling
        for map in self.maps.iter_mut() {
            map.state = match std::mem::replace(&mut map.state, MapState::Invalid) {
                MapState::Loading(rx) => if let Ok(val) = rx.try_recv() {
                    MapState::Pending(Arc::new(val))
                } else {
                    MapState::Loading(rx)
                },
                MapState::Pending(base) => if let Some(env) = self.environment.as_ref() {
                    let (tx, rx) = mpsc::channel();
                    let icons = env.icons.clone();
                    let objtree = env.objtree.clone();
                    let base2 = base.clone();
                    std::thread::spawn(move || {
                        let _ = tx.send(map_repr::AtomMap::new(&base2, &icons, &objtree));
                    });
                    MapState::Preparing(base, rx)
                } else {
                    MapState::Pending(base)
                },
                MapState::Preparing(merge_base, rx) => if let Ok(val) = rx.try_recv() {
                    let (x, y, z) = val.dim_xyz();
                    map.center = [x as f32 * 16.0, y as f32 * 16.0];
                    map.rendered.clear();
                    for _ in 0..z {
                        map.rendered.push(None);
                    }
                    MapState::Active {
                        merge_base,
                        hist: History::new("Loaded".to_owned(), val),
                    }
                } else {
                    MapState::Preparing(merge_base, rx)
                },
                MapState::Refreshing {
                    merge_base,
                    mut hist,
                    rx,
                } => if let Ok(val) = rx.try_recv() {
                    hist.replace_current(val);
                    MapState::Active { merge_base, hist }
                } else {
                    MapState::Refreshing { merge_base, hist, rx }
                },
                other => other,
            };
        }

        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist() {
                let map_renderer = &mut self.map_renderer;
                let factory = &mut self.factory;
                let mut levels = Vec::new();
                if !self.stacked_rendering {  // normal rendering
                    levels.push(map.z_current);
                } else if !self.stacked_inverted {  // stacked rendering
                    levels.extend((map.z_current..map.rendered.len()).rev());
                } else {  // inverted stacked rendering
                    levels.extend(0..=map.z_current);
                }

                for z in levels {
                    if let Some(rendered) = map.rendered.get_mut(z) {
                        rendered.fulfill(|| {
                            map_renderer.render(
                                hist.current(),
                                z as u32,
                            )
                        }).paint(
                            map_renderer,
                            hist.current(),
                            z as u32,
                            map.center,
                            factory,
                            encoder,
                            &self.target,
                        );
                    }
                }
            }
        }
    }

    fn run_ui(&mut self, ui: &Ui, renderer: &mut ImRenderer) -> bool {
        for tool in self.tools.iter_mut() {
            tool.icon.prepare(
                self.environment.as_ref(),
                &mut tools::IconCtx::new(renderer, &mut self.map_renderer),
            );
        }

        #[cfg(not(target_os = "macos"))]
        macro_rules! ctrl_shortcut {
            ($rest:expr) => {
                &im_str!("Ctrl+{}", $rest)
            };
        }
        #[cfg(target_os = "macos")]
        macro_rules! ctrl_shortcut {
            ($rest:expr) => {
                &im_str!("Cmd+{}", $rest)
            };
        }

        let mut continue_running = true;
        let mut window_positions_cond = match self.ui_lock_windows {
            false => Condition::FirstUseEver,
            true => Condition::Always,
        };

        ui.main_menu_bar(|| {
            ui.menu(im_str!("File"), true, || {
                let some_map = self.maps.get(self.map_current).is_some();
                if MenuItem::new(im_str!("Open environment"))
                    .shortcut(ctrl_shortcut!("Shift+O"))
                    .build(ui) { self.open_environment(); }
                ui.menu(im_str!("Recent environments"), !self.config.recent.is_empty(), || {
                    let mut clicked = None;
                    for (i, path) in self.config.recent.iter().enumerate() {
                        if MenuItem::new(&im_str!("{}", path.display()))
                            .shortcut(&im_str!("{}", i + 1))
                            .build(ui)
                        {
                            clicked = Some(path.to_owned());
                        }
                    }
                    if let Some(clicked) = clicked {
                        self.load_environment(clicked);
                    }
                });
                if MenuItem::new(im_str!("Update environment"))
                    .shortcut(ctrl_shortcut!("U"))
                    .enabled(self.environment.is_some())
                    .build(ui) { self.reload_objtree(); }
                ui.separator();
                if MenuItem::new(im_str!("New"))
                    .shortcut(ctrl_shortcut!("N"))
                    .build(ui) { self.new_map(); }
                if MenuItem::new(im_str!("Open"))
                    .shortcut(ctrl_shortcut!("O"))
                    .build(ui) { self.open_map(); }
                if MenuItem::new(im_str!("Close"))
                    .shortcut(ctrl_shortcut!("W"))
                    .enabled(some_map)
                    .build(ui) { self.close_map(); }
                ui.separator();
                if MenuItem::new(im_str!("Save"))
                    .shortcut(ctrl_shortcut!("S"))
                    .enabled(some_map)
                    .build(ui) { self.save_map(); }
                if MenuItem::new(im_str!("Save As"))
                    .shortcut(ctrl_shortcut!("Shift+S"))
                    .enabled(some_map)
                    .build(ui) { self.save_map_as(false); }
                if MenuItem::new(im_str!("Save Copy As"))
                    .enabled(some_map)
                    .build(ui) { self.save_map_as(true); }
                if MenuItem::new(im_str!("Save All"))
                    .enabled(!self.maps.is_empty())
                    .build(ui) { self.save_all(); }
                ui.separator();
                if MenuItem::new(im_str!("Exit"))
                    .shortcut(im_str!("Alt+F4"))
                    .build(ui)
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
            ui.menu(im_str!("Edit"), true, || {
                if MenuItem::new(im_str!("Undo"))
                    .shortcut(ctrl_shortcut!("Z"))
                    .enabled(can_undo)
                    .build(ui) { self.undo(); }
                if MenuItem::new(im_str!("Redo"))
                    .shortcut(ctrl_shortcut!("Shift+Z"))
                    .enabled(can_redo)
                    .build(ui) { self.redo(); }
                ui.separator();
                MenuItem::new(im_str!("Cut"))
                    .shortcut(ctrl_shortcut!("X"))
                    .enabled(false)
                    .build(ui);
                MenuItem::new(im_str!("Copy"))
                    .shortcut(ctrl_shortcut!("C"))
                    .enabled(false)
                    .build(ui);
                MenuItem::new(im_str!("Paste"))
                    .shortcut(ctrl_shortcut!("V"))
                    .enabled(false)
                    .build(ui);
                MenuItem::new(im_str!("Delete"))
                    .shortcut(im_str!("Del"))
                    .enabled(false)
                    .build(ui);
                ui.separator();
                MenuItem::new(im_str!("Select All"))
                    .shortcut(ctrl_shortcut!("A"))
                    .enabled(false)
                    .build(ui);
                MenuItem::new(im_str!("Select None"))
                    .shortcut(ctrl_shortcut!("Shift+A"))
                    .enabled(false)
                    .build(ui);
            });
            ui.menu(im_str!("Options"), true, || {
                MenuItem::new(im_str!("Frame areas"))
                    .enabled(false)
                    .build(ui);
                MenuItem::new(im_str!("Show extra variables"))
                    .build_checkbox(ui, &mut self.ui_extra_vars);
                ui.separator();
                MenuItem::new(im_str!("Stacked rendering"))
                    .build_checkbox(ui, &mut self.stacked_rendering);
                MenuItem::new(im_str!("Invert order"))
                    .build_checkbox(ui, &mut self.stacked_inverted);
                ui.separator();
                for &zoom in [0.5, 1.0, 2.0, 4.0].iter() {
                    if MenuItem::new(&im_str!("{}%", 100.0 * zoom)).selected(self.map_renderer.zoom == zoom).build(ui) {
                        self.map_renderer.zoom = zoom;
                    }
                }
            });
            ui.menu(im_str!("Layers"), true, || {
                MenuItem::new(im_str!("Customize filters..."))
                    .enabled(false)
                    .build(ui);
                ui.separator();
                MenuItem::new(im_str!("Area"))
                    .shortcut(ctrl_shortcut!("1"))
                    .build_checkbox(ui, &mut self.map_renderer.layers[1]);
                MenuItem::new(im_str!("Turf"))
                    .shortcut(ctrl_shortcut!("2"))
                    .build_checkbox(ui, &mut self.map_renderer.layers[2]);
                MenuItem::new(im_str!("Obj"))
                    .shortcut(ctrl_shortcut!("3"))
                    .build_checkbox(ui, &mut self.map_renderer.layers[3]);
                MenuItem::new(im_str!("Mob"))
                    .shortcut(ctrl_shortcut!("4"))
                    .build_checkbox(ui, &mut self.map_renderer.layers[4]);
            });
            ui.menu(im_str!("Window"), true, || {
                MenuItem::new(im_str!("Lock positions"))
                    .build_checkbox(ui, &mut self.ui_lock_windows);
                if MenuItem::new(im_str!("Reset positions")).enabled(!self.ui_lock_windows).build(ui) {
                    window_positions_cond = Condition::Always;
                }
                ui.separator();
                MenuItem::new(im_str!("Errors"))
                    .build_checkbox(ui, &mut self.ui_errors);
            });
            if self.ui_debug_mode {
                ui.menu(im_str!("Debug"), true, || {
                    MenuItem::new(im_str!("Debug Window"))
                        .build_checkbox(ui, &mut self.ui_debug_window);
                    MenuItem::new(im_str!("Style Editor"))
                        .build_checkbox(ui, &mut self.ui_style_editor);
                    MenuItem::new(im_str!("ImGui Metrics"))
                        .build_checkbox(ui, &mut self.ui_imgui_metrics);
                    if MenuItem::new(im_str!("Disable"))
                        .shortcut(im_str!("F3"))
                        .build(ui) { self.ui_debug_mode = false; }
                });
            }
            ui.menu(im_str!("Help"), true, || {
                ui.menu(im_str!("About SpacemanDMM"), true, || {
                    ui.text(&im_str!(
                        "{} {}  Copyright (C) 2017-2019  Tad Hardesty",
                        env!("CARGO_PKG_NAME"),
                        env!("CARGO_PKG_VERSION"),
                    ));
                    ui.text(&im_str!("{}", include_str!(concat!(env!("OUT_DIR"), "/build-info.txt"))));
                    ui.text(im_str!("This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
                        and you are welcome to redistribute it under the conditions of the GNU\n\
                        General Public License version 3."));
                });
                MenuItem::new(im_str!("Open-source licenses"))
                    .enabled(false)
                    .build(ui);
                ui.menu(im_str!("ImGui help"), true, || {
                    ui.show_user_guide();
                });
            });

            const SPINNER: &[&str] = &["|", "/", "-", "\\"];
            self.counter = self.counter.wrapping_add(1);
            let mut i = 0;
            macro_rules! spinner {
                () => {
                    SPINNER[(self.counter / 10 + {
                        i += 1;
                        i
                    }) % SPINNER.len()]
                };
            }

            if let Some(loading) = self.loading_env.as_ref() {
                ui.separator();
                ui.text(im_str!("{} Loading {}", spinner!(), file_name(&loading.path)));
            }
            for map in self.maps.iter() {
                if let Some(path) = map.path.as_ref() {
                    match map.state {
                        MapState::Loading(..) => {
                            ui.separator();
                            ui.text(im_str!("{} Loading {}", spinner!(), file_name(path)));
                        }
                        MapState::Refreshing { .. } |
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
                if MenuItem::new(&im_str!("{} errors", self.errors.len() - self.last_errors)).build(ui) {
                    self.ui_errors = true;
                }
            }
        });

        Window::new(im_str!("Tools"))
            .position([10.0, 30.0], window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size([300.0, 300.0], Condition::FirstUseEver)
            .resizable(!self.ui_lock_windows)
            .build(ui, || {
                let count = ui.fits_width(34.0);  // 32 + 2px border
                for (i, tool) in self.tools.iter().enumerate() {
                    if i % count != 0 {
                        ui.same_line(0.0);
                    }

                    if ui.tool_icon(i == self.tool_current, &tool.icon, &im_str!("{}", tool.name)) {
                        self.tool_current = i;
                    }
                    if ui.is_item_hovered() {
                        ui.tooltip_text(&tool.name);
                    }
                }

                if let Some(tool) = self.tools.get_mut(self.tool_current) {
                    ui.separator();
                    if tool.help.is_empty() {
                        ui.text_wrapped(&im_str!("{}", tool.name));
                    } else {
                        ui.text_wrapped(&im_str!("{} - {}", tool.name, tool.help));
                    }
                    if let Some(ref env) = self.environment {
                        tool.behavior.settings(ui, env, &mut tools::IconCtx::new(renderer, &mut self.map_renderer));
                    } else if self.loading_env.is_some() {
                        ui.text(im_str!("The environment is loading..."));
                    } else {
                        ui.text(im_str!("Load an environment to get started."));
                    }
                }
            });

        Window::new(im_str!("Object Tree"))
            .position([10.0, 340.0], window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size([300.0, 400.0], Condition::FirstUseEver)
            .build(ui, || {
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

        let logical_size = ui.io().display_size;
        Window::new(im_str!("Maps"))
            .position([logical_size[0] as f32 - 230.0, 30.0], window_positions_cond)
            .movable(!self.ui_lock_windows)
            .size([THUMBNAIL_SIZE as f32 + 34.0, logical_size[1] as f32 - 40.0], window_positions_cond)
            .resizable(!self.ui_lock_windows)
            .build(ui, || {
                for (map_idx, map) in self.maps.iter_mut().enumerate() {
                    let dirty = map.state.hist().map_or(false, |h| h.is_dirty());
                    let title = match map.path {
                        Some(ref path) => format!(
                            "{}{}##map_{}",
                            file_name(path),
                            if dirty { " *" } else { "" },
                            path.display()
                        ),
                        None => format!("Untitled##{}", map_idx),
                    };
                    if ui.collapsing_header(&ImString::from(title)).default_open(true).build() {
                        if let Some(hist) = map.state.hist() {
                            let world = hist.current();
                            let dims = world.dim_xyz();
                            if let Some(dmm) = map.state.base_dmm() {
                                ui.text(im_str!(
                                    "{:?}; {}-keys: {}",
                                    dims,
                                    dmm.key_length,
                                    dmm.dictionary.len()
                                ));
                            } else {
                                ui.text(im_str!("{:?}", dims));
                            }
                            for z in 0..dims.2 {
                                let clicked;
                                if let Some(&mut Some(ref mut rendered)) = map.rendered.get_mut(z as usize) {
                                    let map_renderer::RenderedMap { ref mut thumbnail_id, ref thumbnail, .. } = rendered;
                                    let map_renderer = &self.map_renderer;
                                    let tex = *thumbnail_id.fulfill(|| {
                                        renderer.textures().insert((thumbnail.clone(), map_renderer.sampler.clone()))
                                    });
                                    let start = ui.cursor_screen_pos();
                                    let is_current = self.map_current == map_idx && map.z_current == z as usize;
                                    Image::new(tex, [THUMBNAIL_SIZE as f32, THUMBNAIL_SIZE as f32])
                                        .border_col(ui.frame_color(is_current))
                                        .build(ui);
                                    clicked = ui.is_item_hovered() && ui.is_mouse_clicked(MouseButton::Left);
                                    if clicked || (is_current && ui.is_item_hovered() && ui.is_mouse_down(MouseButton::Left)) {
                                        let pos = ui.io().mouse_pos;
                                        map.center = [
                                            (pos[0] - start[0] - 1.0) * 32.0 * dims.0 as f32 / THUMBNAIL_SIZE as f32,
                                            (THUMBNAIL_SIZE as f32 - (pos[1] - start[1] - 1.0)) * 32.0 * dims.1 as f32 / THUMBNAIL_SIZE as f32,
                                        ];
                                    }
                                } else {
                                    clicked = ui.button(
                                        &im_str!("z = {}##map_{}_{}", z + 1, map_idx, z),
                                        [THUMBNAIL_SIZE as f32 + 2.0, THUMBNAIL_SIZE as f32 + 2.0],
                                    );
                                }
                                if clicked {
                                    self.map_current = map_idx;
                                    map.z_current = z as usize;
                                }
                            }
                        } else if let Some(dmm) = map.state.base_dmm() {
                            ui.text(im_str!(
                                "{:?}; {}-keys: {}",
                                dmm.dim_xyz(),
                                dmm.key_length,
                                dmm.dictionary.len()
                            ));
                        }
                    }
                }
            });

        if !ui.io().want_capture_mouse {
            if ui.is_mouse_clicked(MouseButton::Left) {
                if let Some(env) = self.environment.as_ref() {
                    if let Some(map) = self.maps.get_mut(self.map_current) {
                        let z = map.z_current as u32;
                        if let Some(hist) = map.state.hist_mut() {
                            if let Some((x, y)) = self.target_tile {
                                if let Some(tool) = self.tools.get_mut(self.tool_current) {
                                    tool.behavior.click(hist, env, (x, y, z));
                                }
                            }
                        }
                    }
                }
            }
            if ui.is_mouse_clicked(MouseButton::Right) {
                if let Some(tile) = self.target_tile {
                    self.context_tile = Some(tile);
                    ui.open_popup(im_str!("context"));
                }
            }

            if ui.is_mouse_down(MouseButton::Middle) && self.mouse_drag_pos.is_none() {
                self.mouse_drag_pos = Some(self.last_mouse_pos);
            }
        }

        if !ui.is_mouse_down(MouseButton::Middle) && self.mouse_drag_pos.is_some() {
            self.mouse_drag_pos = None;
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

                            {
                                let style = ui.push_style_colors(color_vars);
                                if MenuItem::new(&im_str!("{}", fab.path)).build(ui) {
                                    edit_atoms.push(EditInstance {
                                        inst,
                                        base: EditPrefab::new(fab.clone()),
                                    });
                                }
                                style.pop(ui);
                            }
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

            Window::new(im_str!("New Map"))
                .opened(&mut opened)
                .resizable(false)
                .build(ui, || {
                    ui.input_int(im_str!("X"), &mut new_map.x).build();
                    ui.input_int(im_str!("Y"), &mut new_map.y).build();
                    ui.input_int(im_str!("Z"), &mut new_map.z).build();

                    if !new_map.created {
                        if ui.button(im_str!("New"), [80.0, 20.0]) {
                            new_map.created = true;
                        }
                    } else {
                        ui.button(im_str!("Wait..."), [80.0, 20.0]);
                    }
                    ui.same_line(0.0);
                    if ui.button(im_str!("Cancel"), [80.0, 20.0]) {
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
                    let dmm = Map::new(
                        new_map.x as usize,
                        new_map.y as usize,
                        new_map.z as usize,
                        env.turf.clone(),
                        env.area.clone(),
                    );
                    let atom_map = map_repr::AtomMap::new(&dmm, &env.icons, &env.objtree);
                    let desc = format!("New {}x{}x{} map", new_map.x, new_map.y, new_map.z);
                    self.maps.push(EditorMap {
                        path: None,
                        state: MapState::Active {
                            merge_base: Arc::new(dmm),
                            hist: History::new(desc, atom_map),
                        },
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
            if opened && !closed {
                Some(new_map)
            } else {
                None
            }
        });

        if let Some(map) = self.maps.get_mut(self.map_current) {
            let env = self.environment.as_ref();
            let extra_vars = self.ui_extra_vars;
            let uid = map.uid;
            let mut delete = Vec::new();
            let tools = &mut self.tools;
            let tool_current = self.tool_current;
            map.edit_atoms.retain_mut(|edit| {
                let mut keep = true;
                let mut keep2 = true;

                let EditInstance {
                    ref mut inst,
                    ref mut base,
                } = edit;
                Window::new(&im_str!("{}##{}/{:?}", base.fab.path, uid, inst))
                    .opened(&mut keep)
                    .position(ui.io().mouse_pos, Condition::Appearing)
                    .size([350.0, 500.0], Condition::FirstUseEver)
                    .horizontal_scrollbar(true)
                    .menu_bar(true)
                    .build(ui, || {
                        ui.menu_bar(|| {
                            if MenuItem::new(im_str!("Apply")).build(ui) {
                                // TODO: actually apply
                                keep2 = false;
                            }
                            ui.separator();
                            if MenuItem::new(im_str!("Delete")).build(ui) {
                                keep2 = false;
                                delete.push(inst.clone());
                            }
                            ui.separator();
                            if MenuItem::new(im_str!("Pick")).build(ui) {
                                if let Some(tool) = tools.get_mut(tool_current) {
                                    if let Some(env) = env {
                                        tool.behavior.pick(&env, &base.fab);
                                    }
                                }
                            }
                            ui.separator();
                            base.menu(ui);
                        });
                        base.show(ui, env, extra_vars);
                    });
                keep && keep2
            });
            if let Some(hist) = map.state.hist_mut() {
                if let Some(env) = env {
                    for id in delete.into_iter() {
                        hist.edit(env, "TODO".to_owned(), move |_, world| {
                            let removed = world.remove_instance(id.clone());
                            Box::new(move |env, world| {
                                world.undo_remove_instance(&removed, &env.icons, &env.objtree);
                            })
                        })
                    }
                }
            }

            // Handle mouse dragging.
            if let Some(pos) = self.mouse_drag_pos {
                let diff = (self.last_mouse_pos.0 - pos.0, self.last_mouse_pos.1 - pos.1);
                map.center[0] -= diff.0 as f32 / self.map_renderer.zoom;
                map.center[1] += diff.1 as f32 / self.map_renderer.zoom;
                self.mouse_drag_pos = Some(self.last_mouse_pos);
            }
        }

        if self.ui_debug_mode {
            if self.ui_debug_window {
                let mut opened = self.ui_debug_window;
                Window::new(im_str!("Debug"))
                    .position([320.0, 30.0], Condition::FirstUseEver)
                    .always_auto_resize(true)
                    .opened(&mut opened)
                    .build(ui, || {
                        ui.text(im_str!(
                            "maps[{}], map = {}, zoom = {}",
                            self.maps.len(),
                            self.map_current,
                            self.map_renderer.zoom
                        ));
                        if let Some(env) = self.environment.as_ref() {
                            ui.text(im_str!(
                                "types[{}], icons[{}]",
                                env.objtree.graph.node_count(),
                                env.icons.len()
                            ));
                            ui.text(im_str!("turf = {}", env.turf));
                            ui.text(im_str!("area = {}", env.area));
                        }
                        if let Some(map) = self.maps.get(self.map_current) {
                            ui.text(im_str!("center = {:?}", map.center));
                            if let Some(hist) = map.state.hist() {
                                let current = hist.current();
                                if let Some(level) = current.levels.get(map.z_current) {
                                    ui.text(im_str!(
                                        "draw_calls[{}], pops[{}], atoms[{}]",
                                        level.draw_calls.len(),
                                        current.pops.len(),
                                        level.instances.len()
                                    ));
                                }
                            }
                            if let Some(rendered) = map.rendered.get(map.z_current).and_then(|x| x.as_ref()) {
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
                Window::new(im_str!("Style Editor"))
                    .opened(&mut self.ui_style_editor)
                    .build(ui, || ui.show_default_style_editor());
            }
            if self.ui_imgui_metrics {
                ui.show_metrics_window(&mut self.ui_imgui_metrics);
            }
        }

        if self.ui_errors {
            self.last_errors = self.errors.len();
            let mut ui_errors = self.ui_errors;
            Window::new(im_str!("Errors"))
                .position([320.0, 140.0], Condition::FirstUseEver)
                .size([300.0, 400.0], Condition::FirstUseEver)
                .horizontal_scrollbar(true)
                .opened(&mut ui_errors)
                .build(ui, || {
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
                    return Some((tx as u32, ty as u32));
                }
            }
        }
        None
    }

    fn mouse_wheel(&mut self, ctrl: bool, shift: bool, alt: bool, x: f32, y: f32) {
        if alt {
            if y > 0.0 && self.map_renderer.zoom < 16.0 {
                self.map_renderer.zoom *= 2.0;
            } else if y < 0.0 && self.map_renderer.zoom > 0.0625 {
                self.map_renderer.zoom *= 0.5;
            }
            self.target_tile = self.tile_under(self.last_mouse_pos);
            return;
        }
        let mut mul = if ctrl { -1.0 } else { 1.0 };
        if shift {
            mul *= 8.0;
        }

        if let Some(map) = self.maps.get_mut(self.map_current) {
            let scroll_y = mul * y / self.map_renderer.zoom;
            let scroll_x = mul * x / self.map_renderer.zoom;
            if ctrl {
                map.center[0] += scroll_y;
                map.center[1] += scroll_x;
            } else {
                map.center[1] += scroll_y;
                map.center[0] += scroll_x;
            }
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
    // Actions - called from run_ui() and from chord()

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
        let path2 = path.clone();
        let rx = tasks::spawn(move || {
            use dm::constants::Constant;

            let context = dm::Context::default();
            let objtree = context.parse_environment(&path)?;

            let (mut turf, mut area) = ("/turf".to_owned(), "/area".to_owned());
            if let Some(world) = objtree.find("/world") {
                if let Some(turf_var) = world.get_value("turf") {
                    if let Some(Constant::Prefab(ref prefab)) = turf_var.constant {
                        turf = dm::ast::FormatTreePath(&prefab.path).to_string();
                    }
                }
                if let Some(area_var) = world.get_value("area") {
                    if let Some(Constant::Prefab(ref prefab)) = area_var.constant {
                        area = dm::ast::FormatTreePath(&prefab.path).to_string();
                    }
                }
            }

            Ok(Environment {
                objtree: Arc::new(objtree),
                icons: Arc::new(IconCache::new(path.parent().expect("invalid environment file path"))),
                turf,
                area,
                path,
            })
        });
        self.loading_env = Some(LoadingEnvironment { path: path2, rx });
    }

    fn new_map(&mut self) {
        self.new_map = Some(NewMap {
            x: 32,
            y: 32,
            z: 1,
            created: false,
        });
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
                let path = map.path.as_ref().unwrap();
                if let Err(e) = hist.current().save(map.state.base_dmm()).to_file(path) {
                    self.errors.push(format!("Error writing {}:\n{}", path.display(), e));
                }
                hist.mark_clean();
            }
        }
    }

    fn save_map_as(&mut self, copy: bool) {
        if let Some(map) = self.maps.get_mut(self.map_current) {
            if let Some(hist) = map.state.hist() {
                if let Ok(nfd::Response::Okay(fname)) = nfd::open_save_dialog(Some("dmm"), None) {
                    let path = PathBuf::from(fname);
                    if let Err(e) = hist.current().save(map.state.base_dmm()).to_file(&path) {
                        self.errors.push(format!("Error writing {}:\n{}", path.display(), e));
                    }
                    if !copy {
                        map.path = Some(path);
                        hist.mark_clean();
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

    fn tab_between_maps(&mut self, offset: isize) {
        if self.maps.is_empty() {
            return;
        }
        self.map_current = (self.map_current as isize + self.maps.len() as isize + offset) as usize % self.maps.len();
    }

    fn undo(&mut self) {
        if let Some(env) = self.environment.as_ref() {
            if let Some(map) = self.maps.get_mut(self.map_current) {
                if let Some(hist) = map.state.hist_mut() {
                    hist.undo(env);
                }
            }
        }
    }

    fn redo(&mut self) {
        if let Some(env) = self.environment.as_ref() {
            if let Some(map) = self.maps.get_mut(self.map_current) {
                if let Some(hist) = map.state.hist_mut() {
                    hist.redo(env);
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers

impl Environment {
    fn find_closest_type(&self, mut path: &str) -> (bool, Option<TypeRef>) {
        // find the "best" type by chopping the path if needed
        let mut ty = self.objtree.find(path);
        let red_paths = ty.is_none();
        while ty.is_none() && !path.is_empty() {
            match path.rfind("/") {
                Some(idx) => path = &path[..idx],
                None => break,
            }
            ty = self.objtree.find(path);
        }
        (red_paths, ty)
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

    fn hist(&self) -> Option<&History> {
        match self {
            MapState::Active { ref hist, .. } => Some(hist),
            _ => None,
        }
    }

    fn hist_mut(&mut self) -> Option<&mut History> {
        match self {
            MapState::Active { ref mut hist, .. } => Some(hist),
            _ => None,
        }
    }
}

impl EditPrefab {
    fn new(fab: Prefab) -> EditPrefab {
        EditPrefab {
            filter: ImString::with_capacity(128),
            fab,
        }
    }

    fn menu(&mut self, ui: &Ui) {
        ui.menu(im_str!("Filter..."), true, || {
            ui.input_text(im_str!(""), &mut self.filter).build();
            if MenuItem::new(im_str!("Clear")).build(ui) {
                self.filter.clear();
            }
        });
    }

    fn show(&mut self, ui: &Ui, env: Option<&Environment>, extra_vars: bool) {
        let EditPrefab {
            ref mut filter,
            ref mut fab,
        } = self;

        // find the "best" type by chopping the path if needed
        let (red_paths, ty) = if let Some(env) = env.as_ref() {
            env.find_closest_type(&fab.path)
        } else {
            (true, None)
        };

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
            {
                let style = ui.push_style_colors(GREEN_TEXT);
                ui.text(&im_str!("  {}", name));
                style.pop(ui);
            }
            ui.same_line(offset);
            ui.text(im_str!("{}", value));
        }

        // show the red path on error
        if red_paths {
            ui.separator();
            {
                let style = ui.push_style_colors(RED_TEXT);
                ui.text(im_str!("{}", &fab.path));
                style.pop(ui);
            }
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
    }
}

fn root_node(ui: &Ui, ty: TypeRef, name: &str) {
    if let Some(child) = ty.child(name) {
        tree_node(ui, child);
    }
}

fn tree_node(ui: &Ui, ty: TypeRef) {
    let mut children: Vec<_> = ty.children().collect();
    if children.is_empty() {
        ui.tree_node(&im_str!("{}", ty.name)).leaf(true).build(|| {});
    } else {
        children.sort_by_key(|t| &t.get().name);
        ui.tree_node(&im_str!("{}", ty.name)).build(|| {
            for child in children {
                tree_node(ui, child);
            }
        });
    }
}

fn file_name(path: &Path) -> Cow<str> {
    path.file_name().map_or("".into(), |s| s.to_string_lossy())
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

fn prepare_tool_icon(
    renderer: &mut ImRenderer,
    environment: Option<&Environment>,
    map_renderer: &mut map_renderer::MapRenderer,
    icon: tools::ToolIcon,
) -> tools::ToolIcon {
    use tools::ToolIcon;
    match icon {
        ToolIcon::Dmi {
            icon,
            icon_state,
            tint,
            dir,
        } => if let Some(env) = environment {
            if let Some(id) = env.icons.get_index(icon.as_ref()) {
                let icon = env.icons.get_icon(id);
                if let Some([u1, v1, u2, v2]) = icon.uv_of(&icon_state, dir) {
                    let tex = map_renderer
                        .icon_textures
                        .retrieve(&mut map_renderer.factory, &env.icons, id)
                        .clone();
                    let samp = map_renderer.sampler.clone();
                    ToolIcon::Loaded {
                        tex: renderer.textures().insert((tex, samp)),
                        uv0: [u1, v1],
                        uv1: [u2, v2],
                        tint: Some(tint),
                    }
                } else {
                    ToolIcon::None
                }
            } else {
                ToolIcon::None
            }
        } else {
            ToolIcon::Dmi {
                icon,
                icon_state,
                tint,
                dir,
            }
        },
        ToolIcon::EmbeddedPng { data } => if let Ok(tex) = dmi::texture_from_bytes(&mut map_renderer.factory, data) {
            let samp = map_renderer.sampler.clone();
            ToolIcon::Loaded {
                tex: renderer.textures().insert((tex, samp)),
                uv0: [0.0, 0.0],
                uv1: [1.0, 1.0],
                tint: None,
            }
        } else {
            ToolIcon::None
        },
        other => other,
    }
}

// ---------------------------------------------------------------------------
// Extension traits

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

trait Fulfill<T> {
    fn fulfill<F: FnOnce() -> T>(&mut self, f: F) -> &mut T;
}

impl<T> Fulfill<T> for Option<T> {
    fn fulfill<F: FnOnce() -> T>(&mut self, f: F) -> &mut T {
        if self.is_none() {
            *self = Some(f());
        }
        self.as_mut().unwrap()
    }
}

trait UiExt {
    fn fits_width(&self, width: f32) -> usize;
    fn objtree_menu<'e>(&self, env: &'e Environment, selection: &mut Option<TypeRef<'e>>);
    fn tool_icon(&self, active: bool, icon: &tools::ToolIcon, fallback: &ImStr) -> bool;
    fn frame_color(&self, active: bool) -> [f32; 4];
}

impl<'a> UiExt for Ui<'a> {
    fn fits_width(&self, element_width: f32) -> usize {
        let [width, _] = self.window_size();
        std::cmp::max(((width - 20.0) / (element_width + 8.0)) as usize, 1)
    }

    fn objtree_menu<'e>(&self, env: &'e Environment, selection: &mut Option<TypeRef<'e>>) {
        let root = env.objtree.root();
        objtree_menu_root(self, root, "area", selection);
        objtree_menu_root(self, root, "turf", selection);
        objtree_menu_root(self, root, "obj", selection);
        objtree_menu_root(self, root, "mob", selection);
    }

    fn tool_icon(&self, active: bool, icon: &tools::ToolIcon, fallback: &ImStr) -> bool {
        if let &tools::ToolIcon::Loaded { tex, uv0, uv1, tint } = icon {
            Image::new(tex, [32.0, 32.0])
                .uv0(uv0)
                .uv1(uv1)
                .border_col(self.frame_color(active))
                .tint_col(tint.unwrap_or(self.style_color(StyleColor::Text)))
                .build(self);
            self.is_item_hovered() && self.is_mouse_clicked(MouseButton::Left)
        } else {
            self.button(fallback, [34.0, 34.0])
        }
    }

    fn frame_color(&self, active: bool) -> [f32; 4] {
        if active {
            self.style_color(StyleColor::FrameBgActive)
        } else {
            self.style_color(StyleColor::FrameBg)
        }
    }
}

trait MenuItemExt {
    fn build_checkbox(self, ui: &Ui, selected: &mut bool);
}

impl<'a> MenuItemExt for MenuItem<'a> {
    fn build_checkbox(self, ui: &Ui, selected: &mut bool) {
        if self.selected(*selected).build(ui) {
            *selected = !*selected;
        }
    }
}

fn objtree_menu_root<'e>(ui: &Ui, ty: TypeRef<'e>, name: &str, selection: &mut Option<TypeRef<'e>>) {
    if let Some(child) = ty.child(name) {
        objtree_menu_node(ui, child, selection);
    }
}

fn objtree_menu_node<'e>(ui: &Ui, ty: TypeRef<'e>, selection: &mut Option<TypeRef<'e>>) {
    let mut children: Vec<_> = ty.children().collect();
    if children.is_empty() {
        if MenuItem::new(&im_str!("{}", ty.name)).build(ui) {
            *selection = Some(ty);
        }
    } else {
        children.sort_by_key(|t| &t.get().name);
        ui.menu(&im_str!("{}", ty.name), true, || {
            if MenuItem::new(&im_str!("{}", ty.name)).build(ui) {
                *selection = Some(ty);
            }
            ui.separator();
            for child in children {
                objtree_menu_node(ui, child, selection);
            }
        });
    }
}
