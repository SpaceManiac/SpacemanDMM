//! The map editor proper, with a GUI and everything.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![allow(dead_code)] // TODO: remove when this is not a huge WIP
#![allow(unused_variables)]

mod config;
mod dmi;
mod edit_prefab;
mod editor;
mod history;
mod map_renderer;
mod map_repr;
mod tasks;
mod tools;

use std::sync::Mutex;

use imgui::{ConfigFlags, Context, FontConfig};
use imgui_sdl3::{platform::Platform, renderer::Renderer};
use sdl3::{
    event::{Event, WindowEvent},
    gpu::{ColorTargetInfo, CommandBuffer, Device, ShaderFormat, Texture},
    hint::names::RENDER_VSYNC,
    video::Window,
    EventPump, Sdl,
};
use sdl3_main::{AppResult, MainThreadData, MainThreadToken};

use crate::editor::EditorScene;

#[ouroboros::self_referencing]
struct RenderTarget {
    command_buffer: CommandBuffer,
    #[borrows(mut command_buffer)]
    #[not_covariant]
    swapchain: Result<Texture<'this>, sdl3::Error>,
}

impl RenderTarget {
    fn acquire(device: &Device, window: &Window) -> RenderTarget {
        let command_buffer = device
            .acquire_command_buffer()
            .expect("acquire_command_buffer");
        RenderTarget::new(command_buffer, |command_buffer| {
            // wait_and_acquire_swapchain_texture is the vsync point - event handling and per-frame processing
            // should occur AFTER it, to get lowest-latency UI reactions.
            command_buffer.wait_and_acquire_swapchain_texture(&window)
        })
    }
}

struct Main {
    sdl: Sdl,
    events: EventPump,
    window: Window,
    device: Device,

    imgui: imgui::Context,
    im_platform: imgui_sdl3::platform::Platform,
    im_renderer: imgui_sdl3::renderer::Renderer,

    scene: EditorScene,

    target: Option<RenderTarget>,
}

impl Main {
    fn app_init() -> Self {
        sdl3::hint::set(RENDER_VSYNC, "1");

        let sdl = sdl3::init().expect("sdl3::init");
        let events = sdl.event_pump().expect("event_pump");
        let video = sdl.video().expect("video");

        let window = video
            .window("SpacemanDMM", 1300, 730)
            .position_centered()
            .resizable()
            .build()
            .expect("Window::build");

        // In the examples we only use integer DPI factors, because the UI can get very blurry
        // otherwise. This might or might not be what you want in a real application.
        // let mut window_hidpi_factor = 1.0;
        let hidpi_factor = 1.0;
        // let logical_size = window.size();
        // let mut logical_size = (logical_size.0, logical_size.1);

        let font_size = (13.0 * hidpi_factor) as f32;

        let device = Device::new(ShaderFormat::SPIRV, cfg!(debug_assertions))
            .expect("Device::new")
            .with_window(&window)
            .expect("with_window");

        let mut imgui = imgui::Context::create();
        imgui.style_mut().use_dark_colors();
        imgui.set_ini_filename(None);
        imgui.set_log_filename(None);
        imgui.io_mut().config_flags |= ConfigFlags::DOCKING_ENABLE;
        imgui
            .fonts()
            .add_font(&[imgui::FontSource::DefaultFontData {
                config: Some(FontConfig {
                    oversample_h: 1,
                    pixel_snap_h: true,
                    size_pixels: font_size,
                    ..Default::default()
                }),
            }]);

        let im_platform = Platform::new(&mut imgui);
        let im_renderer = Renderer::new(&device, &window, &mut imgui).expect("Renderer::new");

        let scene = editor::EditorScene::new(&device, window.size());

        let target = Some(RenderTarget::acquire(&device, &window));

        Main {
            sdl,
            events,
            window,
            device,
            imgui,
            im_platform,
            im_renderer,
            scene,
            target,
        }
    }

    fn app_event(&mut self, event: Event) -> AppResult {
        // Pass event to imgui, then use want_capture_X to know whether to
        // ignore the event on our end.
        _ = self.im_platform.handle_event(&mut self.imgui, &event);
        if self.imgui.io().want_capture_mouse {
            match event {
                Event::MouseButtonDown { .. } | Event::MouseWheel { .. } => {
                    return AppResult::Continue
                },
                _ => (),
            }
        }
        if self.imgui.io().want_capture_keyboard {
            match event {
                Event::KeyDown { .. }
                | Event::JoyButtonDown { .. }
                | Event::ControllerButtonDown { .. } => return AppResult::Continue,
                _ => (),
            }
        }

        // Really handle it.
        match event {
            Event::Quit { .. } => return AppResult::Success,
            Event::Window {
                win_event: WindowEvent::Resized(x, y),
                ..
            } => {
                self.scene.logical_size = (x as u32, y as u32);
            },
            Event::KeyDown {
                scancode: Some(scancode),
                ..
            } => {
                self.scene.chord(
                    ctrl(&self.imgui),
                    self.imgui.io().key_shift,
                    self.imgui.io().key_alt,
                    scancode,
                );
            },
            Event::MouseMotion { x, y, .. } => {
                let pos = (x as i32, y as i32);
                self.scene.mouse_moved(pos);
            },
            Event::MouseWheel { x, y, .. } => {
                self.scene.mouse_wheel(
                    ctrl(&self.imgui),
                    self.imgui.io().key_shift,
                    self.imgui.io().key_alt,
                    4.0 * 32.0 * x,
                    4.0 * 32.0 * y,
                );
            },
            _ => (),
        }

        AppResult::Continue
    }

    fn app_iterate(&mut self) -> AppResult {
        // Start the ImGui frame.
        self.im_platform
            .prepare_frame(&mut self.sdl, &mut self.imgui, &self.window, &self.events);
        let ui = self.imgui.new_frame();

        // Process scene.
        self.scene.run();
        if !self.scene.run_ui(ui, &mut self.im_renderer) {
            return AppResult::Success;
        }

        let colors = self.target.as_ref().unwrap().with_swapchain(|swapchain| {
            if let Ok(swapchain) = swapchain {
                // Must finish using swapchain by stuffing it into ColorTargetInfo now,
                // to free up its mutable borrow of command_buffer.
                let target = ColorTargetInfo::default().with_texture(&swapchain);
                let target2 = ColorTargetInfo::default().with_texture(&swapchain);

                // TODO: This might be evil
                Some((target, target2))
            } else {
                // Can be triggered by window being minimized on some platforms.
                None
            }
        });

        let mut command_buffer = self.target.take().unwrap().into_heads().command_buffer;
        if let Some((target, target2)) = colors {
            // Render the main scene.
            self.scene.render(&mut command_buffer, target);

            // Render ImGui's passes.
            self.im_renderer
                .render(
                    &self.device,
                    &mut command_buffer,
                    &[target2],
                    &mut self.imgui,
                )
                .expect("imgui render");

            // Flip it.
            command_buffer.submit().expect("CommandBuffer::submit");
        } else {
            command_buffer.cancel();
            // TODO: Does this break vsync such that we need to do our own scheduling?
        };

        // Vsync between rendering and event handling for lowest latency.
        self.target = Some(RenderTarget::acquire(&self.device, &self.window));

        AppResult::Continue
    }
}

struct Main2(MainThreadData<Main>);

#[sdl3_main::app_impl]
impl Main2 {
    fn app_init() -> Option<Box<Mutex<Self>>> {
        Some(Box::new(Mutex::new(Main2(MainThreadData::new(
            MainThreadToken::assert(),
            Main::app_init(),
        )))))
    }

    fn app_event(&mut self, event: sdl3::sys::events::SDL_Event) -> AppResult {
        self.0
            .get_mut(MainThreadToken::assert())
            .app_event(Event::from_ll(event))
    }

    fn app_iterate(&mut self) -> AppResult {
        self.0.get_mut(MainThreadToken::assert()).app_iterate()
    }
}

#[cfg(not(target_os = "macos"))]
fn ctrl(imgui: &Context) -> bool {
    imgui.io().key_ctrl
}

#[cfg(target_os = "macos")]
fn ctrl(imgui: &Context) -> bool {
    imgui.io().key_super
}
