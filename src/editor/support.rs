use imgui::{FontConfig, Context, MouseCursor};
use imgui_gfx_renderer::{Renderer, Shaders};
use std::time::Instant;

use {ColorFormat, DepthFormat};

#[derive(Copy, Clone, PartialEq, Debug, Default)]
struct MouseState {
    pos: (i32, i32),
    pressed: [bool; 5],
    wheel: f32,
}

pub fn run(title: String, clear_color: [f32; 4]) -> ::EditorScene {
    use gfx::Device;

    let mut events_loop = glutin::EventsLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let window = glutin::WindowBuilder::new()
        .with_title(title)
        .with_window_icon(glutin::Icon::from_rgba(include_bytes!("res/gasmask.raw").to_vec(), 16, 16).ok())
        .with_min_dimensions(glutin::dpi::LogicalSize::new(640.0, 480.0))
        .with_dimensions(glutin::dpi::LogicalSize::new(1300.0, 730.0));
    let (window, mut device, mut factory, mut main_color, mut main_depth) =
        gfx_window_glutin::init::<ColorFormat, DepthFormat>(window, context, &events_loop)
        .expect("failed to initialize glutin window");

    // gfx's gl backend sets FRAMEBUFFER_SRGB permanently by default - actually
    // we want it off permanently. I've been told this is most sanely set on a
    // per-render-pass basis, but neither the world nor imgui blend colors
    // accurately if it is set.
    unsafe {
        device.with_gl(|gl| {
            gl.Disable(::gl::FRAMEBUFFER_SRGB);
        });
    }

    let (ww, wh): (f64, f64) = window.window().get_outer_size().unwrap().into();
    let (dw, dh): (f64, f64) = window.window().get_primary_monitor().get_dimensions().into();
    window.window().set_position(((dw - ww) / 2.0, (dh - wh) / 2.0).into());

    let mut encoder: gfx::Encoder<_, _> = factory.create_command_buffer().into();
    let shaders = {
        let version = device.get_info().shading_language;
        if version.is_embedded {
            if version.major >= 3 {
                Shaders::GlSlEs300
            } else {
                Shaders::GlSlEs100
            }
        } else if version.major >= 4 {
            Shaders::GlSl400
        } else if version.major >= 3 {
            Shaders::GlSl130
        } else {
            Shaders::GlSl110
        }
    };

    let mut imgui = imgui::Context::create();
    imgui.style_mut().use_dark_colors();
    imgui.set_ini_filename(None);

    // In the examples we only use integer DPI factors, because the UI can get very blurry
    // otherwise. This might or might not be what you want in a real application.
    let mut window_hidpi_factor = window.window().get_hidpi_factor();
    let mut hidpi_factor = window_hidpi_factor.round();
    let mut logical_size = window.window()
        .get_inner_size()
        .unwrap()
        .to_physical(window_hidpi_factor)
        .to_logical(hidpi_factor);

    let font_size = (13.0 * hidpi_factor) as f32;

    imgui.fonts().add_font(&[
        imgui::FontSource::DefaultFontData {
            config: Some(FontConfig {
                oversample_h: 1,
                pixel_snap_h: true,
                size_pixels: font_size,
                .. Default::default()
            })
        }
    ]);

    let mut renderer = Renderer::init(&mut imgui, &mut factory, shaders)
        .expect("Failed to initialize renderer");

    configure_keys(&mut imgui);

    let mut scene = ::EditorScene::new(&mut factory, &main_color, &main_depth);

    let mut last_frame = Instant::now();
    let mut mouse_state = MouseState::default();
    let mut quit = false;
    let mut mouse_captured = false;
    let mut kbd_captured = false;

    loop {
        events_loop.poll_events(|event| {
            use glutin::ElementState::Pressed;
            use glutin::WindowEvent::*;
            use glutin::{Event, MouseButton, MouseScrollDelta, TouchPhase};

            if let Event::WindowEvent { event, .. } = event {
                match event {
                    CloseRequested => quit = true,
                    Resized(new_logical_size) => {
                        gfx_window_glutin::update_views(&window, &mut main_color, &mut main_depth);
                        window.resize(new_logical_size.to_physical(hidpi_factor));
                        scene.update_render_target(&main_color, &main_depth);
                        logical_size = new_logical_size
                            .to_physical(window_hidpi_factor)
                            .to_logical(hidpi_factor);
                    },
                    HiDpiFactorChanged(new_factor) => {
                        window_hidpi_factor = new_factor;
                        hidpi_factor = window_hidpi_factor.round();
                        logical_size = window.window()
                            .get_inner_size()
                            .unwrap()
                            .to_physical(window_hidpi_factor)
                            .to_logical(hidpi_factor);
                    },
                    Focused(false) => {
                        // If the window is unfocused, unset modifiers, or
                        // Alt-Tab will set it permanently & cause trouble. No,
                        // I don't know why this doesn't just work.
                        imgui.io_mut().key_ctrl = false;
                        imgui.io_mut().key_alt = false;
                        imgui.io_mut().key_shift = false;
                        imgui.io_mut().key_super = false;
                    },
                    KeyboardInput { input, .. } => {
                        use glutin::VirtualKeyCode as Key;

                        let pressed = input.state == Pressed;
                        match input.virtual_keycode {
                            Some(Key::Tab) => imgui.io_mut().keys_down[0] = pressed,
                            Some(Key::Left) => imgui.io_mut().keys_down[1] = pressed,
                            Some(Key::Right) => imgui.io_mut().keys_down[2] = pressed,
                            Some(Key::Up) => imgui.io_mut().keys_down[3] = pressed,
                            Some(Key::Down) => imgui.io_mut().keys_down[4] = pressed,
                            Some(Key::PageUp) => imgui.io_mut().keys_down[5] = pressed,
                            Some(Key::PageDown) => imgui.io_mut().keys_down[6] = pressed,
                            Some(Key::Home) => imgui.io_mut().keys_down[7] = pressed,
                            Some(Key::End) => imgui.io_mut().keys_down[8] = pressed,
                            Some(Key::Delete) => imgui.io_mut().keys_down[9] = pressed,
                            Some(Key::Back) => imgui.io_mut().keys_down[10] = pressed,
                            Some(Key::Return) => imgui.io_mut().keys_down[11] = pressed,
                            Some(Key::Escape) => imgui.io_mut().keys_down[12] = pressed,
                            Some(Key::A) => imgui.io_mut().keys_down[13] = pressed,
                            Some(Key::C) => imgui.io_mut().keys_down[14] = pressed,
                            Some(Key::V) => imgui.io_mut().keys_down[15] = pressed,
                            Some(Key::X) => imgui.io_mut().keys_down[16] = pressed,
                            Some(Key::Y) => imgui.io_mut().keys_down[17] = pressed,
                            Some(Key::Z) => imgui.io_mut().keys_down[18] = pressed,
                            Some(Key::LControl) | Some(Key::RControl) => imgui.io_mut().key_ctrl = pressed,
                            Some(Key::LShift) | Some(Key::RShift) => imgui.io_mut().key_shift = pressed,
                            Some(Key::LAlt) | Some(Key::RAlt) => imgui.io_mut().key_alt = pressed,
                            Some(Key::LWin) | Some(Key::RWin) => imgui.io_mut().key_super = pressed,
                            _ => {}
                        }

                        if pressed && !kbd_captured {
                            if let Some(key) = input.virtual_keycode {
                                scene.chord(ctrl(&imgui), imgui.io().key_shift, imgui.io().key_alt, key);
                            }
                        }
                    },
                    CursorMoved { position, .. } => {
                        // Rescale position from glutin logical coordinates to our logical
                        // coordinates
                        let pos = position
                            .to_physical(window_hidpi_factor)
                            .to_logical(hidpi_factor)
                            .into();
                        mouse_state.pos = pos;
                        scene.mouse_moved(pos);
                    },
                    MouseInput { state, button, .. } => match button {
                        MouseButton::Left => mouse_state.pressed[0] = state == Pressed,
                        MouseButton::Right => mouse_state.pressed[1] = state == Pressed,
                        MouseButton::Middle => mouse_state.pressed[2] = state == Pressed,
                        MouseButton::Other(i) => if let Some(b) = mouse_state.pressed.get_mut(2 + i as usize) {
                            *b = state == Pressed;
                        },
                    },
                    MouseWheel {
                        delta: MouseScrollDelta::LineDelta(x, y),
                        phase: TouchPhase::Moved,
                        ..
                    } => {
                        mouse_state.wheel = y;
                        if !mouse_captured {
                            scene.mouse_wheel(ctrl(&imgui), imgui.io().key_shift, imgui.io().key_alt, 4.0 * 32.0 * x, 4.0 * 32.0 * y);
                        }
                    },
                    MouseWheel {
                        delta: MouseScrollDelta::PixelDelta(pos),
                        phase: TouchPhase::Moved,
                        ..
                    } => {
                        // Rescale pixel delta from glutin logical coordinates to our logical
                        // coordinates
                        let diff = pos
                            .to_physical(window_hidpi_factor)
                            .to_logical(hidpi_factor);
                        mouse_state.wheel = diff.y as f32;
                        if !mouse_captured {
                            #[cfg(not(target_os = "macos"))]
                            let diff_x = diff.x as f32;
                            #[cfg(target_os = "macos")]
                            let diff_x = -diff.x as f32;
                            scene.mouse_wheel(
                                ctrl(&imgui),
                                imgui.io().key_shift,
                                imgui.io().key_alt,
                                diff_x,
                                diff.y as f32,
                            );
                        }
                    },
                    ReceivedCharacter(c) => imgui.io_mut().add_input_character(c),
                    _ => (),
                }
            }
        });
        if quit {
            break;
        }

        let now = Instant::now();
        let delta = now - last_frame;
        let delta_s = delta.as_secs() as f32 + delta.subsec_nanos() as f32 / 1_000_000_000.0;
        last_frame = now;

        update_mouse(&mut imgui, &mut mouse_state);

        // Workaround: imgui-gfx-renderer will not call ui.render() under this
        // condition, which occurs when minimized, and imgui will assert
        // because of missing either a Render() or EndFrame() call.
        if logical_size.width > 0.0 && logical_size.height > 0.0 {
            imgui.io_mut().display_size = [logical_size.width as f32, logical_size.height as f32];
            imgui.io_mut().display_framebuffer_scale = [hidpi_factor as f32, hidpi_factor as f32];
            imgui.io_mut().delta_time = delta_s;

            let ui = imgui.frame();
            ui.set_window_font_scale(1.0 / hidpi_factor as f32);
            if !scene.run_ui(&ui, &mut renderer) {
                break;
            }

            mouse_captured = ui.io().want_capture_mouse;
            kbd_captured = ui.io().want_capture_keyboard;

            if let Some(mouse_cursor) = ui.mouse_cursor() {
                // Set OS cursor
                window.window().hide_cursor(false);
                window.window().set_cursor(match mouse_cursor {
                    MouseCursor::Arrow => glutin::MouseCursor::Arrow,
                    MouseCursor::TextInput => glutin::MouseCursor::Text,
                    MouseCursor::ResizeAll => glutin::MouseCursor::Move,
                    MouseCursor::ResizeNS => glutin::MouseCursor::NsResize,
                    MouseCursor::ResizeEW => glutin::MouseCursor::EwResize,
                    MouseCursor::ResizeNESW => glutin::MouseCursor::NeswResize,
                    MouseCursor::ResizeNWSE => glutin::MouseCursor::NwseResize,
                    MouseCursor::Hand => glutin::MouseCursor::Hand,
                });
            } else {
                // Hide OS cursor
                window.window().hide_cursor(true);
            }

            encoder.clear(&main_color, clear_color);
            scene.render(&mut encoder);
            renderer
                .render(&mut factory, &mut encoder, &mut main_color, ui.render())
                .expect("Rendering failed");
            encoder.flush(&mut device);
            window.swap_buffers().unwrap();
            device.cleanup();
        }
    }
    scene
}

fn configure_keys(imgui: &mut Context) {
    use imgui::Key::*;

    imgui.io_mut()[Tab] = 0;
    imgui.io_mut()[LeftArrow] = 1;
    imgui.io_mut()[RightArrow] = 2;
    imgui.io_mut()[UpArrow] = 3;
    imgui.io_mut()[DownArrow] = 4;
    imgui.io_mut()[PageUp] = 5;
    imgui.io_mut()[PageDown] = 6;
    imgui.io_mut()[Home] = 7;
    imgui.io_mut()[End] = 8;
    imgui.io_mut()[Delete] = 9;
    imgui.io_mut()[Backspace] = 10;
    imgui.io_mut()[Enter] = 11;
    imgui.io_mut()[Escape] = 12;
    imgui.io_mut()[A] = 13;
    imgui.io_mut()[C] = 14;
    imgui.io_mut()[V] = 15;
    imgui.io_mut()[X] = 16;
    imgui.io_mut()[Y] = 17;
    imgui.io_mut()[Z] = 18;
}


fn update_mouse(imgui: &mut Context, mouse_state: &mut MouseState) {
    imgui.io_mut().mouse_pos = [mouse_state.pos.0 as f32, mouse_state.pos.1 as f32];
    imgui.io_mut().mouse_down = mouse_state.pressed;
    imgui.io_mut().mouse_wheel = mouse_state.wheel;
    mouse_state.wheel = 0.0;
}

#[cfg(not(target_os = "macos"))]
fn ctrl(imgui: &Context) -> bool {
    imgui.io().key_ctrl
}

#[cfg(target_os = "macos")]
fn ctrl(imgui: &Context) -> bool {
    imgui.io().key_super
}
