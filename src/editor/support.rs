use imgui::{FrameSize, ImFontConfig, ImGui, ImGuiMouseCursor, ImVec4};
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
    use gfx::{self, Device};
    use gfx_window_glutin;
    use glutin::{self, GlContext};

    let mut events_loop = glutin::EventsLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let window = glutin::WindowBuilder::new()
        .with_title(title)
        .with_window_icon(glutin::Icon::from_rgba(include_bytes!("res/gasmask.raw").to_vec(), 16, 16).ok())
        .with_min_dimensions(glutin::dpi::LogicalSize::new(640.0, 480.0))
        .with_dimensions(glutin::dpi::LogicalSize::new(1300.0, 730.0));
    let (window, mut device, mut factory, mut main_color, mut main_depth) =
        gfx_window_glutin::init::<ColorFormat, DepthFormat>(window, context, &events_loop);

    // gfx's gl backend sets FRAMEBUFFER_SRGB permanently by default - actually
    // we want it off permanently. I've been told this is most sanely set on a
    // per-render-pass basis, but neither the world nor imgui blend colors
    // accurately if it is set.
    unsafe {
        device.with_gl(|gl| {
            gl.Disable(::gl::FRAMEBUFFER_SRGB);
        });
    }

    let (ww, wh): (f64, f64) = window.get_outer_size().unwrap().into();
    let (dw, dh): (f64, f64) = window.get_primary_monitor().get_dimensions().into();
    window.set_position(((dw - ww) / 2.0, (dh - wh) / 2.0).into());

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

    let mut imgui = ImGui::init();
    imgui.style_mut().colors.clone_from(&dark_theme());
    imgui.set_ini_filename(None);

    // In the examples we only use integer DPI factors, because the UI can get very blurry
    // otherwise. This might or might not be what you want in a real application.
    let mut window_hidpi_factor = window.get_hidpi_factor();
    let mut hidpi_factor = window_hidpi_factor.round();

    let mut frame_size = FrameSize {
        logical_size: window
            .get_inner_size()
            .unwrap()
            .to_physical(window_hidpi_factor)
            .to_logical(hidpi_factor)
            .into(),
        hidpi_factor,
    };

    let font_size = (13.0 * hidpi_factor) as f32;

    imgui.fonts().add_default_font_with_config(
        ImFontConfig::new()
            .oversample_h(1)
            .pixel_snap_h(true)
            .size_pixels(font_size),
    );

    imgui.set_font_global_scale((1.0 / hidpi_factor) as f32);

    let mut renderer = Renderer::init(&mut imgui, &mut factory, shaders, main_color.clone())
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
                        renderer.update_render_target(main_color.clone());
                        scene.update_render_target(&main_color, &main_depth);
                        frame_size.logical_size = new_logical_size
                            .to_physical(window_hidpi_factor)
                            .to_logical(hidpi_factor)
                            .into();
                    },
                    HiDpiFactorChanged(new_factor) => {
                        window_hidpi_factor = new_factor;
                        hidpi_factor = window_hidpi_factor.round();
                        frame_size.hidpi_factor = hidpi_factor;
                        frame_size.logical_size = window
                            .get_inner_size()
                            .unwrap()
                            .to_physical(window_hidpi_factor)
                            .to_logical(hidpi_factor)
                            .into();
                    },
                    Focused(false) => {
                        // If the window is unfocused, unset modifiers, or
                        // Alt-Tab will set it permanently & cause trouble. No,
                        // I don't know why this doesn't just work.
                        imgui.set_key_ctrl(false);
                        imgui.set_key_alt(false);
                        imgui.set_key_shift(false);
                        imgui.set_key_super(false);
                    },
                    KeyboardInput { input, .. } => {
                        use glutin::VirtualKeyCode as Key;

                        let pressed = input.state == Pressed;
                        match input.virtual_keycode {
                            Some(Key::Tab) => imgui.set_key(0, pressed),
                            Some(Key::Left) => imgui.set_key(1, pressed),
                            Some(Key::Right) => imgui.set_key(2, pressed),
                            Some(Key::Up) => imgui.set_key(3, pressed),
                            Some(Key::Down) => imgui.set_key(4, pressed),
                            Some(Key::PageUp) => imgui.set_key(5, pressed),
                            Some(Key::PageDown) => imgui.set_key(6, pressed),
                            Some(Key::Home) => imgui.set_key(7, pressed),
                            Some(Key::End) => imgui.set_key(8, pressed),
                            Some(Key::Delete) => imgui.set_key(9, pressed),
                            Some(Key::Back) => imgui.set_key(10, pressed),
                            Some(Key::Return) => imgui.set_key(11, pressed),
                            Some(Key::Escape) => imgui.set_key(12, pressed),
                            Some(Key::A) => imgui.set_key(13, pressed),
                            Some(Key::C) => imgui.set_key(14, pressed),
                            Some(Key::V) => imgui.set_key(15, pressed),
                            Some(Key::X) => imgui.set_key(16, pressed),
                            Some(Key::Y) => imgui.set_key(17, pressed),
                            Some(Key::Z) => imgui.set_key(18, pressed),
                            Some(Key::LControl) | Some(Key::RControl) => imgui.set_key_ctrl(pressed),
                            Some(Key::LShift) | Some(Key::RShift) => imgui.set_key_shift(pressed),
                            Some(Key::LAlt) | Some(Key::RAlt) => imgui.set_key_alt(pressed),
                            Some(Key::LWin) | Some(Key::RWin) => imgui.set_key_super(pressed),
                            _ => {}
                        }

                        if pressed && !kbd_captured {
                            if let Some(key) = input.virtual_keycode {
                                scene.chord(ctrl(&imgui), imgui.key_shift(), imgui.key_alt(), key);
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
                            scene.mouse_wheel(ctrl(&imgui), imgui.key_shift(), imgui.key_alt(), x, y);
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
                            scene.mouse_wheel(
                                ctrl(&imgui),
                                imgui.key_shift(),
                                imgui.key_alt(),
                                diff.x as f32,
                                diff.y as f32,
                            );
                        }
                    },
                    ReceivedCharacter(c) => imgui.add_input_character(c),
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

        let mouse_cursor = imgui.mouse_cursor();
        if imgui.mouse_draw_cursor() || mouse_cursor == ImGuiMouseCursor::None {
            // Hide OS cursor
            window.hide_cursor(true);
        } else {
            // Set OS cursor
            window.hide_cursor(false);
            window.set_cursor(match mouse_cursor {
                ImGuiMouseCursor::None => unreachable!("mouse_cursor was None!"),
                ImGuiMouseCursor::Arrow => glutin::MouseCursor::Arrow,
                ImGuiMouseCursor::TextInput => glutin::MouseCursor::Text,
                ImGuiMouseCursor::Move => glutin::MouseCursor::Move,
                ImGuiMouseCursor::ResizeNS => glutin::MouseCursor::NsResize,
                ImGuiMouseCursor::ResizeEW => glutin::MouseCursor::EwResize,
                ImGuiMouseCursor::ResizeNESW => glutin::MouseCursor::NeswResize,
                ImGuiMouseCursor::ResizeNWSE => glutin::MouseCursor::NwseResize,
            });
        }

        // Workaround: imgui-gfx-renderer will not call ui.render() under this
        // condition, which occurs when minimized, and imgui will assert
        // because of missing either a Render() or EndFrame() call.
        let logical_size = frame_size.logical_size;
        if logical_size.0 > 0.0 && logical_size.1 > 0.0 {
            let ui = imgui.frame(frame_size, delta_s);
            if !scene.run_ui(&ui, &mut renderer) {
                break;
            }

            mouse_captured = ui.want_capture_mouse();
            kbd_captured = ui.want_capture_keyboard();

            encoder.clear(&main_color, clear_color);
            scene.render(&mut encoder);
            renderer
                .render(ui, &mut factory, &mut encoder)
                .expect("Rendering failed");
            encoder.flush(&mut device);
            window.context().swap_buffers().unwrap();
            device.cleanup();
        }
    }
    scene
}

fn configure_keys(imgui: &mut ImGui) {
    use imgui::ImGuiKey;

    imgui.set_imgui_key(ImGuiKey::Tab, 0);
    imgui.set_imgui_key(ImGuiKey::LeftArrow, 1);
    imgui.set_imgui_key(ImGuiKey::RightArrow, 2);
    imgui.set_imgui_key(ImGuiKey::UpArrow, 3);
    imgui.set_imgui_key(ImGuiKey::DownArrow, 4);
    imgui.set_imgui_key(ImGuiKey::PageUp, 5);
    imgui.set_imgui_key(ImGuiKey::PageDown, 6);
    imgui.set_imgui_key(ImGuiKey::Home, 7);
    imgui.set_imgui_key(ImGuiKey::End, 8);
    imgui.set_imgui_key(ImGuiKey::Delete, 9);
    imgui.set_imgui_key(ImGuiKey::Backspace, 10);
    imgui.set_imgui_key(ImGuiKey::Enter, 11);
    imgui.set_imgui_key(ImGuiKey::Escape, 12);
    imgui.set_imgui_key(ImGuiKey::A, 13);
    imgui.set_imgui_key(ImGuiKey::C, 14);
    imgui.set_imgui_key(ImGuiKey::V, 15);
    imgui.set_imgui_key(ImGuiKey::X, 16);
    imgui.set_imgui_key(ImGuiKey::Y, 17);
    imgui.set_imgui_key(ImGuiKey::Z, 18);
}

fn update_mouse(imgui: &mut ImGui, mouse_state: &mut MouseState) {
    imgui.set_mouse_pos(mouse_state.pos.0 as f32, mouse_state.pos.1 as f32);
    imgui.set_mouse_down(mouse_state.pressed);
    imgui.set_mouse_wheel(mouse_state.wheel);
    mouse_state.wheel = 0.0;
}

fn dark_theme() -> [ImVec4; 43] {
    [
        ImVec4::new(1.00, 1.00, 1.00, 1.00),
        ImVec4::new(0.50, 0.50, 0.50, 1.00),
        ImVec4::new(0.06, 0.06, 0.06, 0.94),
        ImVec4::new(1.00, 1.00, 1.00, 0.00),
        ImVec4::new(0.08, 0.08, 0.08, 0.94),
        ImVec4::new(0.43, 0.43, 0.50, 0.50),
        ImVec4::new(0.00, 0.00, 0.00, 0.00),
        ImVec4::new(0.16, 0.29, 0.48, 0.54),
        ImVec4::new(0.26, 0.59, 0.98, 0.40),
        ImVec4::new(0.26, 0.59, 0.98, 0.67),
        ImVec4::new(0.04, 0.04, 0.04, 1.00),
        ImVec4::new(0.16, 0.29, 0.48, 1.00),
        ImVec4::new(0.00, 0.00, 0.00, 0.51),
        ImVec4::new(0.14, 0.14, 0.14, 1.00),
        ImVec4::new(0.02, 0.02, 0.02, 0.53),
        ImVec4::new(0.31, 0.31, 0.31, 1.00),
        ImVec4::new(0.41, 0.41, 0.41, 1.00),
        ImVec4::new(0.51, 0.51, 0.51, 1.00),
        ImVec4::new(0.26, 0.59, 0.98, 1.00),
        ImVec4::new(0.24, 0.52, 0.88, 1.00),
        ImVec4::new(0.26, 0.59, 0.98, 1.00),
        ImVec4::new(0.26, 0.59, 0.98, 0.40),
        ImVec4::new(0.26, 0.59, 0.98, 1.00),
        ImVec4::new(0.06, 0.53, 0.98, 1.00),
        ImVec4::new(0.26, 0.59, 0.98, 0.31),
        ImVec4::new(0.26, 0.59, 0.98, 0.80),
        ImVec4::new(0.26, 0.59, 0.98, 1.00),
        ImVec4::new(0.43, 0.43, 0.50, 0.50),
        ImVec4::new(0.10, 0.40, 0.75, 0.78),
        ImVec4::new(0.10, 0.40, 0.75, 1.00),
        ImVec4::new(0.26, 0.59, 0.98, 0.25),
        ImVec4::new(0.26, 0.59, 0.98, 0.67),
        ImVec4::new(0.26, 0.59, 0.98, 0.95),
        ImVec4::new(0.41, 0.41, 0.41, 0.50),
        ImVec4::new(0.98, 0.39, 0.36, 1.00),
        ImVec4::new(0.98, 0.39, 0.36, 1.00),
        ImVec4::new(0.61, 0.61, 0.61, 1.00),
        ImVec4::new(1.00, 0.43, 0.35, 1.00),
        ImVec4::new(0.90, 0.70, 0.00, 1.00),
        ImVec4::new(1.00, 0.60, 0.00, 1.00),
        ImVec4::new(0.26, 0.59, 0.98, 0.35),
        ImVec4::new(0.80, 0.80, 0.80, 0.35),
        ImVec4::new(1.00, 1.00, 0.00, 0.90),
    ]
}

#[cfg(not(target_os = "macos"))]
fn ctrl(imgui: &ImGui) -> bool {
    imgui.key_ctrl()
}

#[cfg(target_os = "macos")]
fn ctrl(imgui: &ImGui) -> bool {
    imgui.key_super()
}
