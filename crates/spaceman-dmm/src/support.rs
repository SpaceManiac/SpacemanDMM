//! Platform support helpers.
use imgui::{Context, FontConfig, MouseButton, MouseCursor};
use imgui_sdl3_renderer::Renderer;
use sdl3::{
    event::{Event, WindowEvent},
    hint::names::RENDER_VSYNC,
    keyboard::Scancode,
    mouse::{Cursor, SystemCursor},
    pixels::Color,
    video::WindowContext,
};
use std::time::Instant;

use crate::EditorScene;

pub type TextureCreator = sdl3::render::TextureCreator<WindowContext>;
pub type Canvas = sdl3::render::WindowCanvas;
pub type ImRenderer<'a> = Renderer<'a>;

#[derive(Copy, Clone, PartialEq, Debug, Default)]
struct MouseState {
    pos: (i32, i32),
    pressed: [bool; 5],
    wheel: f32,
}

pub fn run(title: &str, clear_color: [u8; 4]) -> EditorScene {
    sdl3::hint::set(RENDER_VSYNC, "1");

    let sdl = sdl3::init().unwrap();
    let video = sdl.video().unwrap();

    let window = video
        .window(title, 1300, 730)
        .position_centered()
        .resizable()
        .build()
        .unwrap();
    let mut canvas = window.into_canvas();
    let mut events = sdl.event_pump().unwrap();

    let mut imgui = imgui::Context::create();
    imgui.style_mut().use_dark_colors();
    imgui.set_ini_filename(None);

    // In the examples we only use integer DPI factors, because the UI can get very blurry
    // otherwise. This might or might not be what you want in a real application.
    // let mut window_hidpi_factor = 1.0;
    let hidpi_factor = 1.0;
    let logical_size = canvas.logical_size();
    let mut logical_size = (logical_size.0, logical_size.1);

    let font_size = (13.0 * hidpi_factor) as f32;

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

    let texture_creator = canvas.texture_creator();
    let mut im_renderer =
        Renderer::new(&texture_creator, &mut imgui).expect("Failed to initialize renderer");

    let mut scene = EditorScene::new(canvas.texture_creator());

    let mut last_frame = Instant::now();
    let mut mouse_state = MouseState::default();
    let mut quit = false;
    let mut mouse_captured = false;
    let mut kbd_captured = false;

    loop {
        for event in events.poll_iter() {
            match event {
                Event::Quit { .. } => quit = true,
                Event::Window {
                    win_event: WindowEvent::Resized(x, y),
                    ..
                } => {
                    logical_size = (x as u32, y as u32);
                },
                /*
                HiDpiFactorChanged(new_factor) => {
                    window_hidpi_factor = new_factor;
                    hidpi_factor = window_hidpi_factor.round();
                    logical_size = window
                        .window()
                        .get_inner_size()
                        .unwrap()
                        .to_physical(window_hidpi_factor)
                        .to_logical(hidpi_factor);
                },
                */
                Event::Window {
                    win_event: WindowEvent::FocusLost,
                    ..
                } => {
                    // If the window is unfocused, unset modifiers, or
                    // Alt-Tab will set it permanently & cause trouble. No,
                    // I don't know why this doesn't just work.
                    imgui.io_mut().key_ctrl = false;
                    imgui.io_mut().key_alt = false;
                    imgui.io_mut().key_shift = false;
                    imgui.io_mut().key_super = false;
                },
                Event::KeyDown {
                    scancode: Some(scancode),
                    ..
                } => {
                    if let Some(key) = map_scancode(scancode) {
                        imgui.io_mut().keys_down[key as usize] = true;
                        if !kbd_captured {
                            scene.chord(
                                ctrl(&imgui),
                                imgui.io().key_shift,
                                imgui.io().key_alt,
                                key,
                            );
                        }
                    }
                },
                Event::KeyUp {
                    scancode: Some(scancode),
                    ..
                } => {
                    if let Some(key) = map_scancode(scancode) {
                        imgui.io_mut().keys_down[key as usize] = false;
                    }
                },
                Event::MouseMotion { x, y, .. } => {
                    let pos = (x as i32, y as i32);
                    mouse_state.pos = pos;
                    scene.mouse_moved(pos);
                },
                Event::MouseButtonDown { mouse_btn, .. } => {
                    if let Some(b) = map_mousebtn(mouse_btn) {
                        mouse_state.pressed[b as usize] = true;
                    }
                },
                Event::MouseButtonUp { mouse_btn, .. } => {
                    if let Some(b) = map_mousebtn(mouse_btn) {
                        mouse_state.pressed[b as usize] = false;
                    }
                },
                Event::MouseWheel { x, y, .. } => {
                    mouse_state.wheel = y;
                    if !mouse_captured {
                        scene.mouse_wheel(
                            ctrl(&imgui),
                            imgui.io().key_shift,
                            imgui.io().key_alt,
                            4.0 * 32.0 * x,
                            4.0 * 32.0 * y,
                        );
                    }
                },
                Event::TextInput { text, .. } => {
                    for ch in text.chars() {
                        imgui.io_mut().add_input_character(ch);
                    }
                },
                _ => (),
            }
        }
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
        if logical_size.0 > 0 && logical_size.1 > 0 {
            imgui.io_mut().display_size = [logical_size.0 as f32, logical_size.1 as f32];
            imgui.io_mut().display_framebuffer_scale = [hidpi_factor as f32, hidpi_factor as f32];
            imgui.io_mut().font_global_scale = 1.0 / hidpi_factor as f32;
            imgui.io_mut().delta_time = delta_s;

            let ui = imgui.frame();
            if !scene.run_ui(&ui, &mut im_renderer) {
                break;
            }

            mouse_captured = ui.io().want_capture_mouse;
            kbd_captured = ui.io().want_capture_keyboard;

            if let Some(mouse_cursor) = ui.mouse_cursor() {
                // Set OS cursor
                sdl.mouse().show_cursor(true);
                Cursor::from_system(map_cursor(mouse_cursor)).unwrap().set();
            } else {
                // Hide OS cursor
                sdl.mouse().show_cursor(false);
            }

            canvas.set_draw_color(Color::RGBA(
                clear_color[0],
                clear_color[1],
                clear_color[2],
                clear_color[3],
            ));
            canvas.clear();
            scene.render(&mut canvas);
            canvas.present();
        }
    }
    scene
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

fn map_cursor(cursor: MouseCursor) -> SystemCursor {
    match cursor {
        MouseCursor::Arrow => SystemCursor::Arrow,
        MouseCursor::TextInput => SystemCursor::IBeam,
        MouseCursor::ResizeAll => SystemCursor::SizeAll,
        MouseCursor::ResizeNS => SystemCursor::SizeNS,
        MouseCursor::ResizeEW => SystemCursor::SizeWE,
        MouseCursor::ResizeNESW => SystemCursor::SizeNESW,
        MouseCursor::ResizeNWSE => SystemCursor::SizeNWSE,
        MouseCursor::Hand => SystemCursor::Hand,
        MouseCursor::NotAllowed => SystemCursor::No,
    }
}

fn map_mousebtn(btn: sdl3::mouse::MouseButton) -> Option<imgui::MouseButton> {
    match btn {
        sdl3::mouse::MouseButton::Left => Some(MouseButton::Left),
        sdl3::mouse::MouseButton::Right => Some(MouseButton::Right),
        sdl3::mouse::MouseButton::Middle => Some(MouseButton::Middle),
        sdl3::mouse::MouseButton::X1 => Some(MouseButton::Extra1),
        sdl3::mouse::MouseButton::X2 => Some(MouseButton::Extra2),
        _ => None,
    }
}

fn map_scancode(key: Scancode) -> Option<imgui::Key> {
    match key {
        Scancode::A => Some(imgui::Key::A),
        Scancode::B => Some(imgui::Key::B),
        Scancode::C => Some(imgui::Key::C),
        Scancode::D => Some(imgui::Key::D),
        Scancode::E => Some(imgui::Key::E),
        Scancode::F => Some(imgui::Key::F),
        Scancode::G => Some(imgui::Key::G),
        Scancode::H => Some(imgui::Key::H),
        Scancode::I => Some(imgui::Key::I),
        Scancode::J => Some(imgui::Key::J),
        Scancode::K => Some(imgui::Key::K),
        Scancode::L => Some(imgui::Key::L),
        Scancode::M => Some(imgui::Key::M),
        Scancode::N => Some(imgui::Key::N),
        Scancode::O => Some(imgui::Key::O),
        Scancode::P => Some(imgui::Key::P),
        Scancode::Q => Some(imgui::Key::Q),
        Scancode::R => Some(imgui::Key::R),
        Scancode::S => Some(imgui::Key::S),
        Scancode::T => Some(imgui::Key::T),
        Scancode::U => Some(imgui::Key::U),
        Scancode::V => Some(imgui::Key::V),
        Scancode::W => Some(imgui::Key::W),
        Scancode::X => Some(imgui::Key::X),
        Scancode::Y => Some(imgui::Key::Y),
        Scancode::Z => Some(imgui::Key::Z),
        Scancode::_1 => Some(imgui::Key::Keypad1),
        Scancode::_2 => Some(imgui::Key::Keypad2),
        Scancode::_3 => Some(imgui::Key::Keypad3),
        Scancode::_4 => Some(imgui::Key::Keypad4),
        Scancode::_5 => Some(imgui::Key::Keypad5),
        Scancode::_6 => Some(imgui::Key::Keypad6),
        Scancode::_7 => Some(imgui::Key::Keypad7),
        Scancode::_8 => Some(imgui::Key::Keypad8),
        Scancode::_9 => Some(imgui::Key::Keypad9),
        Scancode::_0 => Some(imgui::Key::Keypad0),
        Scancode::Return => Some(imgui::Key::Enter),
        Scancode::Escape => Some(imgui::Key::Escape),
        Scancode::Backspace => Some(imgui::Key::Backspace),
        Scancode::Tab => Some(imgui::Key::Tab),
        Scancode::Space => Some(imgui::Key::Space),
        Scancode::Minus => Some(imgui::Key::Minus),
        Scancode::Equals => Some(imgui::Key::Equal),
        Scancode::LeftBracket => Some(imgui::Key::LeftBracket),
        Scancode::RightBracket => Some(imgui::Key::RightBracket),
        Scancode::Backslash => Some(imgui::Key::Backslash),
        Scancode::Semicolon => Some(imgui::Key::Semicolon),
        Scancode::Apostrophe => Some(imgui::Key::Apostrophe),
        Scancode::Grave => Some(imgui::Key::GraveAccent),
        Scancode::Comma => Some(imgui::Key::Comma),
        Scancode::Period => Some(imgui::Key::Period),
        Scancode::Slash => Some(imgui::Key::Slash),
        Scancode::CapsLock => Some(imgui::Key::CapsLock),
        Scancode::F1 => Some(imgui::Key::F1),
        Scancode::F2 => Some(imgui::Key::F2),
        Scancode::F3 => Some(imgui::Key::F3),
        Scancode::F4 => Some(imgui::Key::F4),
        Scancode::F5 => Some(imgui::Key::F5),
        Scancode::F6 => Some(imgui::Key::F6),
        Scancode::F7 => Some(imgui::Key::F7),
        Scancode::F8 => Some(imgui::Key::F8),
        Scancode::F9 => Some(imgui::Key::F9),
        Scancode::F10 => Some(imgui::Key::F10),
        Scancode::F11 => Some(imgui::Key::F11),
        Scancode::F12 => Some(imgui::Key::F12),
        Scancode::PrintScreen => Some(imgui::Key::PrintScreen),
        Scancode::ScrollLock => Some(imgui::Key::ScrollLock),
        Scancode::Pause => Some(imgui::Key::Pause),
        Scancode::Insert => Some(imgui::Key::Insert),
        Scancode::Home => Some(imgui::Key::Home),
        Scancode::PageUp => Some(imgui::Key::PageUp),
        Scancode::Delete => Some(imgui::Key::Delete),
        Scancode::End => Some(imgui::Key::End),
        Scancode::PageDown => Some(imgui::Key::PageDown),
        Scancode::Right => Some(imgui::Key::RightArrow),
        Scancode::Left => Some(imgui::Key::LeftArrow),
        Scancode::Down => Some(imgui::Key::DownArrow),
        Scancode::Up => Some(imgui::Key::UpArrow),
        Scancode::KpDivide => Some(imgui::Key::KeypadDivide),
        Scancode::KpMultiply => Some(imgui::Key::KeypadMultiply),
        Scancode::KpMinus => Some(imgui::Key::KeypadSubtract),
        Scancode::KpPlus => Some(imgui::Key::KeypadAdd),
        Scancode::KpEnter => Some(imgui::Key::KeypadEnter),
        Scancode::Kp1 => Some(imgui::Key::Keypad1),
        Scancode::Kp2 => Some(imgui::Key::Keypad2),
        Scancode::Kp3 => Some(imgui::Key::Keypad3),
        Scancode::Kp4 => Some(imgui::Key::Keypad4),
        Scancode::Kp5 => Some(imgui::Key::Keypad5),
        Scancode::Kp6 => Some(imgui::Key::Keypad6),
        Scancode::Kp7 => Some(imgui::Key::Keypad7),
        Scancode::Kp8 => Some(imgui::Key::Keypad8),
        Scancode::Kp9 => Some(imgui::Key::Keypad9),
        Scancode::Kp0 => Some(imgui::Key::Keypad0),
        Scancode::KpPeriod => Some(imgui::Key::KeypadDecimal),
        Scancode::Application => Some(imgui::Key::Menu),
        Scancode::KpEquals => Some(imgui::Key::KeypadEqual),
        Scancode::Menu => Some(imgui::Key::Menu),
        Scancode::LCtrl => Some(imgui::Key::LeftCtrl),
        Scancode::LShift => Some(imgui::Key::LeftShift),
        Scancode::LAlt => Some(imgui::Key::LeftAlt),
        Scancode::LGui => Some(imgui::Key::LeftSuper),
        Scancode::RCtrl => Some(imgui::Key::RightCtrl),
        Scancode::RShift => Some(imgui::Key::RightShift),
        Scancode::RAlt => Some(imgui::Key::RightAlt),
        Scancode::RGui => Some(imgui::Key::RightSuper),
        _ => None,
    }
}
