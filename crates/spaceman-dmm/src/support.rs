//! Platform support helpers.
use imgui::{Context, FontConfig};
use imgui_sdl3::{platform::Platform, renderer::Renderer};
use sdl3::{
    event::{Event, WindowEvent},
    gpu::{ColorTargetInfo, Device, ShaderFormat},
    hint::names::RENDER_VSYNC,
};

pub fn run(title: &str) {
    sdl3::hint::set(RENDER_VSYNC, "1");

    let mut sdl = sdl3::init().expect("sdl3::init");
    let mut events = sdl.event_pump().expect("event_pump");
    let video = sdl.video().expect("video");

    let window = video
        .window(title, 1300, 730)
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

    let mut im_platform = Platform::new(&mut imgui);
    let mut im_renderer = Renderer::new(&device, &window, &mut imgui).expect("Renderer::new");

    let mut scene = crate::EditorScene::new(&device, window.size());

    let mut quit = false;

    loop {
        let mut command_buffer = device
            .acquire_command_buffer()
            .expect("acquire_command_buffer");
        // wait_and_acquire_swapchain_texture is the vsync point - event handling and per-frame processing
        // should occur AFTER it, to get lowest-latency UI reactions.
        let swapchain = command_buffer.wait_and_acquire_swapchain_texture(&window);

        for event in events.poll_iter() {
            // Pass event to imgui, then use want_capture_X to know whether to
            // ignore the event on our end.
            _ = im_platform.handle_event(&mut imgui, &event);
            if imgui.io().want_capture_mouse {
                match event {
                    Event::MouseButtonDown { .. } | Event::MouseWheel { .. } => continue,
                    _ => (),
                }
            }
            if imgui.io().want_capture_keyboard {
                match event {
                    Event::KeyDown { .. }
                    | Event::JoyButtonDown { .. }
                    | Event::ControllerButtonDown { .. } => continue,
                    _ => (),
                }
            }

            // Really handle it.
            match event {
                Event::Quit { .. } => quit = true,
                Event::Window {
                    win_event: WindowEvent::Resized(x, y),
                    ..
                } => {
                    scene.logical_size = (x as u32, y as u32);
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
                Event::KeyDown {
                    scancode: Some(scancode),
                    ..
                } => {
                    scene.chord(
                        ctrl(&imgui),
                        imgui.io().key_shift,
                        imgui.io().key_alt,
                        scancode,
                    );
                },
                Event::MouseMotion { x, y, .. } => {
                    let pos = (x as i32, y as i32);
                    scene.mouse_moved(pos);
                },
                Event::MouseWheel { x, y, .. } => {
                    scene.mouse_wheel(
                        ctrl(&imgui),
                        imgui.io().key_shift,
                        imgui.io().key_alt,
                        4.0 * 32.0 * x,
                        4.0 * 32.0 * y,
                    );
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

        // Start the ImGui frame.
        im_platform.prepare_frame(&mut sdl, &mut imgui, &window, &events);
        let ui = imgui.new_frame();

        // Process scene.
        scene.run();
        if !scene.run_ui(&ui, &mut im_renderer) {
            break;
        }

        if let Ok(swapchain) = swapchain {
            // Must finish using swapchain by stuffing it into ColorTargetInfo now,
            // to free up its mutable borrow of command_buffer.
            let target = ColorTargetInfo::default().with_texture(&swapchain);
            let targets = [ColorTargetInfo::default().with_texture(&swapchain)];

            // Render the main scene.
            scene.render(&mut command_buffer, target);

            // Render ImGui's passes.
            im_renderer
                .render(&device, &mut command_buffer, &targets, &mut imgui)
                .expect("imgui render");

            // Flip it.
            command_buffer.submit();
        } else {
            // Can be triggered by window being minimized on some platforms.
            command_buffer.cancel();
            // TODO: Does this break vsync such that we need to do our own scheduling?
        }
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
