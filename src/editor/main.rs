#[macro_use] extern crate conrod;

use std::time::{Instant, Duration};

use conrod::backend::glium::glium::{self, glutin, Surface};

pub fn main() {
    let mut events_loop = glutin::EventsLoop::new();
    let window = glutin::WindowBuilder::new()
        .with_title("SpacemanDMM")
        .with_dimensions(1400, 768);
    let context = glutin::ContextBuilder::new()
        .with_vsync(true)
        .with_multisampling(4);
    let display = glium::Display::new(window, context, &events_loop).unwrap();

    let mut ui = conrod::UiBuilder::new([1400., 768.])
        .theme(conrod::Theme {
            background_color: conrod::Color::Rgba(240./255., 240./255., 240./255., 1.),
            border_color: conrod::Color::Rgba(160./255., 160./255., 160./255., 1.),
            border_width: 1.,
            .. conrod::Theme::default()
        })
        .build();

    let ids = Ids::new(ui.widget_id_generator());

    let mut renderer = conrod::backend::glium::Renderer::new(&display).unwrap();

    let image_map = conrod::image::Map::<glium::texture::Texture2d>::new();

    ui.fonts.insert_from_file("resources/fonts/cmunss.ttf").unwrap();

    let mut event_loop = EventLoop::new();
    'main: loop {
        for event in event_loop.next(&mut events_loop) {
            if let Some(event) = conrod::backend::winit::convert_event(event.clone(), &display) {
                ui.handle_event(event);
                event_loop.needs_update();
            }

            match event {
                glium::glutin::Event::WindowEvent { event, .. } => match event {
                    glium::glutin::WindowEvent::Closed |
                    glium::glutin::WindowEvent::KeyboardInput {
                        input: glium::glutin::KeyboardInput {
                            virtual_keycode: Some(glium::glutin::VirtualKeyCode::Escape),
                            ..
                        },
                        ..
                    } => break 'main,
                    _ => {}
                },
                _ => {}
            }
        }

        set_ui(ui.set_widgets(), &ids);

        if let Some(primitives) = ui.draw_if_changed() {
            renderer.fill(&display, primitives, &image_map);
            let mut target = display.draw();
            target.clear_color(0.0, 0.0, 0.0, 1.0);
            renderer.draw(&display, &mut target, &image_map).unwrap();
            target.finish().unwrap();
        }
    }
}

widget_ids! {
    struct Ids {
        canvas_root,

        canvas_menu,
        canvas_central,
        canvas_status,

        canvas_objtree,
        canvas_middle,
        canvas_map,

        canvas_minimap,
        canvas_instances,

        text_status,
    }
}

fn set_ui(ref mut ui: conrod::UiCell, ids: &Ids) {
    use conrod::*;
    use conrod::widget::*;

    let middle_canvas_items = [
        (ids.canvas_minimap, Canvas::new().length(256.)),
        (ids.canvas_instances, Canvas::new().length_weight(1.)),
    ];

    let main_canvas_items = [
        (ids.canvas_objtree, Canvas::new().length(256.)),
        (ids.canvas_middle, Canvas::new().length(256.).flow_down(&middle_canvas_items)),
        (ids.canvas_map, Canvas::new().length_weight(1.)),
    ];
    let main_canvas = Canvas::new()
        .length_weight(1.)
        .flow_right(&main_canvas_items);

    Canvas::new()
        .flow_down(&[
            (ids.canvas_menu, Canvas::new().length(25.)),
            (ids.canvas_central, main_canvas),
            (ids.canvas_status, Canvas::new().length(25.)),
        ])
        .border(0.)
        .pad(0.)
        .set(ids.canvas_root, ui);

    Text::new("SpacemanDMM")
        .top_left_with_margin_on(ids.canvas_status, 1.)
        .set(ids.text_status, ui);

    /*FileNavigator::all("../tgstation".as_ref())
        .middle_of(ids.canvas)
        .color(color::LIGHT_BLUE)
        .font_size(8)
        .set(ids.files, ui);*/
}

/// In most of the examples the `glutin` crate is used for providing the window context and
/// events while the `glium` crate is used for displaying `conrod::render::Primitives` to the
/// screen.
///
/// This `Iterator`-like type simplifies some of the boilerplate involved in setting up a
/// glutin+glium event loop that works efficiently with conrod.
pub struct EventLoop {
    ui_needs_update: bool,
    last_update: Instant,
}

impl EventLoop {
    pub fn new() -> Self {
        EventLoop {
            last_update: Instant::now(),
            ui_needs_update: true,
        }
    }

    /// Produce an iterator yielding all available events.
    pub fn next(&mut self, events_loop: &mut glium::glutin::EventsLoop) -> Vec<glium::glutin::Event> {
        // We don't want to loop any faster than 60 FPS, so wait until it has been at least 16ms
        // since the last yield.
        let last_update = self.last_update;
        let sixteen_ms = Duration::from_millis(16);
        let duration_since_last_update = Instant::now().duration_since(last_update);
        if duration_since_last_update < sixteen_ms {
            std::thread::sleep(sixteen_ms - duration_since_last_update);
        }

        // Collect all pending events.
        let mut events = Vec::new();
        events_loop.poll_events(|event| events.push(event));

        // If there are no events and the `Ui` does not need updating, wait for the next event.
        if events.is_empty() && !self.ui_needs_update {
            events_loop.run_forever(|event| {
                events.push(event);
                glium::glutin::ControlFlow::Break
            });
        }

        self.ui_needs_update = false;
        self.last_update = Instant::now();

        events
    }

    /// Notifies the event loop that the `Ui` requires another update whether or not there are any
    /// pending events.
    ///
    /// This is primarily used on the occasion that some part of the `Ui` is still animating and
    /// requires further updates to do so.
    pub fn needs_update(&mut self) {
        self.ui_needs_update = true;
    }

}
