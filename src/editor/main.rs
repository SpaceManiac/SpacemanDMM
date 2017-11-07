//! The main crate for the map editor.

#[macro_use] extern crate qt_extras as qt;
#[macro_use] extern crate glium;

mod map_renderer;

use qt::widgets;
use qt::widgets::application::Application;
use qt::core::connection::Signal;

#[allow(unused_mut)]
fn main() {
    Application::create_and_exit(|_app| unsafe {
        // object tree
        let mut tree_view = widgets::tree_view::TreeView::new();

        // minimap
        let mut minimap_widget = qt::glium_widget::create(map_renderer::GliumTest);
        minimap_widget.set_minimum_size((256, 256));
        minimap_widget.set_maximum_size((256, 256));

        // tools
        let mut tools = widgets::label::Label::new(qstr!("Tools Go Here"));

        // instances
        let mut list_view = widgets::list_view::ListView::new();

        // vertical layout
        let mut v_layout = widgets::v_box_layout::VBoxLayout::new();
        v_layout.add_widget(qt_own!(minimap_widget));
        v_layout.add_widget(qt_own!(tools));
        v_layout.add_widget(qt_own!(list_view));

        // map
        let mut map_widget = qt::glium_widget::create(map_renderer::GliumTest);

        // horizontal layout
        let mut h_layout = widgets::h_box_layout::HBoxLayout::new();
        h_layout.add_layout(qt_own!(v_layout));
        h_layout.add_widget((qt_own!(map_widget), 1));
        h_layout.set_contents_margins((0, 0, 0, 0));
        let mut h_layout_widget = widgets::widget::Widget::new();
        h_layout_widget.set_layout(qt_own!(h_layout));

        // root splitter
        let mut splitter = widgets::splitter::Splitter::new(());
        splitter.set_children_collapsible(false);
        splitter.add_widget(qt_own!(tree_view));
        splitter.add_widget(qt_own!(h_layout_widget));
        splitter.set_stretch_factor(0, 0);
        splitter.set_stretch_factor(1, 1);

        // menus
        let mut menu_bar = widgets::menu_bar::MenuBar::new();
        let mut menu_file = &mut *menu_bar.add_menu(qstr!("File"));
        menu_file.add_action(qstr!("Open Environment"));
        menu_file.add_action(qstr!("Recent Environments"));
        menu_file.add_separator();
        menu_file.add_action(qstr!("New"));
        menu_file.add_action(qstr!("Open"));
        menu_file.add_action(qstr!("Close"));
        menu_file.add_separator();

        // status bar
        let mut status_bar = widgets::status_bar::StatusBar::new();

        // build main window
        let mut window = widgets::main_window::MainWindow::new();
        window.set_window_title(qstr!("SpacemanDMM"));
        window.resize((1400, 768));

        window.set_menu_bar(qt_own!(menu_bar));
        window.set_status_bar(qt_own!(status_bar));
        window.set_central_widget(qt_own!(splitter));

        // connect signals
        (*menu_file.add_action(qstr!("Exit"))).signals().triggered().connect(&window.slots().close());

        // cede control
        window.show();
        Application::exec()
    })
}
