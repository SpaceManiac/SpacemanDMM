//! The main crate for the map editor.

#[macro_use] extern crate qt_extras as qt;
#[macro_use] extern crate glium;

extern crate dreammaker as dm;
extern crate dmm_tools;

mod map_renderer;

use std::cell::RefCell;
use std::path::PathBuf;

use dm::objtree::{ObjectTree, TypeRef};

use qt::widgets;
use qt::widgets::application::Application;
use qt::widgets::file_dialog::FileDialog;
use qt::widgets::tree_widget::TreeWidget;
use qt::widgets::tree_widget_item::TreeWidgetItem;
use qt::core::connection::Signal;
use qt::core::slots::SlotNoArgs;
use qt::cpp_utils::static_cast_mut;
use qt::gui::key_sequence::KeySequence;

unsafe fn show_error(message: &str) {
    qt::widgets::message_box::MessageBox::critical((
        0 as *mut qt::widgets::widget::Widget,
        qstr!("Error"),
        qstr!(message),
        qt::core::flags::Flags::from_enum(qt::widgets::message_box::StandardButton::Ok),
    ));
}

struct State {
    environment_file: Option<PathBuf>,
    objtree: Option<dm::objtree::ObjectTree>,
}

impl State {
    fn new() -> State {
        State {
            environment_file: None,
            objtree: None,
        }
    }

    unsafe fn load_env(&mut self, path: String, widget: &mut TreeWidget) {
        let path = PathBuf::from(path);

        let mut preprocessor;
        match dm::preprocessor::Preprocessor::new(path.clone()) {
            Err(_) => return show_error(&format!("Could not open for reading:\n{}", path.display())),
            Ok(pp) => preprocessor = pp,
        };

        let objtree;
        match dm::parser::parse(dm::indents::IndentProcessor::new(&mut preprocessor)) {
            Err(e) => {
                let mut message = format!("\
                    Could not parse the environment:\n\
                    {}\n\n\
                    This may be caused by incorrect or unusual code, but is typically a parser bug. \
                    Change the code to use a more common form, or report the parsing problem.\n\
                ", path.display());
                let mut message_buf = Vec::new();
                let _ = dm::pretty_print_error(&mut message_buf, &preprocessor, &e);
                message.push_str(&String::from_utf8_lossy(&message_buf[..]));
                return show_error(&message);
            },
            Ok(t) => objtree = t,
        }

        self.environment_file = Some(path);
        {
            widget.clear();
            let root = objtree.root();
            for &root_child in ["area", "turf", "obj", "mob"].iter() {
                let ty = root.child(root_child, &objtree).expect("builtins missing");

                let mut root_item = TreeWidgetItem::new(());
                root_item.set_text(0, qstr!(&ty.name));
                add_children(&mut root_item, ty, &objtree);
                widget.add_top_level_item(qt_own!(root_item));
            }
        }
        self.objtree = Some(objtree);
    }
}

unsafe fn add_children(parent: &mut TreeWidgetItem, ty: TypeRef, tree: &ObjectTree) {
    let mut children = ty.children(tree);
    children.sort_unstable_by(|a, b| a.name.cmp(&b.name));
    for each in children {
        let mut child = TreeWidgetItem::new(());
        child.set_text(0, qstr!(&each.name));
        add_children(&mut child, each, tree);
        parent.add_child(qt_own!(child));
    }
}

macro_rules! action {
    (@[$it:ident] (tip = $text:expr)) => {
        $it.set_status_tip(&qstr!($text));
    };
    (@[$it:ident] (key = $(^$m:ident)* $k:ident)) => {
        $it.set_shortcut(&KeySequence::new( qt::core::qt::Key::$k as i32 $(+ qt::core::qt::Modifier::$m as i32)* ));
    };
    (@[$it:ident] (slot = $slot:expr)) => {
        $it.signals().triggered().connect(&$slot);
    };
    (@[$it:ident] $closure:block) => {
        let slot = SlotNoArgs::new(|| $closure);
        $it.signals().triggered().connect(&slot);
    };
    ($add_to:expr, $name:expr $(, $x:tt)*) => {
        let it = &mut *$add_to.add_action(qstr!($name));
        $(action!(@[it] $x);)*
    }
}

#[allow(unused_mut)]
fn main() {
    let mut state = RefCell::new(State::new());

    Application::create_and_exit(|_app| unsafe {
        let mut window = widgets::main_window::MainWindow::new();
        let window_ptr = window.as_mut_ptr();

        // object tree
        let mut tree_widget = widgets::tree_widget::TreeWidget::new();
        let tree_widget_ptr = tree_widget.as_mut_ptr();
        tree_widget.set_column_count(1);
        tree_widget.set_header_hidden(true);

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
        splitter.add_widget(qt_own!(tree_widget));
        splitter.add_widget(qt_own!(h_layout_widget));
        splitter.set_stretch_factor(0, 0);
        splitter.set_stretch_factor(1, 1);

        // menus
        let mut menu_bar = widgets::menu_bar::MenuBar::new();
        let mut menu_file = &mut *menu_bar.add_menu(qstr!("File"));
        action!(menu_file, "Open Environment", (tip = "Load a DME file."), {
            let file = FileDialog::get_open_file_name_unsafe((
                static_cast_mut(window_ptr),
                qstr!("Open File"),
                qstr!("."),
                qstr!("Environments (*.dme)"),
            )).to_std_string();
            if !file.is_empty() {
                state.borrow_mut().load_env(file, &mut *tree_widget_ptr);
            }
        });
        menu_file.add_menu(qstr!("Recent Environments"));
        menu_file.add_separator();
        action!(menu_file, "New", (key = ^CTRL KeyN), (tip = "Create a new map."));
        action!(menu_file, "Open", (key = ^CTRL KeyO), (tip = "Open a map."));
        action!(menu_file, "Close", (key = ^CTRL KeyW), (tip = "Close the current map."));
        menu_file.add_separator();
        action!(menu_file, "Exit", (key = ^ALT KeyF4), (slot = window.slots().close()));

        // status bar
        let mut status_bar = widgets::status_bar::StatusBar::new();

        // build main window
        window.set_window_title(qstr!("SpacemanDMM"));
        window.resize((1400, 768));

        window.set_menu_bar(qt_own!(menu_bar));
        window.set_status_bar(qt_own!(status_bar));
        window.set_central_widget(qt_own!(splitter));

        // cede control
        window.show();
        Application::exec()
    })
}
