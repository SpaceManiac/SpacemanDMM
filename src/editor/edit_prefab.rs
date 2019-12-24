use imgui::*;
use dmm_tools::dmm::Prefab;
use crate::{Environment, GREEN_TEXT, RED_TEXT};

pub struct EditPrefab {
    filter: ImString,
    fab: Prefab,
}

impl EditPrefab {
    pub fn new(fab: Prefab) -> EditPrefab {
        EditPrefab {
            filter: ImString::with_capacity(128),
            fab,
        }
    }

    pub fn path(&self) -> &str {
        &self.fab.path
    }

    pub fn fab(&self) -> &Prefab {
        &self.fab
    }

    pub fn menu(&mut self, ui: &Ui) {
        ui.menu(im_str!("Filter..."), true, || {
            ui.input_text(im_str!(""), &mut self.filter).build();
            if MenuItem::new(im_str!("Clear")).build(ui) {
                self.filter.clear();
            }
        });
    }

    pub fn show(&mut self, ui: &Ui, env: Option<&Environment>, extra_vars: bool) {
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
