use super::*;
use {UiExt, EditPrefab, RetainMut};

/// The standard placement tool.
#[derive(Default)]
pub struct Place {
    fabs: Vec<PlaceFab>,
    fab_current: usize,
}

struct PlaceFab {
    fab: Prefab,
    edit: Option<EditPrefab>,
}

impl ToolBehavior for Place {
    fn settings(&mut self, ui: &Ui) {
        ui.text(im_str!("current: {} / {}", self.fab_current, self.fabs.len()));
        ui.same_line(0.0);
        if ui.small_button(im_str!("Add")) {
            self.fabs.push(PlaceFab {
                fab: Prefab {
                    path: "/obj/item/lighter".to_owned(),
                    vars: Default::default(),
                },
                edit: None,
            });
        }

        let mut i = 0;
        let fab_current = &mut self.fab_current;

        let count = ui.fits_width(32.0);
        self.fabs.retain_mut(|fab| {
            if i % count != 0 {
                ui.same_line(0.0);
            }

            let mut keep = true;
            //ui.small_button(im_str!("{}. {}", i, fab.path));
            ui.button(im_str!(""), (32.0, 32.0));
            if ui.is_item_hovered() {
                ui.tooltip_text(im_str!("{}", fab.fab));
                if ui.imgui().is_mouse_clicked(ImMouseButton::Left) {
                    *fab_current = i;
                } else if ui.imgui().is_mouse_clicked(ImMouseButton::Right) {
                    if fab.edit.is_none() {
                        fab.edit = Some(EditPrefab::new(fab.fab.clone()));
                    }
                }
            }

            let mut keep_editor = true;
            if let Some(ref mut edit) = fab.edit {
                ui.window(im_str!("{}##place/{}", edit.fab.path, i))
                    .opened(&mut keep_editor)
                    .position(ui.imgui().mouse_pos(), ImGuiCond::Appearing)
                    .size((350.0, 500.0), ImGuiCond::FirstUseEver)
                    .horizontal_scrollbar(true)
                    .menu_bar(true)
                    .build(|| {
                        ui.menu_bar(|| {
                            if ui.menu_item(im_str!("Remove")).build() {
                                keep = false;
                            }
                            ui.separator();
                            edit.menu(ui);
                        });
                        //edit.show(ui, env, false);
                    });
            }
            if !keep_editor {
                fab.edit = None;
            }

            // wrapping things up
            if !keep && *fab_current > i {
                *fab_current -= 1;
            }
            i += 1;
            keep
        });
    }

    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
        if let Some(fab) = self.fabs.get(self.fab_current) {
            let fab = fab.fab.clone();
            hist.edit(env, "TODO".to_owned(), move |env, world| {
                let pop = world.add_pop(&fab, &env.icons, &env.objtree);
                let inst = world.add_instance(loc, pop);
                Box::new(move |_, world| {
                    world.remove_instance(inst.clone());
                })
            });
        }
    }
}

