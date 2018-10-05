use super::*;
use {UiExt, EditPrefab, RetainMut};

/// The standard placement tool.
#[derive(Default)]
pub struct Place {
    palette: Vec<PaletteEntry>,
    pal_current: usize,
}

struct PaletteEntry {
    fab: Prefab,
    edit: Option<EditPrefab>,
}

impl PaletteEntry {
    fn new(fab: Prefab) -> PaletteEntry {
        PaletteEntry { fab, edit: None }
    }
}

impl ToolBehavior for Place {
    fn settings(&mut self, ui: &Ui, env: &Environment) {
        let mut i = 0;
        let Place { palette, pal_current } = self;

        let count = ui.fits_width(32.0);
        palette.retain_mut(|fab| {
            if i % count != 0 {
                ui.same_line(0.0);
            }

            let mut keep = true;
            //ui.small_button(im_str!("{}. {}", i, fab.path));
            ui.button(im_str!(""), (32.0, 32.0));
            if ui.is_item_hovered() {
                ui.tooltip_text(im_str!("{:#}", fab.fab));
                if ui.imgui().is_mouse_clicked(ImMouseButton::Left) {
                    *pal_current = i;
                } else if ui.imgui().is_mouse_clicked(ImMouseButton::Right) {
                    if fab.edit.is_none() {
                        fab.edit = Some(EditPrefab::new(fab.fab.clone()));
                    }
                }
            }

            let mut keep_editor = true;
            if let Some(ref mut edit) = fab.edit {
                let fab_fab = &mut fab.fab;
                ui.window(im_str!("Palette: {}##place/{}", edit.fab.path, i))
                    .opened(&mut keep_editor)
                    .position(ui.imgui().mouse_pos(), ImGuiCond::Appearing)
                    .size((350.0, 500.0), ImGuiCond::FirstUseEver)
                    .horizontal_scrollbar(true)
                    .menu_bar(true)
                    .build(|| {
                        ui.menu_bar(|| {
                            if ui.menu_item(im_str!("Apply")).build() {
                                fab_fab.clone_from(&edit.fab);
                            }
                            ui.separator();
                            if ui.menu_item(im_str!("Remove")).build() {
                                keep = false;
                            }
                            ui.separator();
                            edit.menu(ui);
                        });
                        edit.show(ui, Some(env), false);
                    });
            }
            if !keep_editor {
                fab.edit = None;
            }

            // wrapping things up
            if !keep && *pal_current > i {
                *pal_current -= 1;
            }
            i += 1;
            keep
        });

        if i % count != 0 {
            ui.same_line(0.0);
        }
        if ui.button(im_str!("+"), (32.0, 32.0)) {
            ui.open_popup(im_str!("place_tool_add"));
        }
        if ui.is_item_hovered() {
            ui.tooltip_text(im_str!("Add"));
        }

        ui.popup(im_str!("place_tool_add"), || {
            let mut selection = None;
            ui.menu_item(im_str!("Recent")).enabled(false).build();
            ui.separator();
            ui.objtree_menu(env, &mut selection);
            if let Some(sel) = selection {
                let new_fab = Prefab {
                    path: sel.path.to_owned(),
                    vars: Default::default(),
                };
                *pal_current = palette.len();
                palette.push(PaletteEntry {
                    fab: new_fab.clone(),
                    edit: if ui.imgui().key_shift() {
                        Some(EditPrefab::new(new_fab))
                    } else {
                        None
                    },
                });
            }
        });
    }

    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
        if let Some(fab) = self.palette.get(self.pal_current) {
            let fab = fab.fab.clone();
            hist.edit(env, "TODO".to_owned(), move |env, world| {
                let pop = world.add_pop(&fab, &env.icons, &env.objtree);
                let added = world.add_instance(loc, pop);
                Box::new(move |_, world| {
                    world.undo_add_instance(&added);
                })
            });
        }
    }

    fn pick(&mut self, prefab: &Prefab) {
        for (i, fab) in self.palette.iter().enumerate() {
            if fab.fab == *prefab {
                self.pal_current = i;
                return;
            }
        }
        self.pal_current = self.palette.len();
        self.palette.push(PaletteEntry::new(prefab.clone()));
    }
}

