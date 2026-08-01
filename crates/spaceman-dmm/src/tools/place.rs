use super::*;
use crate::edit_prefab::EditPrefab;
use crate::editor::UiExt;
use std::collections::HashSet;

/// The standard placement tool.
#[derive(Default)]
pub struct Place {
    palette: Vec<PaletteEntry>,
    pal_current: usize,
    placing_at: HashSet<(u32, u32, u32)>,
}

struct PaletteEntry {
    fab: Prefab,
    icon: ToolIcon,
    edit: Option<EditPrefab>,
}

impl PaletteEntry {
    fn new(env: &Environment, fab: Prefab) -> PaletteEntry {
        PaletteEntry {
            icon: ToolIcon::from_atom(env, &fab).unwrap_or(ToolIcon::None),
            edit: None,
            fab,
        }
    }
}

impl ToolBehavior for Place {
    fn settings(&mut self, ui: &Ui, env: &Environment, ctx: &mut IconCtx) {
        let mut i = 0;
        let Place {
            palette,
            pal_current,
            ..
        } = self;

        let add_popup_id = "place_tool_add";

        let count = ui.fits_width(34.0);
        palette.retain_mut(|pal| {
            if i % count != 0 {
                ui.same_line();
            }

            let mut keep = true;
            ui.tool_icon(
                i == *pal_current,
                pal.icon.prepare(Some(env), ctx),
                &pal.fab.path,
                ctx.renderer,
            );
            if ui.is_item_hovered() {
                ui.tooltip_text(format!("{:#}", pal.fab));
                if ui.is_mouse_clicked(MouseButton::Left) {
                    *pal_current = i;
                } else if ui.is_mouse_clicked(MouseButton::Right) && pal.edit.is_none() {
                    pal.edit = Some(EditPrefab::new(pal.fab.clone()));
                }
            }

            let mut keep_editor = true;
            if let Some(ref mut edit) = pal.edit {
                let fab = &mut pal.fab;
                ui.window(&format!("Palette: {}##place/{}", edit.path(), i))
                    .opened(&mut keep_editor)
                    .position(ui.io().mouse_pos, Condition::Appearing)
                    .size([0.0, 550.0], Condition::Appearing)
                    .horizontal_scrollbar(true)
                    .menu_bar(true)
                    .build(|| {
                        ui.menu_bar(|| {
                            if ui.menu_item("Apply") {
                                fab.clone_from(edit.fab());
                            }
                            ui.separator();
                            if ui.menu_item("Remove") {
                                keep = false;
                            }
                            ui.separator();
                            edit.menu(ui);
                        });
                        edit.show(ui, Some(env), false);
                    });
            }
            if !keep_editor {
                pal.edit = None;
            }

            // wrapping things up
            if !keep && *pal_current > i {
                *pal_current -= 1;
            }
            i += 1;
            keep
        });

        if i % count != 0 {
            ui.same_line();
        }
        if ui.button_with_size("+", [34.0, 34.0]) {
            ui.open_popup(add_popup_id);
        }
        if ui.is_item_hovered() {
            ui.tooltip_text("Add");
        }

        ui.popup(add_popup_id, || {
            let mut selection = None;
            ui.objtree_menu(env, &mut selection);
            if let Some(sel) = selection {
                let fab = Prefab::from_path(sel.path.to_owned());
                let mut entry = PaletteEntry::new(env, fab);
                if ui.io().key_shift {
                    entry.edit = Some(EditPrefab::new(entry.fab.clone()));
                }

                *pal_current = palette.len();
                palette.push(entry);
            }
        });
    }

    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
        self.placing_at.clear();
        self.drag(hist, env, loc);
    }

    fn drag(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
        // Imitate DM's "paint" behavior.
        if self.placing_at.contains(&loc) {
            return;
        }
        self.placing_at.insert(loc);

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

    fn pick(&mut self, env: &Environment, prefab: &Prefab) {
        for (i, fab) in self.palette.iter().enumerate() {
            if fab.fab == *prefab {
                self.pal_current = i;
                return;
            }
        }
        self.pal_current = self.palette.len();
        self.palette.push(PaletteEntry::new(env, prefab.clone()));
    }
}
