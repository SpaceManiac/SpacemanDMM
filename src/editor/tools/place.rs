use super::*;
use UiExt;

/// The standard placement tool.
#[derive(Default)]
pub struct Place {
    fabs: Vec<Prefab>,
    fab_current: usize,
}

impl ToolBehavior for Place {
    fn settings(&mut self, ui: &Ui) {
        ui.text(im_str!("current: {} / {}", self.fab_current, self.fabs.len()));
        ui.same_line(0.0);
        if ui.small_button(im_str!("Add")) {
            self.fabs.push(Prefab {
                path: "/obj/item/lighter".to_owned(),
                vars: Default::default(),
            });
        }

        let mut i = 0;
        let fab_current = &mut self.fab_current;

        let count = ui.fits_width(32.0);
        self.fabs.retain(|fab| {
            if i % count != 0 {
                ui.same_line(0.0);
            }

            let mut keep = true;
            //ui.small_button(im_str!("{}. {}", i, fab.path));
            ui.button(im_str!(""), (32.0, 32.0));
            if ui.is_item_hovered() {
                ui.tooltip_text(im_str!("{}", fab));
                if ui.imgui().is_mouse_clicked(ImMouseButton::Left) {
                    *fab_current = i;
                } else if ui.imgui().is_mouse_clicked(ImMouseButton::Right) {
                    keep = false;
                    if *fab_current > i {
                        *fab_current -= 1;
                    }
                }
            }
            i += 1;
            keep
        });
    }

    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
        if let Some(fab) = self.fabs.get(self.fab_current) {
            let fab = fab.clone();
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

