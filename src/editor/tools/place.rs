use super::*;

/// The standard placement tool.
#[derive(Default)]
pub struct Place {
    fabs: Vec<Prefab>,
    fab_current: usize,
}

impl ToolBehavior for Place {
    fn settings(&mut self, ui: &Ui) {
        ui.text(im_str!("current: {} / {}", self.fab_current, self.fabs.len()));
        for (i, fab) in self.fabs.iter().enumerate() {
            ui.text(im_str!("{}. {}", i, fab.path));
        }
        if ui.small_button(im_str!("test")) {
            self.fabs.push(Prefab {
                path: "/obj/item/lighter".to_owned(),
                vars: Default::default(),
            });
        }
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

