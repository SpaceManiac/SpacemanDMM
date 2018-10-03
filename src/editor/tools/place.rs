use super::*;

/// The standard placement tool.
#[derive(Default)]
pub struct Place;

impl ToolBehavior for Place {
    fn click(&mut self, hist: &mut History, env: &Environment, loc: (u32, u32, u32)) {
        hist.edit(env, "TODO".to_owned(), move |env, world| {
            let pop = world.add_pop(&Prefab {
                path: "/obj/item/lighter".to_owned(),
                vars: Default::default(),
            }, &env.icons, &env.objtree);
            let inst = world.add_instance(loc, pop);
            Box::new(move |_, world| {
                world.remove_instance(inst.clone());
            })
        });
    }
}

