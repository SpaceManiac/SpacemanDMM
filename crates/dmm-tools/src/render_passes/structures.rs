use super::*;

#[derive(Default)]
pub struct Spawners;
impl RenderPass for Spawners {
    fn expand<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        if !atom.istype("/obj/effect/spawner/structure/") {
            return true;
        }
        match atom.get_var("spawn_list", objtree) {
            &Constant::List(ref elements) => {
                for &(ref key, _) in elements.iter() {
                    // TODO: use a more civilized lookup method
                    let type_key;
                    let reference = match *key {
                        Constant::String(ref s) => s,
                        Constant::Prefab(ref fab) => {
                            type_key = dm::ast::FormatTreePath(&fab.path).to_string();
                            type_key.as_str()
                        }
                        _ => continue,
                    };
                    output.push(Atom::from(objtree.expect(reference)));
                }
                false // don't include the original atom
            }
            _ => true, // TODO: complain?
        }
    }
}

#[derive(Default)]
pub struct GravityGen;
impl RenderPass for GravityGen {
    fn overlays<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _underlays: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
        _: &bumpalo::Bump,
    ) {
        if !atom.istype("/obj/machinery/gravity_generator/main/station/") {
            return;
        }

        for &(count, icon_state, x, y) in &[
            (1, "on_1", 1, 2),
            (2, "on_2", 0, 2),
            (3, "on_3", -1, 2),
            (4, "on_4", 1, 1),
            (5, "on_5", 0, 1),
            (6, "on_6", -1, 1),
            (7, "on_7", 1, 0),
            (9, "on_9", -1, 0),
        ] {
            let mut sprite = Sprite::from_vars(
                objtree,
                &objtree.expect("/obj/machinery/gravity_generator/part"),
            );
            sprite.ofs_x += 32 * x;
            sprite.ofs_y += 32 * y;
            sprite.icon_state = icon_state;
            sprite.plane = 0; // TODO: figure out plane handling for real
            if count <= 3 {
                sprite.layer = Layer::from(4.25); // WALL_OBJ_LAYER
            }
            if count == 5 {
                // energy overlay goes above the middle part
                overlays.push(sprite.clone());
                sprite.icon_state = "activated";
            }
            overlays.push(sprite);
        }
    }
}
