use super::*;

#[derive(Default)]
pub struct Spawners;
impl RenderPass for Spawners {
    fn path_filter(&self, path: &str) -> bool {
        subpath(path, "/obj/effect/spawner/structure/") || !subpath(path, "/obj/effect/spawner/")
    }

    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        if !atom.istype("/obj/effect/spawner/structure/") {
            return false;
        }
        match atom.get_var("spawn_list", objtree) {
            &Constant::List(ref elements) => {
                for &(ref key, _) in elements {
                    // TODO: use a more civilized lookup method
                    let type_key;
                    let reference = match key {
                        &Constant::String(ref s) => s,
                        &Constant::Prefab(ref fab) => {
                            type_key = dm::ast::FormatTreePath(&fab.path).to_string();
                            &type_key
                        },
                        _ => continue,
                    };
                    output.push(Atom::from_type(objtree, reference, atom.loc).unwrap());
                }
                true  // don't include the original atom
            }
            _ => { false }  // TODO: complain?
        }
    }
}

#[derive(Default)]
pub struct GravityGen;
impl RenderPass for GravityGen {
    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        if !atom.istype("/obj/machinery/gravity_generator/main/station/") {
            return false;
        }

        for &(count, icon_state, x, y) in &[
            (1, "on_1", 1, -2),
            (2, "on_2", 0, -2),
            (3, "on_3", -1, -2),
            (4, "on_4", 1, -1),
            (5, "on_5", 0, -1),
            (6, "on_6", -1, -1),
            (7, "on_7", 1, 0),
            (9, "on_9", -1, 0),
        ] {
            let new_loc = ((atom.loc.0 as i32 + x) as u32, (atom.loc.1 as i32 + y) as u32);
            let mut new_atom = Atom::from_type(objtree, "/obj/machinery/gravity_generator/part", new_loc).unwrap();
            new_atom.set_var("icon_state", Constant::string(icon_state));
            if count <= 3 {
                new_atom.set_var("layer", Constant::from(4.25));  // WALL_OBJ_LAYER
            }
            output.push(new_atom);
        }
        false
    }

    /// Apply overlays and underlays to an atom, in the form of pseudo-atoms.
    fn overlays<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        _underlays: &mut Vec<Sprite<'a>>,
        overlays: &mut Vec<Sprite<'a>>,
    ) {
        // energy overlay goes above the middle part
        if atom.istype("/obj/machinery/gravity_generator/part/") &&
            atom.get_var("icon_state", objtree) == &Constant::string("on_5")
        {
            add_to(overlays, atom, "activated");
        }
    }
}
