use super::*;

#[derive(Default)]
pub struct Random;
impl RenderPass for Random {
    fn expand<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        use rand::Rng;
        let mut rng = ::rand::thread_rng();

        if atom.istype("/obj/machinery/vending/snack/random/") {
            if let Some(root) = objtree.find("/obj/machinery/vending/snack") {
                let mut machines = Vec::new();
                for child in root.children() {
                    if child.name != "random" {
                        machines.push(child.get());
                    }
                }
                if let Some(replacement) = rng.choose(&machines) {
                    output.push(Atom::from_type_ref(replacement, atom.loc));
                    return true; // consumed
                }
            }
        } else if atom.istype("/obj/machinery/vending/cola/random/") {
            if let Some(root) = objtree.find("/obj/machinery/vending/cola") {
                let mut machines = Vec::new();
                for child in root.children() {
                    if child.name != "random" {
                        machines.push(child.get());
                    }
                }
                if let Some(replacement) = rng.choose(&machines) {
                    output.push(Atom::from_type_ref(replacement, atom.loc));
                    return true; // consumed
                }
            }
        } else if atom.istype("/obj/item/bedsheet/random/") {
            if let Some(root) = objtree.find("/obj/item/bedsheet") {
                let mut sheets = vec![root.get()]; // basic bedsheet is included
                for child in root.children() {
                    if child.name != "random" {
                        sheets.push(child.get());
                    }
                }
                if let Some(replacement) = rng.choose(&sheets) {
                    output.push(Atom::from_type_ref(replacement, atom.loc));
                    return true; // consumed
                }
            }
        }
        false
    }

    fn adjust_vars<'a>(&self, atom: &mut Atom<'a>, objtree: &'a ObjectTree) {
        use rand::Rng;
        let mut rng = ::rand::thread_rng();

        const CONTRABAND_POSTERS: u32 = 44;
        const LEGIT_POSTERS: u32 = 35;

        if atom.istype("/obj/structure/sign/poster/contraband/random/") {
            atom.set_var(
                "icon_state",
                Constant::string(format!(
                    "poster{}",
                    rng.gen_range(1, 1 + CONTRABAND_POSTERS)
                )),
            );
        } else if atom.istype("/obj/structure/sign/poster/official/random/") {
            atom.set_var(
                "icon_state",
                Constant::string(format!(
                    "poster{}_legit",
                    rng.gen_range(1, 1 + LEGIT_POSTERS)
                )),
            );
        } else if atom.istype("/obj/structure/sign/poster/random/") {
            let i = 1 + rng.gen_range(0, CONTRABAND_POSTERS + LEGIT_POSTERS);
            if i <= CONTRABAND_POSTERS {
                atom.set_var("icon_state", Constant::string(format!("poster{}", i)));
            } else {
                atom.set_var(
                    "icon_state",
                    Constant::string(format!("poster{}_legit", i - CONTRABAND_POSTERS)),
                );
            }
        } else if atom.istype("/obj/item/twohanded/required/kirbyplants/random/") {
            atom.set_var("icon", Constant::string("icons/obj/flora/plants.dmi"));
            let random = rng.gen_range(0, 26);
            if random == 0 {
                atom.set_var("icon_state", Constant::string("applebush"));
            } else {
                atom.set_var(
                    "icon_state",
                    Constant::string(format!("plant-{:02}", random)),
                );
            }
        } else if atom.istype("/obj/structure/sign/barsign/") {
            if let Some(root) = objtree.find("/datum/barsign") {
                let mut signs = Vec::new();
                for child in root.children() {
                    if let Some(v) = child.vars.get("hidden") {
                        if !v.value.constant.as_ref().map_or(false, |c| c.to_bool()) {
                            continue;
                        }
                    }
                    if let Some(icon) = child.vars.get("icon") {
                        if let Some(c) = icon.value.constant.as_ref() {
                            signs.push(c.clone());
                        }
                    }
                }
                if let Some(c) = rng.choose(&signs) {
                    atom.set_var("icon_state", c.clone());
                }
            }
        } else if atom.istype("/obj/item/lipstick/random/") {
            atom.set_var("icon_state", Constant::string("lipstick"));
        // random color is not outwardly visible
        } else if atom.istype("/obj/item/device/tape/random/") {
            let icon = format!(
                "tape_{}",
                rng.choose(&["white", "blue", "red", "yellow", "purple"])
                    .unwrap()
            );
            atom.set_var("icon_state", Constant::string(icon));
        }
    }
}
