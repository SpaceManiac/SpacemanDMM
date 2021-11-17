use super::*;

use rand::Rng;
use rand::seq::SliceRandom;

#[derive(Default)]
pub struct Random;
impl RenderPass for Random {
    fn expand<'a>(&self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        let mut rng = rand::thread_rng();

        if atom.istype("/obj/machinery/vending/snack/random/") {
            if let Some(root) = objtree.find("/obj/machinery/vending/snack") {
                let mut machines = Vec::new();
                for child in root.children() {
                    if child.name != "random" {
                        machines.push(child.get());
                    }
                }
                if let Some(&replacement) = machines.choose(&mut rng) {
                    output.push(Atom::from(replacement));
                    return false;  // consumed
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
                if let Some(&replacement) = machines.choose(&mut rng) {
                    output.push(Atom::from(replacement));
                    return false;  // consumed
                }
            }
        } else if atom.istype("/obj/item/bedsheet/random/") {
            if let Some(root) = objtree.find("/obj/item/bedsheet") {
                let mut sheets = vec![root.get()];  // basic bedsheet is included
                for child in root.children() {
                    if child.name != "random" {
                        sheets.push(child.get());
                    }
                }
                if let Some(&replacement) = sheets.choose(&mut rng) {
                    output.push(Atom::from(replacement));
                    return false;  // consumed
                }
            }
        } else if atom.istype("/obj/effect/spawner/lootdrop/") {
            // Note: does not work with lootdrop/maintenance because its drop
            // tables are stored in GLOB init proc bodies and are not part of
            // the object tree proper.
            if let Constant::List(loot) = atom.get_var("loot", objtree) {
                let mut loot = loot.iter().collect::<Vec<_>>();

                let lootcount = atom.get_var("lootcount", objtree).to_int().unwrap_or(0);
                let lootdoubles = atom.get_var("lootdoubles", objtree).to_bool();
                //let fan_out_items = atom.get_var("fan_out_items", objtree).to_bool();
                let mut loot_spawned = 0;

                while loot_spawned < lootcount && !loot.is_empty() {
                    let lootspawn = pickweight(&loot);
                    /*while let Constant::List(ref list) = lootspawn {
                        lootspawn = pickweight(list);
                    }*/
                    if !lootdoubles {
                        loot.retain(|(k, _)| k != lootspawn);
                    }

                    if let Constant::Prefab(pop) = lootspawn {
                        if let Some(ty) = objtree.type_by_path(&pop.path) {
                            // Usually pixel offsets would be set here, but
                            // that's not currently supported.
                            output.push(Atom::from(ty));
                        }
                    }
                    loot_spawned += 1;
                }
            }
            return false;  // consumed
        }
        true
    }

    fn adjust_sprite<'a>(&self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        objtree: &'a ObjectTree,
        bump: &'a bumpalo::Bump,
    ) {
        let mut rng = rand::thread_rng();

        const CONTRABAND_POSTERS: u32 = 44;
        const LEGIT_POSTERS: u32 = 35;

        if atom.istype("/obj/structure/sign/poster/contraband/random/") {
            sprite.icon_state = bumpalo::format!(in bump, "poster{}", rng.gen_range(1, 1 + CONTRABAND_POSTERS)).into_bump_str();
        } else if atom.istype("/obj/structure/sign/poster/official/random/") {
            sprite.icon_state = bumpalo::format!(in bump, "poster{}_legit", rng.gen_range(1, 1 + LEGIT_POSTERS)).into_bump_str();
        } else if atom.istype("/obj/structure/sign/poster/random/") {
            let i = 1 + rng.gen_range(0, CONTRABAND_POSTERS + LEGIT_POSTERS);
            if i <= CONTRABAND_POSTERS {
                sprite.icon_state = bumpalo::format!(in bump, "poster{}", i).into_bump_str();
            } else {
                sprite.icon_state = bumpalo::format!(in bump, "poster{}_legit", i - CONTRABAND_POSTERS).into_bump_str();
            }
        } else if atom.istype("/obj/item/kirbyplants/random/") || atom.istype("/obj/item/twohanded/required/kirbyplants/random/") {
            sprite.icon = "icons/obj/flora/plants.dmi";
            let random = rng.gen_range(0, 26);
            if random == 0 {
                sprite.icon_state = "applebush";
            } else {
                sprite.icon_state = bumpalo::format!(in bump, "plant-{:02}", random).into_bump_str();
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
                    if let Some(icon) = child.get().vars.get("icon") {
                        if let Some(c) = icon.value.constant.as_ref() {
                            if let Some(text) = c.as_str() {
                                signs.push(text);
                            }
                        }
                    }
                }
                if let Some(c) = signs.choose(&mut rng) {
                    sprite.icon_state = c;
                }
            }
        } else if atom.istype("/obj/item/relic/") {
	        sprite.icon_state = [
                "shock_kit",
                "armor-igniter-analyzer",
                "infra-igniter0",
                "infra-igniter1",
                "radio-multitool",
                "prox-radio1",
                "radio-radio",
                "timer-multitool0",
                "radio-igniter-tank",
            ].choose(&mut rng).unwrap();
        }

        if atom.istype("/obj/item/lipstick/random/") {
            sprite.icon_state = "lipstick";
            // random color is not outwardly visible
        } else if atom.istype("/obj/item/tape/random/") {
            sprite.icon_state = [
                "tape_white",
                "tape_blue",
                "tape_red",
                "tape_yellow",
                "tape_purple",
            ].choose(&mut rng).unwrap();
        }
    }
}

fn pickweight<'a>(list: &[&'a (Constant, Option<Constant>)]) -> &'a Constant {
    let mut total: i32 = list.iter().map(|(_, v)| v.as_ref().unwrap_or(Constant::null()).to_int().unwrap_or(1)).sum();
    total = rand::thread_rng().gen_range(1, total + 1);
    for (k, v) in list.iter() {
        total -= v.as_ref().unwrap_or(Constant::null()).to_int().unwrap_or(1);
        if total <= 0 {
            return k;
        }
    }
    Constant::null()
}
