use super::*;

use rand::seq::IndexedRandom;
use rand::RngExt;

#[derive(Default)]
pub struct Random;
impl RenderPass for Random {
    fn expand<'a>(
        &self,
        atom: &Atom<'a>,
        objtree: &'a ObjectTree,
        output: &mut Vec<Atom<'a>>,
    ) -> bool {
        let mut rng = rand::rng();

        if atom.istype("/obj/machinery/vending/snack/random/") {
            if let Some(root) = objtree.find("/obj/machinery/vending/snack") {
                let mut machines = Vec::new();
                for child in root.children() {
                    if child.name() != "random" {
                        machines.push(child.get());
                    }
                }
                if let Some(&replacement) = machines.choose(&mut rng) {
                    output.push(Atom::from(replacement));
                    return false; // consumed
                }
            }
        } else if atom.istype("/obj/machinery/vending/cola/random/") {
            if let Some(root) = objtree.find("/obj/machinery/vending/cola") {
                let mut machines = Vec::new();
                for child in root.children() {
                    if child.name() != "random" {
                        machines.push(child.get());
                    }
                }
                if let Some(&replacement) = machines.choose(&mut rng) {
                    output.push(Atom::from(replacement));
                    return false; // consumed
                }
            }
        } else if atom.istype("/obj/item/bedsheet/random/") {
            if let Some(root) = objtree.find("/obj/item/bedsheet") {
                let mut sheets = vec![root.get()]; // basic bedsheet is included
                for child in root.children() {
                    if child.name() != "random" {
                        sheets.push(child.get());
                    }
                }
                if let Some(&replacement) = sheets.choose(&mut rng) {
                    output.push(Atom::from(replacement));
                    return false; // consumed
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
                        if let Some(ty) = objtree.type_by_path(pop.path.iter()) {
                            // Usually pixel offsets would be set here, but
                            // that's not currently supported.
                            output.push(Atom::from(ty));
                        }
                    }
                    loot_spawned += 1;
                }
            }
            return false; // consumed
        }
        true
    }

    fn adjust_sprite<'a>(
        &self,
        atom: &Atom<'a>,
        sprite: &mut Sprite<'a>,
        objtree: &'a ObjectTree,
        bump: &'a bumpalo::Bump,
    ) {
        let mut rng = rand::rng();

        // small selection of contraband and legit poster icon states
        // in theory this could iterate through subtypes of /obj/structure/sign/poster,
        // but i am far too lazy for that ~lucy
        const CONTRABAND_POSTERS: &[&str] = &[
            "aspev_syndie",
            "microwave",
            "singletank_bomb",
            "kudzu",
            "free_drone",
            "lusty_xenomorph",
        ];
        const LEGIT_POSTERS: &[&str] = &[
            "aspev_hardhat",
            "aspev_piping",
            "aspev_meth",
            "aspev_epi",
            "aspev_delam",
            "cleanliness",
            "help_others",
            "build",
            "bless_this_spess",
            "science",
            "ue_no",
            "safety_internals",
            "safety_eye_protection",
        ];

        if atom.istype("/obj/structure/sign/poster/contraband/random/") {
            sprite.icon_state = CONTRABAND_POSTERS.choose(&mut rng).unwrap();
        } else if atom.istype("/obj/structure/sign/poster/official/random/") {
            sprite.icon_state = LEGIT_POSTERS.choose(&mut rng).unwrap();
        } else if atom.istype("/obj/structure/sign/poster/random/") {
            let poster_type = if rng.random_ratio(
                CONTRABAND_POSTERS.len() as u32,
                (CONTRABAND_POSTERS.len() + LEGIT_POSTERS.len()) as u32,
            ) {
                CONTRABAND_POSTERS
            } else {
                LEGIT_POSTERS
            };
            sprite.icon_state = poster_type.choose(&mut rng).unwrap();
        } else if atom.istype("/obj/item/kirbyplants/random/")
            || atom.istype("/obj/item/twohanded/required/kirbyplants/random/")
        {
            sprite.icon = "icons/obj/fluff/flora/plants.dmi";
            let random = rng.random_range(0..=29);
            if random == 0 {
                sprite.icon_state = "applebush";
            } else {
                sprite.icon_state =
                    bumpalo::format!(in bump, "plant-{:02}", random).into_bump_str();
            }
        } else if atom.istype("/obj/structure/sign/barsign/") {
            if let Some(root) = objtree.find("/datum/barsign") {
                let mut signs = Vec::new();
                for child in root.children() {
                    if let Some(v) = child.vars.get("hidden") {
                        if !v.value.constant.as_ref().is_some_and(|c| c.to_bool()) {
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
            ]
            .choose(&mut rng)
            .unwrap();
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
            ]
            .choose(&mut rng)
            .unwrap();
        }
    }
}

fn pickweight<'a>(list: &[&'a (Constant, Option<Constant>)]) -> &'a Constant {
    let mut total: i32 = list
        .iter()
        .map(|(_, v)| {
            v.as_ref()
                .unwrap_or_else(Constant::null)
                .to_int()
                .unwrap_or(1)
        })
        .sum();
    total = rand::rng().random_range(1..=total);
    for (k, v) in list.iter() {
        total -= v
            .as_ref()
            .unwrap_or_else(Constant::null)
            .to_int()
            .unwrap_or(1);
        if total <= 0 {
            return k;
        }
    }
    Constant::null()
}
