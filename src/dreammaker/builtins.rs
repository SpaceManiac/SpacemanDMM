//! BYOND built-in types, procs, and vars.

use super::objtree::*;
use super::ast::*;
use super::{Location, FileId, DMError};

/// Register BYOND builtins into the specified object tree.
pub fn register_builtins(tree: &mut ObjectTree) -> Result<(), DMError> {
    let location = Location {
        file: FileId::builtins(),
        line: 1,
        column: 1,
    };

    macro_rules! entries {
        ($($($elem:ident)/ * $(= $val:expr)*;)*) => {
            $(loop {
                #![allow(unreachable_code)]
                let elems = [$(stringify!($elem)),*];
                $(
                    tree.add_var(location, elems.iter().cloned(), $val)?;
                    break;
                )*
                tree.add_entry(location, elems.iter().cloned())?;
                break;
            })*
        }
    }

    macro_rules! path {
        ($(/$elem:ident)*) => {
            Expression::from(Term::Prefab(Prefab {
                path: vec![$((PathOp::Slash, stringify!($elem).to_owned())),*],
                vars: Default::default(),
            }))
        }
    }

    macro_rules! int {
        ($e:expr) => {Expression::from(Term::Int($e))}
    }

    entries! {
        // __root
        var/type;
        var/parent_type;
        var/tag;
        var/vars;

        datum;

        atom/parent_type = path!(/datum);
        atom/var/alpha;
        atom/var/appearance;
        atom/var/appearance_flags;
        atom/var/blend_mode;
        atom/var/color;
        atom/var/contents;
        atom/var/density;
        atom/var/desc;
        atom/var/dir;
        atom/var/gender;
        atom/var/icon/icon;
        atom/var/icon_state;
        atom/var/invisibility;
        atom/var/infra_luminosity;
        atom/var/atom/loc;
        atom/var/layer;
        atom/var/luminosity;
        atom/var/maptext;
        atom/var/maptext_width;
        atom/var/maptext_height;
        atom/var/maptext_x;
        atom/var/maptext_y;
        atom/var/mouse_over_pointer;
        atom/var/mouse_drag_pointer;
        atom/var/mouse_drop_pointer;
        atom/var/mouse_drop_zone;
        atom/var/mouse_opacity;
        atom/var/name;
        atom/var/opacity;
        atom/var/overlays;
        atom/var/override;
        atom/var/parent_type;
        atom/var/pixel_x;
        atom/var/pixel_y;
        atom/var/pixel_w;
        atom/var/pixel_z;
        atom/var/plane;
        atom/var/suffix;
        atom/var/tag;
        atom/var/text;
        atom/var/transform;
        atom/var/type;
        atom/var/underlays;
        atom/var/vars;
        atom/var/verbs;
        atom/var/x;
        atom/var/y;
        atom/var/z;

        atom/movable;
        atom/movable/var/animate_movement;
        atom/movable/var/bound_x;
        atom/movable/var/bound_y;
        atom/movable/var/bound_width;
        atom/movable/var/bound_height;
        atom/movable/var/locs;
        atom/movable/var/screen_loc;
        atom/movable/var/glide_size;
        atom/movable/var/step_size;
        atom/movable/var/step_x;
        atom/movable/var/step_y;

        area/parent_type = path!(/atom);
        turf/parent_type = path!(/atom);
        obj/parent_type = path!(/atom/movable);

        mob/parent_type = path!(/atom/movable);
        mob/var/ckey;
        mob/var/client/client;
        mob/var/list/group;
        mob/var/key;
        mob/var/see_infrared;
        mob/var/see_invisible;
        mob/var/see_in_dark;
        mob/var/sight;

        world;
        var/static/world/world;
        world/var/static/address;
        world/var/static/area/area = path!(/area);
        world/var/static/cache_lifespan = int!(30);
        world/var/static/contents;
        world/var/static/cpu;
        world/var/static/executor;
        world/var/static/fps = int!(10);
        world/var/static/game_state = int!(0);
        world/var/static/host;
        world/var/static/hub;
        world/var/static/hub_password;
        world/var/static/icon_size = int!(32);
        world/var/static/internet_address;
        world/var/static/log;
        world/var/static/loop_checks = int!(1);
        world/var/static/map_format = int!(0); // TOPDOWN_MAP
        world/var/static/maxx;
        world/var/static/maxy;
        world/var/static/maxz;
        world/var/static/mob/mob = path!(/mob);
        world/var/static/name = Expression::from(Term::String("byond".into()));
        world/var/static/params;
        world/var/static/port;
        world/var/static/realtime;
        world/var/static/reachable;
        world/var/static/sleep_offline = int!(0);
        world/var/static/status;
        world/var/static/system_type;
        world/var/static/tick_lag = int!(1);
        world/var/static/tick_usage;
        world/var/static/turf/turf = path!(/turf);
        world/var/static/time;
        world/var/static/timeofday;
        world/var/static/url;
        world/var/static/version = int!(0);
        world/var/static/view = int!(5);
        world/var/static/visibility = int!(1);

        client;
        client/var/address;
        client/var/authenticate;
        client/var/bounds;
        client/var/byond_version;
        client/var/CGI;
        client/var/ckey;
        client/var/color;
        client/var/command_text;
        client/var/connection;
        client/var/control_freak = int!(0);
        client/var/computer_id;
        client/var/default_verb_category = Expression::from(Term::String("Commands".into()));
        client/var/dir = int!(1);  // NORTH
        client/var/edge_limit;
        client/var/eye;
        client/var/fps = int!(0);
        client/var/gender;
        client/var/glide_size = int!(0);
        client/var/images;
        client/var/inactivity;
        client/var/key;
        client/var/lazy_eye;
        client/var/mob;
        client/var/mouse_pointer_icon;
        client/var/perspective = int!(0);  // MOB_PERSPECTIVE
        client/var/pixel_x = int!(0);
        client/var/pixel_y = int!(0);
        client/var/pixel_w = int!(0);
        client/var/pixel_z = int!(0);
        client/var/preload_rsc = int!(1);
        client/var/screen;
        client/var/script;
        client/var/show_map = int!(1);
        client/var/show_popup_menus = int!(1);
        client/var/show_verb_panel = int!(1);
        client/var/statobj;
        client/var/statpanel;
        client/var/tick_lag = int!(0);
        client/var/verbs;
        client/var/view;
        client/var/virtual_eye;

        sound;
        sound/var/file;
        sound/var/repeat;
        sound/var/wait;
        sound/var/channel;
        sound/var/volume;
        sound/var/frequency;
        sound/var/pan;
        sound/var/priority;
        sound/var/status;
        sound/var/x;
        sound/var/y;
        sound/var/z;
        sound/var/falloff;
        sound/var/environment;
        sound/var/echo;
    };

    Ok(())
}

#[test]
fn check_builtins() {
    let mut tree = ObjectTree::default();
    register_builtins(&mut tree).unwrap();
    println!("{:#?}", tree);
}
