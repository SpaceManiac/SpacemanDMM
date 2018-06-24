//! BYOND built-in types, procs, and vars.

use super::objtree::*;
use super::ast::*;
use super::{Location, FileId, DMError};
use super::preprocessor::{DefineMap, Define};

/// Register BYOND builtin macros to the given define map.
pub fn default_defines(defines: &mut DefineMap) {
    use super::lexer::Token::*;
    let location = Location {
        file: FileId::builtins(),
        line: 1,
        column: 1,
    };

    macro_rules! c {
        ($($i:ident = $($x:expr),*;)*) => {
            $(
                assert!(defines.insert(
                    stringify!($i).into(), (location, Define::Constant { subst: vec![$($x),*] })
                ).is_none());
            )*
        }
    }
    c! {
        DM_VERSION = Int(512);

        // eye and sight
        SEEINVIS = Int(2);
        SEEMOBS = Int(4);
        SEEOBJS = Int(8);
        SEETURFS = Int(16);

        // gliding
        NO_STEPS = Int(0);
        FORWARD_STEPS = Int(1);
        SLIDE_STEPS = Int(2);
        SYNC_STEPS = Int(3);

        // appearance_flags
        LONG_GLIDE = Int(1);
        RESET_COLOR = Int(2);
        RESET_ALPHA = Int(4);
        RESET_TRANSFORM = Int(8);
        NO_CLIENT_COLOR = Int(16);
        KEEP_TOGETHER = Int(32);
        KEEP_APART = Int(64);
        PLANE_MASTER = Int(128);
        TILE_BOUND = Int(256);
        PIXEL_SCALE = Int(512);

        CONTROL_FREAK_ALL = Int(1);
        CONTROL_FREAK_SKIN = Int(2);
        CONTROL_FREAK_MACROS = Int(4);

        // icons
        ICON_ADD = Int(0);
        ICON_SUBTRACT = Int(1);
        ICON_MULTIPLY = Int(2);
        ICON_OVERLAY = Int(3);
        ICON_AND = Int(4);
        ICON_OR = Int(5);
        ICON_UNDERLAY = Int(6);

        // matrix
        MATRIX_COPY = Int(0);
        MATRIX_MULTIPLY = Int(1);
        MATRIX_ADD = Int(2);
        MATRIX_SUBTRACT = Int(3);
        MATRIX_INVERT = Int(4);
        MATRIX_ROTATE = Int(5);
        MATRIX_SCALE = Int(6);
        MATRIX_TRANSLATE = Int(7);
        MATRIX_INTERPOLATE = Int(8);
        MATRIX_MODIFY = Int(128);

        // animation easing
        LINEAR_EASING = Int(0);
        SINE_EASING = Int(1);
        CIRCULAR_EASING = Int(2);
        CUBIC_EASING = Int(3);
        BOUNCE_EASING = Int(4);
        ELASTIC_EASING = Int(5);
        BACK_EASING = Int(6);
        QUAD_EASING = Int(7);
        EASE_IN = Int(64);
        EASE_OUT = Int(128);

        // animation flags
        ANIMATION_END_NOW = Int(1);
        ANIMATION_LINEAR_TRANSFORM = Int(2);
        ANIMATION_PARALLEL = Int(4);
        ANIMATION_RELATIVE = Int(256);

        // database
        DATABASE_OPEN = Int(0);
        DATABASE_CLOSE = Int(1);
        DATABASE_ERROR_CODE = Int(2);
        DATABASE_ERROR = Int(3);
        DATABASE_QUERY_CLEAR = Int(4);
        DATABASE_QUERY_ADD = Int(5);
        DATABASE_QUERY_EXEC = Int(8);
        DATABASE_QUERY_NEXT = Int(9);
        DATABASE_QUERY_ABORT = Int(10);
        DATABASE_QUERY_RESET = Int(11);
        DATABASE_QUERY_ROWS_AFFECTED = Int(12);
        DATABASE_ROW_COLUMN_NAMES = Int(16);
        DATABASE_ROW_COLUMN_VALUE = Int(17);
        DATABASE_ROW_LIST = Int(18);
    }
    // TODO: ASSERT, CRASH, EXCEPTION, REGEX_QUOTE, REGEX_QUOTE_REPLACEMENT
}

/// Register BYOND builtins into the specified object tree.
pub fn register_builtins(tree: &mut ObjectTree) -> Result<(), DMError> {
    let location = Location {
        file: FileId::builtins(),
        line: 1,
        column: 1,
    };

    macro_rules! entries {
        ($($($elem:ident)/ * $(($($arg:ident),*))* $(= $val:expr)*;)*) => {
            $(loop {
                #![allow(unreachable_code)]
                let elems = [$(stringify!($elem)),*];
                $(
                    tree.add_var(location, elems.iter().cloned(), elems.len() + 1, $val)?;
                    break;
                )*
                $(
                    tree.add_proc(location, elems.iter().cloned(), elems.len() + 1, vec![$(
                        Parameter { name: stringify!($arg).to_owned(), .. Default::default() }
                    ),*])?;
                    break;
                )*
                tree.add_entry(location, elems.iter().cloned(), elems.len() + 1)?;
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
    macro_rules! string {
        ($e:expr) => {Expression::from(Term::String($e.into()))}
    }

    entries! {
        // directions
        var/const/NORTH = int!(1);
        var/const/SOUTH = int!(2);
        var/const/EAST = int!(4);
        var/const/WEST = int!(8);
        var/const/NORTHEAST = int!(5);
        var/const/SOUTHEAST = int!(6);
        var/const/NORTHWEST = int!(9);
        var/const/SOUTHWEST = int!(10);
        var/const/UP = int!(16);
        var/const/DOWN = int!(32);

        // eye and sight
        var/const/BLIND = int!(1);
        var/const/SEE_MOBS = int!(4);
        var/const/SEE_OBJS = int!(8);
        var/const/SEE_TURFS = int!(16);
        var/const/SEE_SELF = int!(32);
        var/const/SEE_INFRA = int!(64);
        var/const/SEE_PIXELS = int!(256);
        var/const/SEE_THRU = int!(512);
        var/const/SEE_BLACKNESS = int!(1024);

        var/const/MOB_PERSPECTIVE = int!(0);
        var/const/EYE_PERSPECTIVE = int!(1);
        var/const/EDGE_PERSPECTIVE = int!(2);

        // layers
        var/const/FLOAT_LAYER = int!(-1);
        var/const/AREA_LAYER = int!(1);
        var/const/TURF_LAYER = int!(2);
        var/const/OBJ_LAYER = int!(3);
        var/const/MOB_LAYER = int!(4);
        var/const/FLY_LAYER = int!(5);
        var/const/EFFECTS_LAYER = int!(5000);
        var/const/TOPDOWN_LAYER = int!(10000);
        var/const/BACKGROUND_LAYER = int!(20000);
        var/const/FLOAT_PLANE = int!(-32767);

        // map formats
        var/const/TOPDOWN_MAP = int!(0);
        var/const/ISOMETRIC_MAP = int!(1);
        var/const/SIDE_MAP = int!(2);
        var/const/TILED_ICON_MAP = int!(32768);

        var/const/TRUE = int!(1);
        var/const/FALSE = int!(0);

        var/const/MALE = string!("male");
        var/const/FEMALE = string!("female");
        var/const/NEUTER = string!("neuter");
        var/const/PLURAL = string!("plural");

        var/const/MOUSE_INACTIVE_POINTER = int!(0);
        var/const/MOUSE_ACTIVE_POINTER = int!(1);
        var/const/MOUSE_DRAG_POINTER = int!(3);
        var/const/MOUSE_DROP_POINTER = int!(4);
        var/const/MOUSE_ARROW_POINTER = int!(5);
        var/const/MOUSE_CROSSHAIRS_POINTER = int!(6);
        var/const/MOUSE_HAND_POINTER = int!(7);

        var/const/MOUSE_LEFT_BUTTON = int!(1);
        var/const/MOUSE_RIGHT_BUTTON = int!(2);
        var/const/MOUSE_MIDDLE_BUTTON = int!(4);
        var/const/MOUSE_CTRL_KEY = int!(8);
        var/const/MOUSE_SHIFT_KEY = int!(16);
        var/const/MOUSE_ALT_KEY = int!(32);

        var/const/MS_WINDOWS = string!("MS Windows");
        var/const/UNIX = string!("UNIX");

        // sound
        var/const/SOUND_MUTE = int!(1);
        var/const/SOUND_PAUSED = int!(2);
        var/const/SOUND_STREAM = int!(4);
        var/const/SOUND_UPDATE = int!(16);

        // blend_mode
        var/const/BLEND_DEFAULT = int!(0);
        var/const/BLEND_OVERLAY = int!(1);
        var/const/BLEND_ADD = int!(2);
        var/const/BLEND_SUBTRACT = int!(3);
        var/const/BLEND_MULTIPLY = int!(4);

        // __root
        var/type;
        var/parent_type;
        var/tag;
        var/vars;

        datum;
        datum/proc/New();
        datum/proc/Del();
        datum/proc/Read(/*savefile*/F);
        datum/proc/Topic(href, href_list);
        datum/proc/Write(/*savefile*/F);

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
        atom/var/pixel_x;
        atom/var/pixel_y;
        atom/var/pixel_w;
        atom/var/pixel_z;
        atom/var/plane;
        atom/var/suffix;
        atom/var/text;
        atom/var/transform;
        atom/var/underlays;
        atom/var/verbs;
        atom/var/x;
        atom/var/y;
        atom/var/z;
        atom/proc/Click(location, control, params);
        atom/proc/DblClick(location, control, params);
        atom/proc/Enter(/*atom/movable*/O, /*atom*/oldloc);
        atom/proc/Entered(/*atom/movable*/Obj, /*atom*/OldLoc);
        atom/proc/Exit(/*atom/movable*/O, /*atom*/newloc);
        atom/proc/Exited(/*atom/movable*/Obj, /*atom*/newloc);
        atom/proc/MouseDown(location, control, params);
        atom/proc/MouseDrag(over_object, src_location, over_location, src_control, over_control, params);
        atom/proc/MouseDrop(over_object, src_location, over_location, src_control, over_control, params);
        atom/proc/MouseEntered(location, control, params);
        atom/proc/MouseExited(location, control, params);
        atom/proc/MouseMove(location, control, params);
        atom/proc/MouseUp(location, control, params);
        atom/proc/MouseWheel(delta_x, delta_y, location, control, params);
        atom/New(loc);
        atom/proc/Stat();

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
        atom/movable/proc/Bump(/*atom*/Obstacle);
        atom/movable/proc/Cross(/*atom/movable*/O);
        atom/movable/proc/Crossed(/*atom/movable*/O);
        atom/movable/proc/Move(NewLoc, Dir/*=0*/, step_x/*=0*/, step_y/*=0*/);
        atom/movable/proc/Uncross(/*atom/movable*/O);
        atom/movable/proc/Uncrossed(/*atom/movable*/O);

        area/parent_type = path!(/atom);
        area/layer = int!(1);

        turf/parent_type = path!(/atom);
        turf/layer = int!(2);

        obj/parent_type = path!(/atom/movable);
        obj/layer = int!(3);

        mob/parent_type = path!(/atom/movable);
        mob/layer = int!(4);
        mob/var/ckey;
        mob/var/client/client;
        mob/var/list/group;
        mob/var/key;
        mob/var/see_infrared;
        mob/var/see_invisible;
        mob/var/see_in_dark;
        mob/var/sight;
        mob/proc/Login();
        mob/proc/Logout();

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
        client/proc/AllowUpload(filename, filelength);
        client/proc/Center();
        client/proc/CheckPassport(passport_identifier);
        client/proc/Click(object, location, control, params);
        client/proc/Command(command);
        client/proc/DblClick(object, location, control, params);
        client/Del();
        client/proc/East();
        client/proc/Export(file);
        client/proc/Import(Query);
        client/proc/IsByondMember();
        client/proc/MouseDown(object, location, control, params);
        client/proc/MouseDrag(src_object, over_object, src_location, over_location, src_control, over_control, params);
        client/proc/MouseDrop(over_object, src_location, over_location, src_control, over_control, params);
        client/proc/MouseEntered(object, location, control, params);
        client/proc/MouseExited(object, location, control, params);
        client/proc/MouseMove(object, location, control, params);
        client/proc/MouseUp(object, location, control, params);
        client/proc/MouseWheel(object, delta_x, delta_y, location, control, params);
        client/proc/Move(loc, dir);
        client/New();
        client/proc/North();
        client/proc/Northeast();
        client/proc/Northwest();
        client/proc/SendPage(msg, recipient, options);
        client/proc/South();
        client/proc/Southeast();
        client/proc/Southwest();
        client/proc/Stat();
        client/Topic(href, href_list, hsrc);
        client/proc/West();

        sound;
        sound/var/file;
        sound/var/repeat;
        sound/var/wait;
        sound/var/channel;
        sound/var/frequency = int!(0);
        sound/var/pan = int!(0);
        sound/var/volume = int!(100);
        sound/var/priority = int!(0);
        sound/var/status = int!(0);
        sound/var/environment = int!(-1);
        sound/var/echo;
        sound/var/x = int!(0);
        sound/var/y = int!(0);
        sound/var/z = int!(0);
        sound/var/falloff = int!(1);
        sound/New(file, repeat, wait, channel, volume);

        icon;
        icon/proc/Blend(icon, function/*=ICON_ADD*/, x/*=1*/, y/*=1*/);
        icon/proc/Crop(x1, y1, x2, y2);
        icon/proc/DrawBox(rgb, x1, y1, x2/*=x1*/, y2/*=y1*/);
        icon/proc/Flip(dir);
        icon/proc/GetPixel(x, y, icon_state, dir/*=0*/, frame/*=0*/, moving/*=-1*/);
        icon/proc/Width();
        icon/proc/IconStates(mode/*=0*/);
        icon/proc/Insert(new_icon, icon_state, dir, frame, moving, delay);
        icon/proc/MapColors(/*...*/);
        icon/New(icon, icon_state, dir, frame, moving);
        icon/proc/Scale(width, height);
        icon/proc/SetIntensity(r, g/*=r*/, b/*=r*/);
        icon/proc/Shift(dir, offset, wrap/*=0*/);
        icon/proc/SwapColor(old_rgba, new_rgba);
        icon/proc/Turn(angle);
        icon/proc/Height();

        matrix;
        matrix/var/a;
        matrix/var/b;
        matrix/var/c;
        matrix/var/d;
        matrix/var/e;
        matrix/var/f;
        matrix/New();
        matrix/proc/Multiply(m);
        matrix/proc/Add(m);
        matrix/proc/Subtract(m);
        matrix/proc/Invert();
        matrix/proc/Turn(a);
        matrix/proc/Scale(x, y);
        matrix/proc/Translate(x, y);
        matrix/proc/Interpolate(m2, t);

        exception;
        exception/var/name;
        exception/var/desc;
        exception/var/file;
        exception/var/line;
        exception/New(name, file, line);

        regex;
        regex/var/name;
        regex/var/flags;
        regex/var/text;
        regex/var/match;
        regex/var/list/group;
        regex/var/index;
        regex/var/next;
        regex/New(text, flags);
        regex/proc/Find(text, start, end);
        regex/proc/Replace(text, rep, start, end);

        database;
        database/proc/Close();
        database/proc/Error();
        database/proc/ErrorMsg();
        database/proc/Open(filename);
        database/proc/New(filename);

        database_query/proc/Add(text, item1, item2 /*...*/);
        database_query/proc/Close();
        database_query/proc/Columns(column);
        database_query/proc/Error();
        database_query/proc/ErrorMsg();
        database_query/proc/Execute(database);
        database_query/proc/GetColumn(column);
        database_query/proc/GetRowData();
        database_query/proc/New(text, item1, item2 /*...*/);
        database_query/proc/NextRow();
        database_query/proc/Reset();
        database_query/proc/RowsAffected();

        image;
        image/var/alpha;
        image/var/appearance;
        image/var/appearance_flags;
        image/var/blend_mode;
        image/var/color;
        image/var/icon/icon;
        image/var/icon_state;
        image/var/text;
        image/var/dir;
        image/var/underlays;
        image/var/overlays;
        image/var/atom/loc;
        image/var/layer = int!(5);
        image/var/maptext;
        image/var/maptext_width;
        image/var/maptext_height;
        image/var/maptext_x;
        image/var/maptext_y;
        image/var/pixel_x;
        image/var/pixel_y;
        image/var/pixel_w;
        image/var/pixel_z;
        image/var/plane;
        image/var/x;
        image/var/y;
        image/var/z;
        image/var/override;
        image/var/transform;
        mutable_appearance/parent_type = path!(/image);
    };

    Ok(())
}
