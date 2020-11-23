//! BYOND built-in types, procs, and vars.

use builtins_proc_macro::entries;

use super::objtree::*;
use super::{Location, DMError};
use super::preprocessor::{DefineMap, Define};
use super::constants::Constant;

const DM_VERSION: i32 = 513;
const DM_BUILD: i32 = 1527;

/// Register BYOND builtin macros to the given define map.
pub fn default_defines(defines: &mut DefineMap) {
    use super::lexer::*;
    use super::lexer::Token::*;
    let location = Location::builtins();

    // #define EXCEPTION(value) new /exception(value)
    defines.insert("EXCEPTION".to_owned(), (location, Define::Function {
        params: vec!["value".to_owned()],
        variadic: false,
        subst: vec![
            Ident("new".to_owned(), true),
            Punct(Punctuation::Slash),
            Ident("exception".to_owned(), false),
            Punct(Punctuation::LParen),
            Ident("value".to_owned(), false),
            Punct(Punctuation::RParen),
        ],
        docs: Default::default(),
    }));

    // #define ASSERT(expression) if (!(expression)) { CRASH("[__FILE__]:[__LINE__]:Assertion Failed: [#X]") }
    defines.insert("ASSERT".to_owned(), (location, Define::Function {
        params: vec!["expression".to_owned()],
        variadic: false,
        subst: vec![
            Ident("if".to_owned(), true),
            Punct(Punctuation::LParen),
            Punct(Punctuation::Not),
            Punct(Punctuation::LParen),
            Ident("expression".to_owned(), false),
            Punct(Punctuation::RParen),
            Punct(Punctuation::RParen),
            Punct(Punctuation::LBrace),
            Ident("CRASH".to_owned(), false),
            Punct(Punctuation::LParen),
            InterpStringBegin("".to_owned()),
            Ident("__FILE__".to_owned(), false),
            InterpStringPart(":".to_owned()),
            Ident("__LINE__".to_owned(), false),
            InterpStringPart(":Assertion Failed: ".to_owned()),
            Punct(Punctuation::Hash),
            Ident("expression".to_owned(), false),
            InterpStringEnd("".to_owned()),
            Punct(Punctuation::RParen),
            Punct(Punctuation::RBrace),
        ],
        docs: Default::default(),
    }));

    // constants
    macro_rules! c {
        ($($i:ident = $($x:expr),*;)*) => {
            for (name, value) in &[
                $((stringify!($i), [$($x,)*]),)*
            ] {
                let previous = defines.insert(name.to_string(), (location, Define::Constant { subst: value.to_vec(), docs: Default::default() }));
                assert!(previous.is_none(), name);
            }
        }
    }
    c! {
        DM_VERSION = Int(DM_VERSION);
        DM_BUILD = Int(DM_BUILD);
        SPACEMAN_DMM = Int(1);

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
        PASS_MOUSE = Int(1024);

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
        JUMP_EASING = Int(256); // 513

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

        // 513 stuff

        // vis_flags
        VIS_INHERIT_ICON = Int(1);
        VIS_INHERIT_ICON_STATE = Int(2);
        VIS_INHERIT_DIR = Int(4);
        VIS_INHERIT_LAYER = Int(8);
        VIS_INHERIT_PLANE = Int(16);
        VIS_INHERIT_ID = Int(32);
        VIS_UNDERLAY = Int(64);
        VIS_HIDE = Int(128);

        // world.Profile()
        PROFILE_STOP = Int(1);
        PROFILE_CLEAR = Int(2);
        PROFILE_AVERAGE = Int(4);
        PROFILE_START = Int(0);
        PROFILE_REFRESH = Int(0);
        PROFILE_RESTART = Int(2);
    }
}

/// Register BYOND builtins into the specified object tree.
pub fn register_builtins(tree: &mut ObjectTree) -> Result<(), DMError> {
    macro_rules! one_entry {
        ($($elem:ident)/ *) => {
            tree.add_builtin_entry(&[$(stringify!($elem)),*])?;
        };
        ($($elem:ident)/ * = $val:expr) => {
            tree.add_builtin_var(&[$(stringify!($elem)),*], $val)?;
        };
        ($($elem:ident)/ * ($($arg:ident $(= $ignored:expr)*),*)) => {
            tree.add_builtin_proc(&[$(stringify!($elem)),*], &[$(stringify!($arg)),*])?;
        }
    }

    macro_rules! path {
        ($(/$elem:ident)*) => {
            Constant::Prefab(super::constants::Pop {
                path: vec![$(stringify!($elem).to_owned()),*],
                vars: Default::default(),
            })
        }
    }
    macro_rules! int {
        ($e:expr) => {
            Constant::Int($e)
        };
    }
    macro_rules! string {
        ($e:expr) => {
            Constant::String($e.into())
        };
    }

    entries! {
        var/const/vars;
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
        var/const/BLEND_INSET_OVERLAY = int!(5);

        // this is just a procstyle syntax wrapper for \ref[foo]
        proc/ref(A);

        // alpha mask filter
        var/const/MASK_INVERSE = int!(1);
        var/const/MASK_SWAP = int!(2);

        // rgb filter
        var/const/FILTER_COLOR_RGB = int!(0);
        var/const/FILTER_COLOR_HSV = int!(1);
        var/const/FILTER_COLOR_HSL = int!(2);
        var/const/FILTER_COLOR_HCY = int!(3);

        // layering / ray filter
        var/const/FILTER_OVERLAY = int!(1);
        var/const/FILTER_UNDERLAY = int!(2);

        // wave filter / ripple filter
        var/const/WAVE_SIDEWAYS = int!(1);
        var/const/WAVE_BOUNDED = int!(2);

        // outline filter
        var/const/OUTLINE_SHARP = int!(1);
        var/const/OUTLINE_SQUARE = int!(2);

        // global procs
        proc/abs(A);
        proc/addtext(Arg1, Arg2/*, ...*/);
        proc/alert(Usr/*=usr*/,Message,Title,Button1/*="Ok"*/,Button2,Button3);
        proc/animate(Object, time, loop, easing, flags, // +2 forms
            // these kwargs
            alpha, color, infra_luminosity, layer, maptext_width, maptext_height,
            maptext_x, maptext_y, luminosity, pixel_x, pixel_y, pixel_w, pixel_z,
            transform, dir, icon, icon_state, invisibility, maptext, suffix, appearance,
            dir, radius,
            // filters only
            size, x, y, offset, flags, repeat);
        proc/arccos(X);
        proc/arcsin(X);
        proc/arglist(List);  // special form
        proc/ascii2text(N);
        proc/block(Start,End);
        proc/bounds(Ref/*=src*/, Dist/*=0*/);  // +2 forms
        proc/bounds_dist(Ref, Target);
        proc/browse(Body,Options);
        proc/browse_rsc(File,FileName);
        proc/ckey(Key);
        proc/ckeyEx(Text);
        proc/cmptext(T1,T2/*,...*/);
        proc/cmptextEx(T1,T2/*,...*/);
        proc/copytext(T,Start/*=1*/,End/*=0*/);
        proc/cos(X);
        proc/fcopy(Src,Dst);
        proc/fcopy_rsc(File);
        proc/fdel(File);
        proc/fexists(File);
        proc/file(Path);
        proc/file2text(File);
        proc/filter(type,
            // see ast.rs for VALID_FILTER_TYPES
            size,
            color,
            x, y,
            offset,
            flags,
            border,
            render_source,
            icon,
            space,
            transform,
            blend_mode,
            density,
            threshold,
            factor,
            repeat,
            radius,
            falloff
        );
        proc/findlasttext(Haystack,Needle,Start=0,End=1);
        proc/findlasttextEx(Haystack,Needle,Start=0,End=1);
        proc/findtext(Haystack,Needle,Start=1,End=0);
        proc/findtextEx(Haystack,Needle,Start=1,End=0);
        proc/flick(Icon,Object);
        proc/flist(Path);
        proc/ftp(File,Name);
        proc/get_dir(Loc1,Loc2);
        proc/get_dist(Loc1,Loc2);
        proc/get_step(Ref,Dir);
        proc/get_step_away(Ref,Trg,Max=5);
        proc/get_step_rand(Ref);
        proc/get_step_to(Ref,Trg,Min=0);
        proc/get_step_towards(Ref,Trg);
        proc/hascall(Object,ProcName);
        proc/hearers(Depth=world.view,Center=usr);
        proc/html_decode(HtmlText);
        proc/html_encode(PlainText);
        proc/icon(icon,icon_state,dir,frame,moving);  // SNA
        proc/icon_states(Icon, mode=0);
        proc/image(icon,loc,icon_state,layer,dir,pixel_x,pixel_y);  // SNA
        proc/initial(Var);  // special form
        proc/input(Usr=usr,Message,Title,Default)/*as Type in List*/;  // special form
        proc/isarea(Loc1, Loc2/*,...*/);
        proc/isfile(File);
        proc/isicon(Icon);
        proc/isloc(Loc1, Loc2/*,...*/);
        proc/ismob(Loc1, Loc2/*,...*/);
        proc/isnull(Val);
        proc/isnum(Val);
        proc/isobj(Loc1, Loc2/*,...*/);
        proc/ispath(Val,Type); // +1 form
        proc/issaved(Var);  // special form? FALSE for global, const, tmp
        proc/istext(Val);
        proc/isturf(Loc1, Loc2/*,...*/);
        proc/istype(Val,Type);
        proc/jointext(List,Glue,Start=1,End=0);
        proc/json_decode(JSON);
        proc/json_encode(Value);
        proc/length(E);
        proc/lentext(T);  // deprecated
        proc/link(url);
        proc/list(A,B,C/*,...*/);  // +1 form
        proc/list2params(List);
        proc/locate(Type)/*in Container*/;  // +3 forms
        proc/log(X=2.718, Y);
        proc/lowertext(T);
        proc/matrix();  // +2 forms
        proc/max(A,B,C/*,...*/);
        proc/md5(T);
        proc/min(A,B,C/*,...*/);
        proc/missile(Type,Start,End);
        proc/newlist(A,B,C/*,...*/);
        proc/nonspantext(Haystack,Needles,Start=1);
        proc/num2text(N,SigFig=6, Radix); // +1 form, (N,SigFig) (N, Digits, Radix)
        proc/obounds(Ref=src, Dist=0);  // +1 form
        proc/ohearers(Depth=world.view,Center=usr);
        proc/orange(Dist,Center=usr);
        proc/output(msg, control);
        proc/oview(Dist,Center=usr);
        proc/oviewers(Depth=world.view,Center=usr);
        proc/params2list(Params);
        proc/pick(Val1,Val2/*,...*/);  // also has a special form
        proc/prob(P);
        proc/rand(L=0,H);  // +1 form
        proc/rand_seed(Seed);
        proc/range(Dist,Center=usr);
        proc/regex(pattern, flags);  // +1 form
        proc/REGEX_QUOTE(text);
        proc/REGEX_QUOTE_REPLACEMENT(text);
        proc/replacetext(Haystack,Needle,Replacement,Start=1,End=0);
        proc/replacetextEx(Haystack,Needle,Replacement,Start=1,End=0);
        proc/rgb(R,G,B,A=null);
        proc/roll(ndice=1,sides);  // +1 form
        proc/round(A,B=null);
        proc/run(File);
        proc/shell(Command);
        proc/shutdown(Addr,Natural=0);
        proc/sin(X);
        proc/sleep(Delay);
        proc/sorttext(T1,T2/*,...*/);
        proc/sorttextEx(T1,T2/*,...*/);
        proc/sound(file,repeat=0,wait,channel,volume);  // SNA
        proc/spantext(Haystack,Needles,Start=1);
        proc/splittext(Text,Delimiter,Start=1,End=0,include_delimiters=0);
        proc/sqrt(A);
        proc/startup(File,Port=0,Options/*,...*/);
        proc/stat(Name,Value);
        proc/statpanel(Panel,Name,Value);
        proc/step(Ref,Dir,Speed=0);
        proc/step_away(Ref,Trg,Max=5,Speed=0);
        proc/step_rand(Ref,Speed=0);
        proc/step_to(Ref,Trg,Min=0,Speed=0);
        proc/step_towards(Ref,Trg,Speed);
        proc/text(FormatText,Args);
        proc/text2ascii(T,pos=1);
        proc/text2file(Text,File);
        proc/text2num(T, Radix);
        proc/text2path(T);
        proc/time2text(timestamp,format);
        proc/turn(Dir,Angle);  // +2 forms
        proc/typesof(Type1,Type2/*,...*/);
        proc/uppertext(T);
        proc/url_decode(UrlText);
        proc/url_encode(PlainText, format=0);
        proc/view(Dist=5,Center=usr);
        proc/viewers(Depth=world.view,Center=usr);
        proc/walk(Ref,Dir,Lag=0,Speed=0);
        proc/walk_away(Ref,Trg,Max=5,Lag=0,Speed=0);
        proc/walk_rand(Ref,Lag=0,Speed=0);
        proc/walk_to(Ref,Trg,Min=0,Lag=0,Speed=0);
        proc/walk_towards(Ref,Trg,Lag=0,Speed=0);
        proc/winclone(player, window_name, clone_name);
        proc/winexists(player, control_id);
        proc/winget(player, control_id, params);
        proc/winset(player, control_id, params);
        proc/winshow(player, window, show=1);
        proc/CRASH(message);  // kind of special, but let's pretend

        // database builtin procs
        proc/_dm_db_new_query();
        proc/_dm_db_execute(db_query, sql_query, db_connection, cursor_handler, unknown);
        proc/_dm_db_next_row(db_query, item, conversions);
        proc/_dm_db_rows_affected(db_query);
        proc/_dm_db_row_count(db_query);
        proc/_dm_db_error_msg(db_query);
        proc/_dm_db_columns(db_query, db_column);
        proc/_dm_db_close(db_query);
        proc/_dm_db_new_con();
        proc/_dm_db_connect(db_con, dbi_handler, user_handler, password_handler, cursor_handler, unknown);
        proc/_dm_db_quote(db_con, _str);
        proc/_dm_db_is_connected(db_con);

        /*
        These root types expose a subset of /datum's builtins, but are not
        parented to it:

                    type  parent_type  vars  tag  New  Del  Topic  Read  Write
        world                          yes        yes  yes  yes
        list        yes   yes          yes   yes
        savefile    yes   yes          yes   yes  yes  yes
        client      yes   yes          yes   yes  yes  yes  yes

        All other root types have an implicit `parent_type = /datum`.
        */
        datum;
        datum/var/const/type;  // not editable
        datum/var/const/parent_type;  // not editable
        datum/var/tag;
        datum/var/const/list/vars;  // not editable
        datum/proc/New();
        datum/proc/Del();
        datum/proc/Read(/*savefile*/F);
        datum/proc/Topic(href, href_list);
        datum/proc/Write(/*savefile*/F);

        list;
        list/var/const/type;
        list/var/const/parent_type;
        list/var/tag;
        list/var/const/list/vars;
        list/proc/Add(Item1, Item2/*,...*/);
        list/proc/Copy(Start=1, End=0);
        list/proc/Cut(Start=1, End=0);
        list/proc/Find(Elem, Start=1, End=0);
        list/proc/Insert(Index, Item1, Item2/*,...*/);
        list/proc/Join(Glue, Start=1, End=0);
        list/proc/Remove(Item1, Item2/*,...*/);
        list/proc/Swap(Index1, Index2);
        list/var/len;

        atom/parent_type = path!(/datum);
        atom/var/alpha = int!(255);
        atom/var/tmp/appearance;  // not editable
        atom/var/appearance_flags = int!(0);
        atom/var/blend_mode = int!(0);
        atom/var/bounds;
        atom/var/color;
        atom/var/list/atom/contents;  // TODO: editable on movables only
        atom/var/density = int!(0);
        atom/var/desc;
        atom/var/dir = int!(2);
        atom/var/gender = string!("neuter");
        atom/var/icon/icon;
        atom/var/icon_state;
        atom/var/invisibility = int!(0);
        atom/var/infra_luminosity = int!(0);
        atom/var/tmp/atom/loc;  // not editable
        atom/var/layer = int!(1);
        atom/var/luminosity = int!(0);
        atom/var/maptext;  // all maptext vars not editable, but it's not obvious why
        atom/var/maptext_width = int!(32);
        atom/var/maptext_height = int!(32);
        atom/var/maptext_x = int!(0);
        atom/var/maptext_y = int!(0);
        atom/var/mouse_over_pointer = int!(0);
        atom/var/mouse_drag_pointer = int!(0);
        atom/var/mouse_drop_pointer = int!(1);
        atom/var/mouse_drop_zone = int!(0);
        atom/var/mouse_opacity = int!(1);
        atom/var/name;
        atom/var/opacity = int!(0);
        atom/var/tmp/list/overlays;  // not editable
        //atom/var/override;  // listed under /atom but docs say /image only
        atom/var/pixel_x = int!(0);
        atom/var/pixel_y = int!(0);
        atom/var/pixel_w = int!(0);
        atom/var/pixel_z = int!(0);
        atom/var/plane = int!(0);
        atom/var/suffix;
        atom/var/text;
        atom/var/matrix/transform;
        atom/var/tmp/list/underlays;  // not editable
        atom/var/tmp/list/verbs;  // not editable
        atom/var/tmp/x;  // not editable
        atom/var/tmp/y;  // not editable
        atom/var/tmp/z;  // not editable
        atom/var/list/filters;
        atom/proc/Click(location, control, params);
        atom/proc/Cross(/*atom/movable*/O);
        atom/proc/Crossed(/*atom/movable*/O);
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
        atom/proc/Uncross(/*atom/movable*/O);
        atom/proc/Uncrossed(/*atom/movable*/O);

        atom/movable;
        atom/movable/var/animate_movement = int!(1);
        atom/movable/var/bound_x = int!(0);
        atom/movable/var/bound_y = int!(0);
        atom/movable/var/bound_width = int!(32);
        atom/movable/var/bound_height = int!(32);
        atom/movable/var/tmp/list/locs;  // not editable
        atom/movable/var/screen_loc;
        atom/movable/var/glide_size = int!(0);
        atom/movable/var/step_size = int!(32);
        atom/movable/var/step_x = int!(0);
        atom/movable/var/step_y = int!(0);
        atom/movable/var/list/vis_contents;
        atom/movable/var/tmp/list/vis_locs;
        atom/movable/proc/Bump(/*atom*/Obstacle);
        atom/movable/proc/Move(NewLoc, Dir/*=0*/, step_x/*=0*/, step_y/*=0*/);

        area/parent_type = path!(/atom);
        area/layer = int!(1);
        area/luminosity = int!(1);

        turf/parent_type = path!(/atom);
        turf/layer = int!(2);
        turf/var/list/vis_contents;
        turf/var/tmp/list/vis_locs;

        obj/parent_type = path!(/atom/movable);
        obj/layer = int!(3);

        mob/parent_type = path!(/atom/movable);
        mob/layer = int!(4);
        mob/var/tmp/ckey;  // not editable, use key instead
        mob/var/tmp/client/client;  // not editable
        mob/var/list/group;  // not editable, but it's not obvious why
        mob/var/key;
        mob/var/see_infrared;
        mob/var/see_invisible;
        mob/var/see_in_dark;
        mob/var/sight;
        mob/proc/Login();
        mob/proc/Logout();

        world;
        world/var/const/list/vars;
        world/proc/New();
        world/proc/Del();
        world/proc/Topic(T, Addr, Master, Keys);
        var/static/world/world;
        world/var/address;
        world/var/area/area = path!(/area);
        world/var/byond_build = int!(DM_BUILD);
        world/var/byond_version = int!(DM_VERSION);
        world/var/cache_lifespan = int!(30);
        world/var/list/atom/contents;
        world/var/cpu;
        world/var/executor;
        world/var/fps = int!(10);
        world/var/game_state = int!(0);
        world/var/host;
        world/var/hub;
        world/var/hub_password;
        world/var/icon_size = int!(32);
        world/var/internet_address;
        world/var/log;
        world/var/loop_checks = int!(1);
        world/var/map_format = int!(0); // TOPDOWN_MAP
        world/var/maxx;
        world/var/maxy;
        world/var/maxz;
        world/var/mob/mob = path!(/mob);
        world/var/name = string!("byond");
        world/var/params;
        world/var/port;
        world/var/realtime;
        world/var/reachable;
        world/var/sleep_offline = int!(0);
        world/var/status;
        world/var/system_type;
        world/var/tick_lag = int!(1);
        world/var/tick_usage;
        world/var/turf/turf = path!(/turf);
        world/var/time;
        world/var/timeofday;
        world/var/url;
        world/var/version = int!(0);
        world/var/view = int!(5);
        world/var/visibility = int!(1);
        world/proc/AddCredits(player, credits, note);
        world/proc/ClearMedal(medal, player);
        world/proc/Export(Addr, File, Persist, Clients);
        world/proc/GetConfig(config_set, param);
        world/proc/GetCredits(player);
        world/proc/GetMedal(medal, player);
        world/proc/GetScores(key, fields, count, skip);
        world/proc/Import();
        world/proc/IsBanned(key, address, computer_id, type);
        world/proc/IsSubscribed(player);
        world/proc/OpenPort(port);
        world/proc/PayCredits(player, credits, note);
        world/proc/Profile(command, format);
        world/proc/Reboot(reason);
        world/proc/Repop();
        world/proc/SetConfig(config_set, param, value);
        world/proc/SetMedal(medal, player);
        world/proc/SetScores(key, fields);
        world/proc/Error(exception);

        client;
        client/var/const/type;
        client/var/const/parent_type;
        client/var/tag;
        client/var/const/list/vars;
        client/proc/New();
        client/proc/Del();
        client/proc/Topic(href, href_list, hsrc);
        client/var/address;
        client/var/authenticate;
        client/var/bounds;
        client/var/bound_x;
        client/var/bound_y;
        client/var/bound_width;
        client/var/bound_height;
        client/var/byond_version;
        client/var/byond_build;
        client/var/CGI;
        client/var/ckey;
        client/var/color;
        client/var/command_text;
        client/var/connection;
        client/var/control_freak = int!(0);
        client/var/computer_id;
        client/var/default_verb_category = string!("Commands");
        client/var/dir = int!(1);  // NORTH
        client/var/edge_limit;
        client/var/eye;
        client/var/fps = int!(0);
        client/var/gender;
        client/var/glide_size = int!(0);
        client/var/list/images;
        client/var/inactivity;
        client/var/key;
        client/var/lazy_eye;
        client/var/mob/mob;
        client/var/mouse_pointer_icon;
        client/var/perspective = int!(0);  // MOB_PERSPECTIVE
        client/var/pixel_x = int!(0);
        client/var/pixel_y = int!(0);
        client/var/pixel_w = int!(0);
        client/var/pixel_z = int!(0);
        client/var/preload_rsc = int!(1);
        client/var/list/screen;
        client/var/script;
        client/var/show_map = int!(1);
        client/var/show_popup_menus = int!(1);
        client/var/show_verb_panel = int!(1);
        client/var/statobj;
        client/var/statpanel;
        client/var/tick_lag = int!(0);
        client/var/list/verbs;
        client/var/view;
        client/var/virtual_eye;
        client/proc/AllowUpload(filename, filelength);
        client/proc/Center();
        client/proc/CheckPassport(passport_identifier);
        client/proc/Click(object, location, control, params);
        client/proc/Command(command);
        client/proc/DblClick(object, location, control, params);
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
        client/proc/North();
        client/proc/Northeast();
        client/proc/Northwest();
        client/proc/SendPage(msg, recipient, options);
        client/proc/South();
        client/proc/Southeast();
        client/proc/Southwest();
        client/proc/Stat();
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
        // only used by client.SoundQuery() for now:
        sound/var/offset = int!(0);
        sound/var/len = int!(0);
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

        database/query/proc/Add(text, item1, item2 /*...*/);
        database/query/proc/Close();
        database/query/proc/Columns(column);
        database/query/proc/Error();
        database/query/proc/ErrorMsg();
        database/query/proc/Execute(database);
        database/query/proc/GetColumn(column);
        database/query/proc/GetRowData();
        database/query/proc/New(text, item1, item2 /*...*/);
        database/query/proc/NextRow();
        database/query/proc/Reset();
        database/query/proc/RowsAffected();

        image;
        image/var/alpha;
        image/var/appearance;
        image/var/appearance_flags;
        image/var/blend_mode;
        image/var/color;
        image/var/desc;
        image/var/icon/icon;
        image/var/icon_state;
        image/var/text;
        image/var/dir;
        image/var/list/underlays;
        image/var/list/overlays;
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
        image/var/render_source;
        image/var/render_target;
        image/var/x;
        image/var/y;
        image/var/z;
        image/var/override;
        image/var/matrix/transform;
        image/var/list/vis_contents;
        image/var/list/filters;
        // undocumented /image vars
        image/var/animate_movement;
        image/var/density;
        image/var/gender;
        image/var/invisibility;
        image/var/luminosity;
        image/var/mouse_drag_pointer;
        image/var/mouse_drop_pointer;
        image/var/mouse_drop_zone;
        image/var/mouse_opacity;
        image/var/mouse_over_pointer;
        image/var/name;
        image/var/opacity;
        image/var/screen_loc;
        image/var/suffix;
        image/var/verbs;
        image/var/vis_flags;

        image/New(icon, loc, icon_state, layer, dir);

        // The BYOND reference states:
        //
        // > The /mutable_appearance datum is technically a descendant of
        // > /image, but this is only for convenience, and should not be relied
        // > on for any other purpose as it is subject to change in future
        // > versions.
        //
        // It then lists which vars are actually documented to exist on mutable
        // appearances. In order to be compatible with some unusual usage of
        // images and mutable appearances in the wild, SpacemanDMM reproduces
        // the reality of the DM compiler in this case rather than what the
        // documentation reports.
        mutable_appearance/parent_type = path!(/image);

        savefile;
        savefile/var/const/type;
        savefile/var/const/parent_type;
        savefile/var/tag;
        savefile/var/const/list/vars;
        savefile/proc/New(filename, timeout);
        savefile/proc/Del();
        savefile/var/cd;
        savefile/var/list/dir;
        savefile/var/eof;
        savefile/var/name;
        savefile/proc/ExportText(/* path=cd, file */);
        savefile/proc/Flush();
        savefile/proc/ImportText(/* path=cd, file */);
        savefile/proc/Lock(timeout);
        savefile/proc/Unlock();

        // 513 stuff
        proc/arctan(A,B);
        proc/clamp(NumberOrList,Low,High);
        proc/islist(List);
        proc/ismovable(Loc1, Loc2/*,...*/);
        proc/sha1(StringOrFile);
        proc/tan(A);

        // text procs
        proc/length_char(E);
        proc/text2ascii_char(T,pos=1);
        proc/copytext_char(T,Start/*=1*/,End/*=0*/);
        proc/findtext_char(Haystack,Needle,Start=1,End=0);
        proc/findtextEx_char(Haystack,Needle,Start=1,End=0);
        proc/findlasttext_char(Haystack,Needle,Start=0,End=1);
        proc/findlasttextEx_char(Haystack,Needle,Start=0,End=1);
        proc/replacetext_char(Haystack,Needle,Replacement,Start=1,End=0);
        proc/replacetextEx_char(Haystack,Needle,Replacement,Start=1,End=0);
        proc/spantext_char(Haystack,Needles,Start=1);
        proc/nonspantext_char(Haystack,Needles,Start=1);
        proc/splittext_char(Text,Delimiter,Start=1,End=0,include_delimiters=0);

        atom/var/render_target;
        atom/var/render_source;
        atom/var/vis_flags;

        client/proc/MeasureText(text, style, width/*=0*/);
        client/proc/SoundQuery();

        regex/proc/Find_char(text, start, end);
        regex/proc/Replace_char(text, rep, start, end);
    };

    Ok(())
}
