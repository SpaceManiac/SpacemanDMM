//! The built-in template.

use std::path::Path;

use dm::ast::InputType;
use maud::{display, html, Markup, PreEscaped, Render, DOCTYPE};

use crate::{markdown::DocBlock, Environment, Index, IndexTree, ModuleArgs, ModuleItem, Type};

pub(crate) fn base(
    env: &Environment,
    base_href: &str,
    title: &dyn Render,
    head: &dyn Render,
    header: &dyn Render,
    content: &dyn Render,
) -> Markup {
    html! {
        (DOCTYPE)
        html lang="en" {
            head {
                meta charset="utf-8";
                @if !base_href.is_empty() {
                    base href=(base_href);
                }
                link rel="stylesheet" href="dmdoc.css";
                title {
                    (title) " - " (env.world_name)
                }
                (head)
            }
            body {
                header {
                    a href="index.html" { (env.world_name) } " - "
                    a href="index.html#modules" { "Modules" } " - "
                    a href="index.html#types" { "Types" }
                    (header)
                }
                main {
                    (content)
                }
                footer {
                    (env.filename)
                    @if !env.git.revision.is_empty() {
                        " "
                        @if !env.git.web_url.is_empty() {
                            a href=(format!("{}/tree/{}", env.git.web_url, env.git.revision)) {
                                (env.git.revision[..7])
                            }
                        } @else {
                            (env.git.revision)
                        }
                        @if !env.git.branch.is_empty() {
                            " ("
                            (env.git.branch)
                            @if !env.git.remote_branch.is_empty() && env.git.remote_branch != env.git.branch {
                                " → " (env.git.remote_branch)
                            }
                            ")"
                        }
                        " — "
                        @if !env.dmdoc.url.is_empty() {
                            a href=(env.dmdoc.url) {
                                "dmdoc " (env.dmdoc.version)
                            }
                        } @else {
                            "dmdoc " (env.dmdoc.version)
                        }
                    }
                }
            }
        }
    }
}

fn teaser(block: &DocBlock, prefix: &str) -> Markup {
    let teaser = block.teaser();
    html! {
        @if !teaser.0.is_empty() {
            (prefix)
            (teaser)
        }
    }
}

fn git_link(env: &Environment, file: &str, line: u32) -> Markup {
    let icon = html! {
        img src="git.png" width="16" height="16" title=(format!("{}{}{}", file, if line != 0 { " " } else { "" }, line));
    };
    html! {
        @if !file.is_empty() {
            " "
            @if !env.git.web_url.is_empty() && !env.git.revision.is_empty() {
                a href=(format!(
                    "{}/blob/{}/{}{}{}",
                    env.git.web_url,
                    env.git.revision,
                    file,
                    if line != 0 { "#L" } else { "" },
                    line
                )) {
                    (icon)
                }
            } @else {
                (icon)
            }
        }
    }
}

fn index_tree(elems: &[IndexTree], extra_class: &str) -> Markup {
    html! {
        ul .index-tree .(extra_class) {
            @for tree in elems {
                (index_tree_elem(tree, ""))
            }
        }
    }
}

fn index_tree_elem(tree: &IndexTree, prefix: &str) -> Markup {
    html! {
        @if tree.children.len() == 1 && tree.htmlname.is_empty() && tree.teaser.0.is_empty() {
            (index_tree_elem(&tree.children[0], &format!("{}{}/", prefix, tree.self_name)))
        } @else {
            li .(if !tree.children.is_empty() { "has-children" } else { "" }) {
                @if !prefix.is_empty() {
                    span class="no-substance" { (prefix) }
                }
                @if tree.htmlname.is_empty() {
                    span .(if tree.no_substance { "no-substance" } else { "" }) title=(tree.full_name) { (tree.self_name) }
                } @else {
                    a href=(format!("{}.html", tree.htmlname)) title=(tree.full_name) { (tree.self_name) }
                }
                @if !tree.teaser.0.is_empty() {
                    " - " (tree.teaser)
                }
                @if !tree.children.is_empty() {
                    (index_tree(&tree.children, ""))
                }
            }
        }
    }
}

fn percentage(amt: usize, total: usize) -> Markup {
    if total > 0 {
        PreEscaped(format!(", {:.1}%", (amt as f32) * 100.0 / (total as f32)))
    } else {
        Markup::default()
    }
}

pub(crate) fn dm_index(index: &Index) -> Markup {
    let Index { env, html, modules, types } = index;
    base(
        env,
        "",
        &display("Index"),
        &html! {
            (maud::PreEscaped("\n<!-- produced by: \n"))
            (env.dmdoc.build_info)
            (maud::PreEscaped("\n-->\n"))
            script src="dmdoc.js" {}
        },
        &display(""),
        &html! {
            h1 { (env.title) }
            @if let Some(html) = html { (html) }

            @if !modules.is_empty() {
                h3 id="modules" {
                    "Modules "
                    aside {
                        "("
                        (env.coverage.modules) " modules, "
                        (env.coverage.macros_documented)"/"(env.coverage.macros_all)" macros"
                        (percentage(env.coverage.macros_documented, env.coverage.macros_all))
                        ")"
                    }
                }
                (index_tree(modules, "modules"))
            }

            @if !types.is_empty() {
                h3 id="types" {
                    "Types "
                    aside {
                        "("
                        (env.coverage.types_detailed) " detailed/"
                        (env.coverage.types_documented) " documented/"
                        (env.coverage.types_all) " total"
                        (percentage(env.coverage.types_documented, env.coverage.types_all))
                        ")"
                    }
                }
                (index_tree(types, ""))
            }
        }
    )
}

pub(crate) fn dm_module(module: &ModuleArgs) -> Markup {
    let ModuleArgs { env, base_href, details } = *module;
    base(
        env,
        base_href,
        &display(&details.orig_filename),
        &display(""),
        &html! {
            @if !details.defines.is_empty() {
                " — "
                a href=(format!("{}.html#define", details.htmlname)) { "Define Details" }
            }
        },
        &html! {
            h1 {
                @if let Some(ref name) = details.name {
                    (name) " "
                    aside {
                        (details.orig_filename)
                    }
                } @else {
                    (details.orig_filename)
                }
                (git_link(env, &details.orig_filename, 0))
            }

            table class="summary" cellspacing="0" {
                @for item in details.items.iter() {
                    @match item {
                        ModuleItem::Docs(docs) => {
                            tr {
                                td colspan="2" {
                                    (docs)
                                }
                            }
                        },
                        ModuleItem::Define { name, teaser } => {
                            tr {
                                th {
                                    a href=(format!("{}.html#define/{}", details.htmlname, name)) { (name) }
                                }
                                td {
                                    (teaser)
                                }
                            }
                        },
                        ModuleItem::Type { path, teaser, substance } => {
                            tr {
                                th {
                                    @if *substance {
                                        a href=(format!("{}.html", &path[1..])) {
                                            (path)
                                        }
                                    } @else {
                                        (env.linkify_type_str(path))
                                    }
                                }
                                td {
                                    (teaser)
                                }
                            }
                        },
                        ModuleItem::GlobalProc { name, teaser } => {
                            tr {
                                th {
                                    "/proc/"
                                    a href=(format!("global.html#proc/{}", name)) {
                                        (name)
                                    }
                                }
                                td {
                                    (teaser)
                                }
                            }
                        },
                        ModuleItem::GlobalVar { name, teaser } => {
                            tr {
                                th {
                                    "/var/"
                                    a href=(format!("global.html#var/{}", name)) {
                                        (name)
                                    }
                                }
                                td {
                                    (teaser)
                                }
                            }
                        },
                        ModuleItem::DocComment { .. } => {}
                    }
                }
            }

            @if !details.defines.is_empty() {
                h2 id="define" { "Define Details" }
                @for (name, define) in details.defines.iter() {
                    h3 id=(format!("define/{}", name)) {
                        aside class="declaration" {
                            "#define "
                        }
                        (name)
                        @if define.has_params {
                            aside {
                                "("
                                @for (i, param) in define.params.iter().enumerate() {
                                    @if i > 0 {
                                        ", "
                                    }
                                    (param)
                                }
                                @if define.is_variadic {
                                    " ..."
                                }
                                ")"
                            }
                        }
                        (git_link(env, &details.orig_filename, define.line))
                    }
                    (define.docs.html)
                }
            }
        }
    )
}

pub(crate) fn dm_type(ty: &Type) -> Markup {
    let Type { env, base_href, path, details } = *ty;
    base(
        env,
        base_href,
        &display(path),
        &display(""),
        &html! {
            @if !details.vars.is_empty() {
                " — "
                a href=(format!("{}.html#var", details.htmlname)) { "Var Details" }
            }
            @if !details.procs.is_empty() {
                @if !details.vars.is_empty() { " - " } @else { " — " }
                a href=(format!("{}.html#proc", details.htmlname)) { "Proc Details" }
            }
        },
        &html! {
            h1 {
                @if path == "global" {
                    "(global)"
                } @else if !details.name.is_empty() {
                    (details.name)
                    " "
                    aside {
                        (env.linkify_type_str(path))
                    }
                } @else {
                    (env.linkify_type_str(path))
                }
                @if let Some(parent_type) = details.parent_type {
                    aside {
                        " inherits "
                        (env.linkify_type_str(parent_type))
                    }
                }
                (git_link(env, &details.file.to_string_lossy(), details.line))
            }
            @if let Some(ref docs) = details.docs {
                (docs.html)
            }

            @if !details.vars.is_empty() || !details.procs.is_empty() {
                table class="summary" cellspacing="0" {
                    @if !details.vars.is_empty() {
                        tr { td colspan="2" { h2 { "Vars" } } }
                        @for (name, var) in details.vars.iter() {
                            tr {
                                th { a href=(format!("{}.html#var/{}", details.htmlname, name)) { (name) } }
                                td { (teaser(&var.docs, "")) }
                            }
                        }
                    }
                    @if !details.procs.is_empty() {
                        tr { td colspan="2" { h2 { "Procs" } } }
                        @for (name, proc) in details.procs.iter() {
                            tr {
                                th { a href=(format!("{}.html#proc/{}", details.htmlname, name)) { (name) } }
                                td { (teaser(&proc.docs, "")) }
                            }
                        }
                    }
                }
            }

            @if !details.vars.is_empty() {
                h2 id="var" { "Var Details" }
                @for (name, var) in details.vars.iter() {
                    h3 id=(format!("var/{}", name)) {
                        @if !var.decl.is_empty() {
                            aside class="declaration" { (var.decl) " " }
                        } @else if let Some(ref parent) = var.parent {
                            aside class="parent" {
                                a title=(format!("/{}", parent)) href=(format!("{}.html#var/{}", parent, name)) {
                                    "\u{2191}"  // &uarr;
                                }
                            }
                        }
                        (name)
                        @if let Some(ref ty) = var.type_ {
                            " "
                            aside {
                                "\u{2013} "  // &ndash;
                                @if ty.is_static { "/static" }
                                @if ty.is_const { "/const" }
                                @if ty.is_tmp { "/tmp" }
                                @if ty.is_final { "/final" }
                                (env.linkify_type_array(ty.path))
                            }
                        }
                        (git_link(env, &var.file.to_string_lossy(), var.line))
                    }
                    (var.docs.html)
                }
            }

            @if !details.procs.is_empty() {
                h2 id="proc" { "Proc Details" }
                @for (name, proc) in details.procs.iter() {
                    h3 id=(format!("proc/{}", name)) {
                        @if !proc.decl.is_empty() {
                            aside class="declaration" { (proc.decl) " " }
                        } @else if let Some(ref parent) = proc.parent {
                            aside class="parent" {
                                a title=(format!("/{}", parent)) href=(format!("{}.html#proc/{}", parent, name)) {
                                    "\u{2191}"  // &uarr;
                                }
                            }
                        }
                        (name)
                        aside {
                            "("
                            @for (i, param) in proc.params.iter().enumerate() {
                                @if i > 0 { ", " }
                                @if !param.type_path.is_empty() {
                                    (env.linkify_type_str(&param.type_path))
                                    "/"
                                }
                                (param.name)
                                @if let Some(input_type) = param.input_type {
                                    @if !input_type.is_empty() {
                                        i { " as " }
                                        (render_input_type(env, input_type))
                                    }
                                }
                            }
                            ") "
                            (git_link(env, &proc.file.to_string_lossy(), proc.line))
                        }
                    }
                    (proc.docs.html)
                }
            }
        }
    )
}

pub fn render_input_type(env: &Environment, input_type: InputType) -> Markup {
    html! {
        @for (i, &name) in matching_names(input_type).iter().enumerate() {
            @if i > 0 { " | " }
            @match name {
                "mob" => (linkify_input_type(env, "mob", "/mob")),
                "obj" => (linkify_input_type(env, "obj", "/obj")),
                "turf" => (linkify_input_type(env, "turf", "/turf")),
                "area" => (linkify_input_type(env, "area", "/area")),
                //"icon" => (linkify_input_type(env, "icon", "/icon")),
                //"sound" => (linkify_input_type(env, "sound", "/sound")),
                "movable" => (linkify_input_type(env, "movable", "/atom/movable")),
                "atom" => (linkify_input_type(env, "atom", "/atom")),
                "list" => (linkify_input_type(env, "list", "/list")),
                _ => (name),
            }
        }
    }
}

fn linkify_input_type(env: &Environment, show: &str, typepath: &str) -> Markup {
    if env.all_type_names.contains(typepath) {
        html! {
            a href=(format!("{}.html", &typepath[1..])) { (show) }
        }
    } else {
        html! { (show) }
    }
}

fn matching_names(mut input_type: InputType) -> Vec<&'static str> {
    let mut result = Vec::with_capacity(input_type.bits().count_ones() as usize);
    for &(name, value) in InputType::ENTRIES.iter().rev() {
        if input_type.contains(value) {
            input_type.remove(value);
            result.push(name);
        }
    }
    result.reverse();
    result
}

pub fn save_resources(output_path: &Path) -> std::io::Result<()> {
    #[cfg(debug_assertions)]
    macro_rules! resources {
        ($($name:expr,)*) => {
            let env = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/src/template"));
            $(
                std::fs::copy(&env.join($name), &output_path.join($name))?;
            )*
        }
    }

    #[cfg(not(debug_assertions))]
    macro_rules! resources {
        ($($name:expr,)*) => {{
            use std::io::Write;
            $(
                crate::create(&output_path.join($name))?.write_all(include_bytes!(concat!("template/", $name)))?;
            )*
        }}
    }

    resources! {
        "dmdoc.css",
        "dmdoc.js",
        "git.png",
    }

    Ok(())
}
