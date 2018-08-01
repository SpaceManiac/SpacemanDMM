//! Supporting functions for completion and go-to-definition.

use langserver::*;

use dm::annotation::Annotation;
use dm::objtree::{TypeRef, TypeVar, TypeProc};

use {Span, is_constructor_name};
use symbol_search::starts_with;

pub fn item_var(ty: TypeRef, name: &str, var: &TypeVar) -> CompletionItem {
    let mut detail = ty.pretty_path().to_owned();
    if let Some(ref decl) = var.declaration {
        if decl.var_type.is_const {
            if let Some(ref constant) = var.value.constant {
                if ty.is_root() {
                    detail = constant.to_string();
                } else {
                    detail = format!("{} - {}", constant, detail);
                }
            }
        }
    }

    CompletionItem {
        label: name.to_owned(),
        kind: Some(CompletionItemKind::Field),
        detail: Some(detail),
        .. Default::default()
    }
}

pub fn item_proc(ty: TypeRef, name: &str, _proc: &TypeProc) -> CompletionItem {
    CompletionItem {
        label: name.to_owned(),
        kind: Some(if ty.is_root() {
            CompletionItemKind::Function
        } else if is_constructor_name(name) {
            CompletionItemKind::Constructor
        } else {
            CompletionItemKind::Method
        }),
        detail: Some(ty.pretty_path().to_owned()),
        insert_text: Some(format!("{}(", name)),
        .. Default::default()
    }
}

pub fn items_ty(results: &mut Vec<CompletionItem>, ty: TypeRef, query: &str) {
    // type variables
    for (name, var) in ty.get().vars.iter() {
        if starts_with(name, query) {
            results.push(item_var(ty, name, var));
        }
    }

    // procs
    for (name, proc) in ty.procs.iter() {
        if starts_with(name, query) {
            results.push(item_proc(ty, name, proc));
        }
    }
}

pub fn combine_tree_path<'a, I>(iter: &I, mut absolute: bool, mut parts: &'a [String]) -> impl Iterator<Item=&'a str>
    where I: Iterator<Item=(Span, &'a Annotation)> + Clone
{
    // cut off the part of the path we haven't selected
    if_annotation! { Annotation::InSequence(idx) in iter; {
        parts = &parts[..::std::cmp::min(idx+1, parts.len())];
    }}
    // if we're on the right side of a 'var/', start the lookup there
    if let Some(i) = parts.iter().position(|x| x == "var") {
        parts = &parts[i+1..];
        absolute = true;
    }
    // if we're on the right side of a 'list/', start the lookup there
    match parts.split_first() {
        Some((kwd, rest)) if kwd == "list" && !rest.is_empty() => parts = rest,
        _ => {}
    }

    let mut prefix_parts = &[][..];
    if !absolute {
        if_annotation! { Annotation::TreeBlock(parts) in iter; {
            prefix_parts = parts;
            if let Some(i) = prefix_parts.iter().position(|x| x == "var") {
                // if we're inside a 'var' block, start the lookup there
                prefix_parts = &prefix_parts[i+1..];
            }
        }}
    }

    prefix_parts.iter().chain(parts).map(|x| &**x)
}
