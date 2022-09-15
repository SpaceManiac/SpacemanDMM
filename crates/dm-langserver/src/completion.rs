//! Supporting functions for completion and go-to-definition.

use std::collections::HashSet;

use lsp_types::*;

use dm::annotation::Annotation;
use dm::ast::PathOp;
use dm::objtree::{ProcValue, TypeProc, TypeRef, TypeVar};

use crate::symbol_search::contains;
use crate::{is_constructor_name, Engine, Span};

use ahash::RandomState;

static PROC_KEYWORDS: &[&str] = &[
    // Implicit variables
    "args", "global", "src", "usr", // Term
    "null", "as", ".", "..", "new",
    // "list", "input", "locate", "pick" appear in builtin proc list
    "call", // Statement
    "return", "throw", "while", "do", "if", "else", "for", "var", "set", "spawn", "switch", "try",
    "catch", "continue", "break", "goto", "del",
    // "CRASH" appears in builtin proc list
];

fn item_var(ty: TypeRef, name: &str, var: &TypeVar) -> CompletionItem {
    let mut detail = format!("on {}", ty.pretty_path());
    if let Some(ref decl) = var.declaration {
        if decl.var_type.flags.is_const() {
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
        kind: Some(CompletionItemKind::FIELD),
        detail: Some(detail),
        documentation: item_documentation(&var.value.docs),
        ..Default::default()
    }
}

fn item_proc(ty: TypeRef, name: &str, proc: &TypeProc) -> CompletionItem {
    CompletionItem {
        label: name.to_owned(),
        kind: Some(if ty.is_root() {
            CompletionItemKind::FUNCTION
        } else if is_constructor_name(name) {
            CompletionItemKind::CONSTRUCTOR
        } else {
            CompletionItemKind::METHOD
        }),
        detail: Some(format!("on {}", ty.pretty_path())),
        documentation: item_documentation(&proc.main_value().docs),
        ..Default::default()
    }
}

fn item_documentation(docs: &dm::docs::DocCollection) -> Option<Documentation> {
    if docs.is_empty() {
        return None;
    }

    Some(Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: docs.text(),
    }))
}

fn items_ty<'a>(
    results: &mut Vec<CompletionItem>,
    skip: &mut HashSet<(&str, &'a String), RandomState>,
    ty: TypeRef<'a>,
    query: &str,
) {
    // type variables
    for (name, var) in ty.get().vars.iter() {
        if !skip.insert(("var", name)) {
            continue;
        }
        if contains(name, query) {
            results.push(item_var(ty, name, var));
        }
    }

    // procs
    for (name, proc) in ty.get().procs.iter() {
        if !skip.insert(("proc", name)) {
            continue;
        }
        if contains(name, query) {
            results.push(CompletionItem {
                insert_text: Some(name.to_owned()),
                ..item_proc(ty, name, proc)
            });
        }
    }
}

pub fn combine_tree_path<'a, I>(
    iter: &I,
    mut absolute: bool,
    mut parts: &'a [String],
) -> impl Iterator<Item = &'a str>
where
    I: Iterator<Item = (Span, &'a Annotation)> + Clone,
{
    // cut off the part of the path we haven't selected
    if_annotation! { Annotation::InSequence(idx) in iter; {
        parts = &parts[..::std::cmp::min(idx+1, parts.len())];
    }}
    // if we're on the right side of a 'var/', start the lookup there
    if let Some(i) = parts.iter().position(|x| x == "var") {
        parts = &parts[i + 1..];
        absolute = true;
    }
    // if we're on the right side of a 'list/', start the lookup there
    while let Some((first, rest)) = parts.split_first() {
        if first == "list" {
            parts = rest;
        } else {
            break;
        }
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

impl<'a> Engine<'a> {
    pub fn follow_type_path<'b, I>(
        &'b self,
        iter: &I,
        mut parts: &'b [(PathOp, String)],
    ) -> Option<TypePathResult<'b>>
    where
        I: Iterator<Item = (Span, &'a Annotation)> + Clone,
    {
        // cut off the part of the path we haven't selected
        if_annotation! { Annotation::InSequence(idx) in iter; {
            parts = &parts[..::std::cmp::min(idx+1, parts.len())];
        }}
        // if we're on the right side of a 'list/', start the lookup there
        match parts.split_first() {
            Some(((PathOp::Slash, kwd), rest)) if kwd == "list" && !rest.is_empty() => parts = rest,
            _ => {}
        }

        // use the first path op to select the starting type of the lookup
        if parts.is_empty() {
            return Some(TypePathResult {
                ty: self.objtree.root(),
                decl: None,
                proc: None,
            });
        }
        let mut ty = match parts[0].0 {
            PathOp::Colon => return None, // never finds anything, apparently?
            PathOp::Slash => self.objtree.root(),
            PathOp::Dot => match self.find_type_context(iter) {
                (Some(base), _) => base,
                (None, _) => self.objtree.root(),
            },
        };

        // follow the path ops until we hit 'proc' or 'verb'
        let mut iter = parts.iter();
        let mut decl = None;
        for &(op, ref name) in iter.by_ref() {
            if name == "proc" {
                decl = Some("proc");
                break;
            } else if name == "verb" {
                decl = Some("verb");
                break;
            }
            if let Some(next) = ty.navigate(op, name) {
                ty = next;
            } else {
                break;
            }
        }
        let mut proc = None;
        if decl.is_some() {
            if let Some((_, proc_name)) = iter.next() {
                // '/datum/proc/proc_name'
                if let Some(proc_ref) = ty.get_proc(proc_name) {
                    proc = Some((proc_name.as_str(), proc_ref.get()));
                }
            }
            // else '/datum/proc', no results
        }
        // '/datum'
        Some(TypePathResult { ty, decl, proc })
    }

    pub fn tree_completions(
        &self,
        results: &mut Vec<CompletionItem>,
        exact: bool,
        ty: TypeRef,
        query: &str,
    ) {
        // path keywords
        for &name in ["proc", "var", "verb"].iter() {
            if contains(name, query) {
                results.push(CompletionItem {
                    label: name.to_owned(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                })
            }
        }

        if exact {
            // child types
            for child in ty.children() {
                if contains(child.name(), query) {
                    results.push(CompletionItem {
                        label: child.name().to_owned(),
                        kind: Some(CompletionItemKind::CLASS),
                        documentation: item_documentation(&child.docs),
                        ..Default::default()
                    });
                }
            }
        }

        let mut next = Some(ty).filter(|ty| !ty.is_root());
        let mut skip = HashSet::with_hasher(RandomState::default());
        while let Some(ty) = next {
            // override a parent's var
            for (name, var) in ty.get().vars.iter() {
                if !skip.insert(("var", name)) {
                    continue;
                }
                if contains(name, query) {
                    results.push(CompletionItem {
                        insert_text: Some(format!("{} = ", name)),
                        ..item_var(ty, name, var)
                    });
                }
            }

            // override a parent's proc
            for (name, proc) in ty.get().procs.iter() {
                if !skip.insert(("proc", name)) {
                    continue;
                }
                if contains(name, query) {
                    use std::fmt::Write;

                    let mut completion = format!("{}(", name);
                    let mut sep = "";
                    for param in proc.main_value().parameters.iter() {
                        for each in param.var_type.type_path.iter() {
                            let _ = write!(completion, "{}{}", sep, each);
                            sep = "/";
                        }
                        let _ = write!(completion, "{}{}", sep, param.name);
                        sep = ", ";
                    }
                    let _ = write!(completion, ")\n\t. = ..()\n\t");

                    results.push(CompletionItem {
                        insert_text: Some(completion),
                        ..item_proc(ty, name, proc)
                    });
                }
            }
            next = ty.parent_type();
        }
    }

    pub fn path_completions<'b, I>(
        &'b self,
        results: &mut Vec<CompletionItem>,
        iter: &I,
        parts: &'b [(PathOp, String)],
        _last_op: PathOp,
        query: &str,
    ) where
        I: Iterator<Item = (Span, &'b Annotation)> + Clone,
    {
        // TODO: take last_op into account
        match self.follow_type_path(iter, parts) {
            // '/datum/<complete types>'
            Some(TypePathResult {
                ty,
                decl: None,
                proc: None,
            }) => {
                // path keywords
                for &name in ["proc", "verb"].iter() {
                    if contains(name, query) {
                        results.push(CompletionItem {
                            label: name.to_owned(),
                            kind: Some(CompletionItemKind::KEYWORD),
                            ..Default::default()
                        })
                    }
                }

                // child types
                for child in ty.children() {
                    if contains(child.name(), query) {
                        results.push(CompletionItem {
                            label: child.name().to_owned(),
                            kind: Some(CompletionItemKind::CLASS),
                            documentation: item_documentation(&child.docs),
                            ..Default::default()
                        });
                    }
                }
            }
            // '/datum/proc/<complete procs>'
            // TODO: take the path op into acocunt (`/proc` vs `.proc`)
            Some(TypePathResult {
                ty,
                decl: Some(decl),
                proc: None,
            }) => {
                let mut next = Some(ty);
                let mut skip = HashSet::with_hasher(RandomState::default());
                while let Some(ty) = next {
                    // reference a declared proc
                    for (name, proc) in ty.get().procs.iter() {
                        if !skip.insert(("proc", name)) {
                            continue;
                        }
                        // declarations only
                        let proc_decl = match proc.declaration.as_ref() {
                            Some(decl) => decl,
                            None => continue,
                        };
                        if proc_decl.kind.is_verb() != (decl == "verb") {
                            continue;
                        }
                        if contains(name, query) {
                            results.push(item_proc(ty, name, proc));
                        }
                    }
                    next = ty.parent_type_without_root();
                }
            }
            _ => {}
        }
    }

    pub fn unscoped_completions<'b, I>(
        &'b self,
        results: &mut Vec<CompletionItem>,
        iter: &I,
        query: &str,
    ) where
        I: Iterator<Item = (Span, &'b Annotation)> + Clone,
    {
        let (ty, proc_name) = self.find_type_context(iter);

        // implicit proc vars
        if proc_name.is_some() {
            for &name in PROC_KEYWORDS.iter() {
                if contains(name, query) {
                    results.push(CompletionItem {
                        label: name.to_owned(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    });
                }
            }
        }

        // local variables
        for (_, annotation) in iter.clone() {
            if let Annotation::LocalVarScope(_var_type, name) = annotation {
                if contains(name, query) {
                    results.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        detail: Some("(local)".to_owned()),
                        ..Default::default()
                    });
                }
            }
        }

        // proc parameters
        let ty = ty.unwrap_or_else(|| self.objtree.root());
        if let Some((proc_name, idx)) = proc_name {
            if let Some(proc) = ty.get().procs.get(proc_name) {
                if let Some(value) = proc.value.get(idx) {
                    for param in value.parameters.iter() {
                        if contains(&param.name, query) {
                            results.push(CompletionItem {
                                label: param.name.clone(),
                                kind: Some(CompletionItemKind::VARIABLE),
                                detail: Some("(parameter)".to_owned()),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        }

        // macros
        if let Some(ref defines) = self.defines {
            // TODO: verify that the macro is in scope at the location
            for (_, &(ref name, ref define)) in defines.iter() {
                if contains(name, query) {
                    results.push(CompletionItem {
                        label: name.to_owned(),
                        kind: Some(CompletionItemKind::CONSTANT),
                        detail: Some(define.display_with_name(name).to_string()),
                        documentation: item_documentation(define.docs()),
                        ..Default::default()
                    });
                }
            }
        }

        // fields
        let mut next = Some(ty);
        let mut skip = HashSet::with_hasher(RandomState::default());
        while let Some(ty) = next {
            items_ty(results, &mut skip, ty, query);
            next = ty.parent_type();
        }
    }

    pub fn scoped_completions<'b, I>(
        &'b self,
        results: &mut Vec<CompletionItem>,
        iter: &I,
        priors: &[String],
        query: &str,
    ) where
        I: Iterator<Item = (Span, &'b Annotation)> + Clone,
    {
        let mut next = self.find_scoped_type(iter, priors);
        let mut skip = HashSet::with_hasher(RandomState::default());
        while let Some(ty) = next {
            items_ty(results, &mut skip, ty, query);
            next = ty.parent_type_without_root();
        }
    }
}

pub struct TypePathResult<'a> {
    pub ty: TypeRef<'a>,
    pub decl: Option<&'static str>,
    pub proc: Option<(&'a str, &'a ProcValue)>,
}
