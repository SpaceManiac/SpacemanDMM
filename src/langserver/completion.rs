//! Supporting functions for completion and go-to-definition.

use std::collections::HashSet;

use langserver::*;

use dm::ast::PathOp;
use dm::annotation::Annotation;
use dm::objtree::{TypeRef, TypeVar, TypeProc, ProcValue};

use {Engine, Span, io, is_constructor_name, ignore_root};
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
        .. Default::default()
    }
}

pub fn items_ty<'a>(results: &mut Vec<CompletionItem>, skip: &mut HashSet<(&str, &'a String)>, ty: TypeRef<'a>, query: &str) {
    // type variables
    for (name, var) in ty.get().vars.iter() {
        if !skip.insert(("var", name)) {
            continue;
        }
        if starts_with(name, query) {
            results.push(item_var(ty, name, var));
        }
    }

    // procs
    for (name, proc) in ty.get().procs.iter() {
        if !skip.insert(("proc", name)) {
            continue;
        }
        if starts_with(name, query) {
            results.push(CompletionItem {
                insert_text: Some(format!("{}(", name)),
                .. item_proc(ty, name, proc)
            });
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

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    pub fn follow_type_path<'b, I>(&'b self, iter: &I, mut parts: &'b [(PathOp, String)]) -> Option<TypePathResult<'b>>
        where I: Iterator<Item=(Span, &'a Annotation)> + Clone
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
            return Some(TypePathResult { ty: self.objtree.root(), decl: None, proc: None });
        }
        let mut ty = match parts[0].0 {
            PathOp::Colon => return None,  // never finds anything, apparently?
            PathOp::Slash => self.objtree.root(),
            PathOp::Dot => {
                match self.find_type_context(iter) {
                    (Some(base), _) => base,
                    (None, _) => self.objtree.root(),
                }
            }
        };

        // follow the path ops until we hit 'proc' or 'verb'
        let mut iter = parts.iter();
        let mut decl = None;
        while let Some(&(op, ref name)) = iter.next() {
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
                    proc = Some((proc_name.as_str(), proc_ref));
                }
            }
            // else '/datum/proc', no results
        }
        // '/datum'
        Some(TypePathResult { ty, decl, proc })
    }

    pub fn tree_completions(&self, results: &mut Vec<CompletionItem>, exact: bool, ty: TypeRef, query: &str) {
        // path keywords
        for &name in ["proc", "var", "verb"].iter() {
            if starts_with(name, query) {
                results.push(CompletionItem {
                    label: name.to_owned(),
                    kind: Some(CompletionItemKind::Keyword),
                    .. Default::default()
                })
            }
        }

        if exact {
            // child types
            for child in ty.children() {
                if starts_with(&child.name, query) {
                    results.push(CompletionItem {
                        label: child.name.to_owned(),
                        kind: Some(CompletionItemKind::Class),
                        .. Default::default()
                    });
                }
            }
        }

        let mut next = Some(ty);
        let mut skip = HashSet::new();
        while let Some(ty) = ignore_root(next) {
            // override a parent's var
            for (name, var) in ty.get().vars.iter() {
                if !skip.insert(("var", name)) {
                    continue;
                }
                if starts_with(name, query) {
                    results.push(CompletionItem {
                        insert_text: Some(format!("{} = ", name)),
                        .. item_var(ty, name, var)
                    });
                }
            }

            // override a parent's proc
            for (name, proc) in ty.get().procs.iter() {
                if !skip.insert(("proc", name)) {
                    continue;
                }
                if starts_with(name, query) {
                    use std::fmt::Write;

                    let mut completion = format!("{}(", name);
                    let mut sep = "";
                    for param in proc.value.last().unwrap().parameters.iter() {
                        for each in param.path.iter() {
                            let _ = write!(completion, "{}{}", sep, each);
                            sep = "/";
                        }
                        let _ = write!(completion, "{}{}", sep, param.name);
                        sep = ", ";
                    }
                    let _ = write!(completion, ")\n\t. = ..()\n\t");

                    results.push(CompletionItem {
                        insert_text: Some(completion),
                        .. item_proc(ty, name, proc)
                    });
                }
            }
            next = ty.parent_type();
        }
    }

    pub fn path_completions<'b, I>(&'b self, results: &mut Vec<CompletionItem>, iter: &I, parts: &'b [(PathOp, String)], _last_op: PathOp, query: &str)
        where I: Iterator<Item=(Span, &'b Annotation)> + Clone
    {
        // TODO: take last_op into account
        match self.follow_type_path(iter, parts) {
            // '/datum/<complete types>'
            Some(TypePathResult { ty, decl: None, proc: None }) => {
                // path keywords
                for &name in ["proc", "verb"].iter() {
                    if starts_with(name, query) {
                        results.push(CompletionItem {
                            label: name.to_owned(),
                            kind: Some(CompletionItemKind::Keyword),
                            .. Default::default()
                        })
                    }
                }

                // child types
                for child in ty.children() {
                    if starts_with(&child.name, query) {
                        results.push(CompletionItem {
                            label: child.name.to_owned(),
                            kind: Some(CompletionItemKind::Class),
                            .. Default::default()
                        });
                    }
                }
            },
            // '/datum/proc/<complete procs>'
            // TODO: take the path op into acocunt (`/proc` vs `.proc`)
            Some(TypePathResult { ty, decl: Some(decl), proc: None }) => {
                let mut next = Some(ty);
                while let Some(ty) = next {
                    // reference a declared proc
                    for (name, proc) in ty.get().procs.iter() {
                        // declarations only
                        let mut proc_decl = match proc.declaration.as_ref() {
                            Some(decl) => decl,
                            None => continue
                        };
                        if proc_decl.is_verb != (decl == "verb") {
                            continue
                        }
                        if starts_with(name, query) {
                            results.push(item_proc(ty, name, proc));
                        }
                    }
                    next = ignore_root(ty.parent_type());
                }
            },
            _ => {}
        }
    }

    pub fn unscoped_completions<'b, I>(&'b self, results: &mut Vec<CompletionItem>, iter: &I, query: &str)
        where I: Iterator<Item=(Span, &'b Annotation)> + Clone
    {
        let (ty, proc_name) = self.find_type_context(iter);

        // implicit proc vars
        for &name in ["args", "global", "src", "usr"].iter() {
            if starts_with(name, query) {
                results.push(CompletionItem {
                    label: name.to_owned(),
                    kind: Some(CompletionItemKind::Keyword),
                    .. Default::default()
                });
            }
        }

        // local variables
        for (_, annotation) in iter.clone() {
            if let Annotation::LocalVarScope(_var_type, name) = annotation {
                if starts_with(name, query) {
                    results.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::Variable),
                        detail: Some("(local)".to_owned()),
                        .. Default::default()
                    });
                }
            }
        }

        // proc parameters
        let ty = ty.unwrap_or(self.objtree.root());
        if let Some((proc_name, idx)) = proc_name {
            if let Some(proc) = ty.get().procs.get(proc_name) {
                for param in proc.value[idx].parameters.iter() {
                    if starts_with(&param.name, query) {
                        results.push(CompletionItem {
                            label: param.name.clone(),
                            kind: Some(CompletionItemKind::Variable),
                            detail: Some("(parameter)".to_owned()),
                            .. Default::default()
                        });
                    }
                }
            }
        }

        let mut next = Some(ty);
        let mut skip = HashSet::new();
        while let Some(ty) = next {
            items_ty(results, &mut skip, ty, query);
            next = ty.parent_type();
        }
    }

    pub fn scoped_completions<'b, I>(&'b self, results: &mut Vec<CompletionItem>, iter: &I, priors: &[String], query: &str)
        where I: Iterator<Item=(Span, &'b Annotation)> + Clone
    {
        let mut next = self.find_scoped_type(iter, priors);
        let mut skip = HashSet::new();
        while let Some(ty) = next {
            items_ty(results, &mut skip, ty, query);
            next = ignore_root(ty.parent_type());
        }
    }
}

pub struct TypePathResult<'a> {
    pub ty: TypeRef<'a>,
    pub decl: Option<&'static str>,
    pub proc: Option<(&'a str, &'a ProcValue)>,
}
