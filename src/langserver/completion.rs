//! Supporting functions for completion and go-to-definition.

use langserver::*;

use dm::objtree::{TypeRef, TypeVar, TypeProc};

use is_constructor_name;

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
