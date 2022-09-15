//! Query parser and evaluator for workspace symbol search.

/// A parsed symbol query.
#[derive(Debug)]
pub enum Query {
    Anything(String),
    Define(String),
    Type(String),
    Var(String),
    Proc(String),
}

impl Query {
    /// Parse a symbol query.
    pub fn parse(query: &str) -> Option<Query> {
        if !any_alphanumeric(query) {
            return None;
        }
        Some(if let Some(query) = query.strip_prefix('#') {
            Query::Define(query.to_lowercase())
        } else if let Some(query) = query.strip_prefix("var/") {
            if !any_alphanumeric(query) {
                return None;
            }
            Query::Var(query.to_lowercase())
        } else if let Some(query) = query.strip_prefix("proc/") {
            if !any_alphanumeric(query) {
                return None;
            }
            Query::Proc(query.to_lowercase())
        } else if query.contains('/') {
            Query::Type(query.to_lowercase())
        } else {
            Query::Anything(query.to_lowercase())
        })
    }

    /// Check whether this query matches the define with the given name.
    pub fn matches_define(&self, name: &str) -> bool {
        match *self {
            Query::Anything(ref q) => starts_with(name, q),
            Query::Define(ref q) => starts_with(name, q),
            _ => false,
        }
    }

    pub fn matches_type(&self, name: &str, path: &str) -> bool {
        match *self {
            Query::Anything(ref q) => starts_with(name, q),
            Query::Type(ref q) => path.to_lowercase().contains(q),
            _ => false,
        }
    }

    pub fn matches_on_type(&self, _path: &str) -> bool {
        matches!(*self, Query::Anything(_) | Query::Proc(_) | Query::Var(_))
    }

    pub fn matches_var(&self, name: &str) -> bool {
        match *self {
            Query::Anything(ref q) | Query::Var(ref q) => starts_with(name, q),
            _ => false,
        }
    }

    pub fn matches_proc(&self, name: &str, _kind: dm::ast::ProcDeclKind) -> bool {
        match *self {
            Query::Anything(ref q) | Query::Proc(ref q) => starts_with(name, q),
            _ => false,
        }
    }
}

fn simplify(s: &str) -> impl Iterator<Item = char> + Clone + '_ {
    s.chars()
        .flat_map(|c| c.to_lowercase())
        .filter(|c| c.is_alphanumeric())
}

// ignore case and underscores
pub fn starts_with<'a>(fulltext: &'a str, query: &'a str) -> bool {
    let mut query_chars = simplify(query);
    simplify(fulltext)
        .zip(&mut query_chars)
        .all(|(a, b)| a == b)
        && query_chars.next().is_none()
}

pub fn contains<'a>(fulltext: &'a str, query: &'a str) -> bool {
    let (mut fulltext, query) = (simplify(fulltext), simplify(query));
    loop {
        let mut clone = query.clone();
        if fulltext.clone().zip(&mut clone).all(|(a, b)| a == b) && clone.next().is_none() {
            return true;
        }
        if fulltext.next().is_none() {
            return false;
        }
    }
}

fn any_alphanumeric(text: &str) -> bool {
    text.chars()
        .flat_map(|c| c.to_lowercase())
        .any(|c| c.is_alphanumeric())
}
