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
        if query.is_empty() || query == "#" || query == "/" {
            return None
        }
        Some(if query.starts_with("#") {
            Query::Define(query[1..].to_lowercase())
        } else if query.starts_with("var/") {
            let query = &query["var/".len()..];
            if query.is_empty() {
                return None
            }
            Query::Var(query.to_lowercase())
        } else if query.starts_with("proc/") {
            let query = &query["proc/".len()..];
            if query.is_empty() {
                return None
            }
            Query::Proc(query.to_lowercase())
        } else if query.contains("/") {
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
        match *self {
            Query::Anything(_) |
            Query::Proc(_) |
            Query::Var(_) => true,
            _ => false,
        }
    }

    pub fn matches_var(&self, name: &str) -> bool {
        match *self {
            Query::Anything(ref q) |
            Query::Var(ref q) => starts_with(name, q),
            _ => false,
        }
    }

    pub fn matches_proc(&self, name: &str, _is_verb: bool) -> bool {
        match *self {
            Query::Anything(ref q) |
            Query::Proc(ref q) => starts_with(name, q),
            _ => false,
        }
    }
}

// ignore case and underscores
fn starts_with<'a>(fulltext: &'a str, query: &'a str) -> bool {
    let simplify = |s: &'a str| s.chars().flat_map(|c| c.to_lowercase()).filter(|c| c.is_alphanumeric());

    let mut query_chars = simplify(query);
    simplify(fulltext).zip(&mut query_chars).all(|(a, b)| a == b) && query_chars.next().is_none()
}
