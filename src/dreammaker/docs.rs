//! Documentation parser.

use std::fmt;

/// A documentation comment.
#[derive(Clone, Debug, PartialEq)]
pub struct DocComment {
    pub kind: CommentKind,
    pub target: DocTarget,
    pub text: String,
}

impl DocComment {
    /// Construct an empty DocComment with the given properties.
    pub fn new(kind: CommentKind, target: DocTarget) -> DocComment {
        DocComment { kind, target, text: String::new() }
    }

    /// Check if this comment is entirely textless.
    pub fn is_empty(&self) -> bool {
        is_empty(&self.text, self.kind.ignore_char())
    }

    /// Merge or begin an in-progress doc comment with this one.
    pub fn merge_into(mut self, other: &mut Option<DocComment>) {
        self.simplify();
        if self.is_empty() {
            return;
        }
        match *other {
            None => *other = Some(self),
            Some(ref mut it) => {
                let extra_newline = match (it.kind, self.kind) {
                    (CommentKind::Block, CommentKind::Block) => "\n",
                    _ => "",
                };
                it.text = format!("{}\n{}{}", it.text, extra_newline, self.text);
            }
        }
    }

    /// Simplify this doc comment, stripping line-start whitespace.
    pub fn simplify(&mut self) {
        self.text = simplify(&self.text, self.kind.ignore_char());
    }
}

impl fmt::Display for DocComment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.kind, self.target) {
            (CommentKind::Block, DocTarget::FollowingItem) => write!(f, "/**{}*/", self.text),
            (CommentKind::Block, DocTarget::EnclosingItem) => write!(f, "/*!{}*/", self.text),
            (CommentKind::Line,  DocTarget::FollowingItem) => write!(f, "///{}", self.text),
            (CommentKind::Line,  DocTarget::EnclosingItem) => write!(f, "//!{}", self.text),
        }
    }
}

/// The possible documentation comment kinds.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CommentKind {
    /// A block `/** */` comment.
    Block,
    /// A line `///` comment.
    Line,
}

impl CommentKind {
    fn ignore_char(self) -> char {
        match self {
            CommentKind::Block => '*',
            CommentKind::Line => '/',
        }
    }
}

fn simplify(text: &str, ignore_char: char) -> String {
    let mut prefix = None;
    let mut suffix = None;

    // determine the common prefix and suffix to strip
    for line in text.lines() {
        if is_empty(line, ignore_char) {
            continue;
        }

        let this_prefix = &line[..line.len() - line.trim_left_matches(|c: char| c.is_whitespace() || c == ignore_char).len()];
        match prefix {
            None => prefix = Some(this_prefix),
            Some(ref mut prefix) => {
                let (mut chars, mut this_chars) = (prefix.chars(), this_prefix.chars());
                let mut no_match;
                loop {
                    no_match = chars.as_str();
                    match chars.next() {
                        Some(ch) => if Some(ch) != this_chars.next() { break },
                        None => break,
                    }
                }
                *prefix = &prefix[..prefix.len() - no_match.len()];
            }
        }

        let this_suffix = &line[line.trim_right_matches(|c: char| c.is_whitespace() || c == ignore_char).len()..];
        match suffix {
            None => suffix = Some(this_suffix),
            Some(ref mut suffix) => {
                let (mut chars, mut this_chars) = (suffix.chars(), this_suffix.chars());
                let mut no_match;
                loop {
                    no_match = chars.as_str();
                    match chars.next_back() {
                        Some(ch) => if Some(ch) != this_chars.next_back() { break },
                        None => break,
                    }
                }
                *suffix = &suffix[no_match.len()..];
            }
        }
    }

    let prefix_len = prefix.map_or(0, |s| s.len());
    let suffix_len = suffix.map_or(0, |s| s.len());

    // perform the stripping
    let mut newlines = 0;
    let mut out = String::new();
    for line in text.lines() {
        // Skip empty lines at the start or end of the comment...
        if is_empty(line, ignore_char) {
            if newlines > 0 {
                newlines += 1;
            }
            continue;
        }
        // ...but include them in the middle.
        for _ in 0..newlines {
            out.push_str("\n");
        }
        out.push_str(&line[prefix_len..line.len()-suffix_len]);
        newlines = 1;
    }
    out
}

fn is_empty(text: &str, ignore_char: char) -> bool {
    text.chars().all(|c| c.is_whitespace() || c == ignore_char)
}

/// The possible items that a documentation comment may target.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum DocTarget {
    /// Starting with `*` or `/`, referring to the following item.
    FollowingItem,
    /// Starting with `!`, referring to the enclosing item.
    EnclosingItem,
}
