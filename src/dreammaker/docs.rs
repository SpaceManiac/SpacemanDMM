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
        let ignore_char = self.kind.ignore_char();
        self.text.chars().all(|c| c.is_whitespace() || c == ignore_char)
    }

    /// Append the contents of another doc comment to this one.
    pub fn merge_with(&mut self, other: DocComment) {
        if other.is_empty() {
            return;
        }

        self.text = format!("{}\n{}", self.text, other.text);
    }

    /// Merge or begin an in-progress doc comment with this one.
    pub fn merge_into(self, other: &mut Option<DocComment>) {
        if self.is_empty() {
            return;
        }
        // TODO: if `kind` differs, pre-simplify?
        match *other {
            None => *other = Some(self),
            Some(ref mut it) => it.merge_with(self),
        }
    }

    /// Simplify this doc comment, stripping line-start whitespace.
    pub fn simplify(&mut self) {
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

/// The possible items that a documentation comment may target.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum DocTarget {
    /// Starting with `*` or `/`, referring to the following item.
    FollowingItem,
    /// Starting with `!`, referring to the enclosing item.
    EnclosingItem,
}
