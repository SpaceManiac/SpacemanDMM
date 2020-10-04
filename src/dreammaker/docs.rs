//! Documentation parser.

use std::fmt;

/// A collection of documentation comments targeting the same item.
#[derive(Default, Clone, Debug, PartialEq)]
pub struct DocCollection {
    elems: Vec<DocComment>,
}

impl DocCollection {
    /// Push a doc comment to the collection.
    pub fn push(&mut self, comment: DocComment) {
        self.elems.push(comment);
    }

    /// Combine another collection into this one.
    pub fn extend(&mut self, collection: DocCollection) {
        self.elems.extend(collection.elems);
    }

    /// Check whether this collection is empty.
    pub fn is_empty(&self) -> bool {
        self.elems.iter().all(|c| c.is_empty())
    }

    /// Render this collection to a single Markdown document.
    pub fn text(&self) -> String {
        let mut output = String::new();
        let mut line_comments = String::new();
        for each in self.elems.iter() {
            match each.kind {
                CommentKind::Block => {
                    if !line_comments.is_empty() {
                        simplify(&mut output, &line_comments, '/');
                        line_comments.clear();
                    }
                    if each.is_empty() {
                        continue;
                    }
                    // block comments are always paragraphs
                    output.push('\n');
                    if simplify(&mut output, &each.text, '*') {
                        output.push('\n');
                    }
                },
                CommentKind::Line => {
                    // line comments are paragraphs only if there are blanks
                    line_comments.push_str(&each.text);
                    line_comments.push('\n');
                },
            }
        }
        if !line_comments.is_empty() {
            simplify(&mut output, &line_comments, '/');
        }
        output
    }
}

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
        DocComment {
            kind,
            target,
            text: String::new(),
        }
    }

    /// Check if this comment is entirely textless.
    fn is_empty(&self) -> bool {
        is_empty(&self.text, self.kind.ignore_char())
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

fn simplify(out: &mut String, text: &str, ignore_char: char) -> bool {
    if text.is_empty() {
        return false;
    }

    let mut prefix = None;
    let mut suffix = None;

    // determine the common prefix and suffix to strip
    for line in text.lines() {
        if is_empty(line, ignore_char) {
            continue;
        }

        let this_prefix = &line[..line.len() - line
            .trim_start_matches(|c: char| c.is_whitespace() || c == ignore_char)
            .len()];
        match prefix {
            None => prefix = Some(this_prefix),
            Some(ref mut prefix) => {
                let (mut chars, mut this_chars) = (prefix.chars(), this_prefix.chars());
                let mut no_match;
                loop {
                    no_match = chars.as_str();
                    match chars.next() {
                        Some(ch) => if Some(ch) != this_chars.next() {
                            break;
                        },
                        None => break,
                    }
                }
                *prefix = &prefix[..prefix.len() - no_match.len()];
            }
        }

        let this_suffix = &line[line.trim_end_matches(|c: char| c.is_whitespace() || c == ignore_char).len()..];
        match suffix {
            None => suffix = Some(this_suffix),
            Some(ref mut suffix) => {
                let (mut chars, mut this_chars) = (suffix.chars(), this_suffix.chars());
                let mut no_match;
                loop {
                    no_match = chars.as_str();
                    match chars.next_back() {
                        Some(ch) => if Some(ch) != this_chars.next_back() {
                            break;
                        },
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
    let mut anything = false;
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
            out.push('\n');
        }
        out.push_str(&line[prefix_len..line.len() - suffix_len]);
        anything = true;
        newlines = 1;
    }
    anything
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
