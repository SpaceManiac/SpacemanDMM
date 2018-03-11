//! Utilities for managing the text document synchronization facilities of the
//! language server protocol.

use std::io::{self, Read};
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::collections::HashMap;

use jsonrpc;
use langserver::{TextDocumentItem, TextDocumentIdentifier,
    VersionedTextDocumentIdentifier, TextDocumentContentChangeEvent};

use super::{url_to_path, invalid_request};

/// A store for the contents of currently-open documents, with appropriate
/// fallback for documents which are not currently open.
#[derive(Default)]
pub struct DocumentStore {
    map: HashMap<PathBuf, Document>,
}

impl DocumentStore {
    pub fn open(&mut self, doc: TextDocumentItem) -> Result<(), jsonrpc::Error> {
        let path = url_to_path(doc.uri)?;
        match self.map.insert(path, Document::new(doc.version, doc.text)) {
            None => Ok(()),
            Some(_) => Err(invalid_request("opened a document a second time")),
        }
    }

    pub fn close(&mut self, id: TextDocumentIdentifier) -> Result<(), jsonrpc::Error> {
        let path = url_to_path(id.uri)?;
        match self.map.remove(&path) {
            Some(_) => Ok(()),
            None => Err(invalid_request("cannot close non-opened document")),
        }
    }

    pub fn change(&mut self,
        doc_id: VersionedTextDocumentIdentifier,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<(), jsonrpc::Error> {
        let path = url_to_path(doc_id.uri)?;

        // "If a versioned text document identifier is sent from the server to
        // the client and the file is not open in the editor (the server has
        // not received an open notification before) the server can send `null`
        // to indicate that the version is known and the content on disk is the
	    // truth (as speced with document content ownership)."
        let new_version = match doc_id.version {
            Some(version) => version,
            None => return Err(invalid_request("don't know how to deal with this")),
        };

        let document = match self.map.get_mut(&path) {
            Some(doc) => doc,
            None => return Err(invalid_request("that document isn't opened")),
        };

        if new_version < document.version {
            eprintln!("new_version: {} < document_version: {}", new_version, document.version);
            return Err(invalid_request("version numbers shouldn't go backwards"));
        }
        document.version = new_version;

        // Make an effort to apply all changes, even if one failed.
        let mut result = Ok(());
        for change in changes {
            let this_result = document.change(change);
            if result.is_ok() {
                result = this_result;
            }
        }
        result
    }

    pub fn get_contents<'a>(&'a self, path: &Path) -> io::Result<Cow<'a, str>> {
        match self.map.get(path) {
            Some(document) => Ok(Cow::Borrowed(&document.text)),
            None => {
                let mut text = String::new();
                let mut file = ::std::fs::File::open(path)?;
                file.read_to_string(&mut text)?;
                Ok(Cow::Owned(text))
            }
        }
    }

    #[allow(dead_code)]
    pub fn read<'a>(&'a self, path: &Path) -> io::Result<Box<io::Read + 'a>> {
        match self.map.get(path) {
            Some(document) => Ok(Box::new(document.text.as_bytes()) as Box<io::Read>),
            None => ::std::fs::File::open(path).map(|f| Box::new(f) as Box<io::Read>),
        }
    }
}

/// The internal representation of document contents received from the client.
struct Document {
    version: u64,
    text: String,
}

impl Document {
    fn new(version: u64, text: String) -> Document {
        Document { version, text }
    }

    fn change(&mut self, change: TextDocumentContentChangeEvent) -> Result<(), jsonrpc::Error> {
        let (range, range_length) = match (change.range, change.range_length) {
            (Some(a), Some(b)) => (a, b),
            _ => {
                // "If range and rangeLength are omitted the new text is
                // considered to be the full content of the document."
                self.text = change.text;
                return Ok(());
            }
        };

        let start_pos = total_offset(&self.text, range.start.line, range.start.character)?;
        splice(&mut self.text, start_pos .. start_pos + range_length as usize, &change.text);
        Ok(())
    }
}

// Based on the unstable String::splice from libstd.
fn splice(text: &mut String, range: ::std::ops::Range<usize>, replace_with: &str) {
    assert!(text.is_char_boundary(range.start));
    assert!(text.is_char_boundary(range.end));
    unsafe {
        text.as_mut_vec()
    }.splice(range, replace_with.bytes());
}

/// Find the offset into the given text at which the given zero-indexed line
/// number begins.
pub fn line_offset(text: &str, line_number: u64) -> Result<usize, jsonrpc::Error> {
    // Hopefully this logic isn't too far off.
    let mut start_pos = 0;
    for _ in 0..line_number {
        match text[start_pos..].find("\n") {
            Some(next_pos) => start_pos += next_pos + 1,
            None => return Err(invalid_request("line number apparently out of range")),
        }
    }
    Ok(start_pos)
}

pub fn total_offset(text: &str, line: u64, character: u64) -> Result<usize, jsonrpc::Error> {
    Ok(line_offset(text, line)? + character as usize)
}

pub fn find_word(text: &str, offset: usize) -> &str {
    // go left as far as we can
    let mut start = offset;
    loop {
        let mut start_next = start - 1;
        while !text.is_char_boundary(start_next) {
            start_next -= 1;
        }
        if !text[start_next..start].chars().next().map_or(false, is_ident) {
            break
        }
        start = start_next;
    }

    // go right as far as we can
    let mut end = offset;
    loop {
        let mut end_next = end + 1;
        while !text.is_char_boundary(end_next) {
            end_next += 1;
        }
        if !text[end..end_next].chars().next().map_or(false, is_ident) {
            break
        }
        end = end_next;
    }

    if start == end {
        ""
    } else {
        &text[start..end]
    }
}

fn is_ident(ch: char) -> bool {
    (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}
