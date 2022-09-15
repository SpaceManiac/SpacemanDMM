//! Utilities for managing the text document synchronization facilities of the
//! language server protocol.
#![allow(dead_code)]

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, BufRead, Read};
use std::rc::Rc;
use url::Url;

use lsp_types::{
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    VersionedTextDocumentIdentifier,
};

use super::{invalid_request, url_to_path};

use ahash::RandomState;

/// A store for the contents of currently-open documents, with appropriate
/// fallback for documents which are not currently open.
#[derive(Default)]
pub struct DocumentStore {
    map: HashMap<Url, Document, RandomState>,
}

impl DocumentStore {
    pub fn open(&mut self, doc: TextDocumentItem) -> Result<(), jsonrpc::Error> {
        match self
            .map
            .insert(doc.uri.clone(), Document::new(doc.version, doc.text))
        {
            None => Ok(()),
            Some(_) => Err(invalid_request(format!("opened twice: {}", doc.uri))),
        }
    }

    pub fn close(&mut self, id: TextDocumentIdentifier) -> Result<Url, jsonrpc::Error> {
        match self.map.remove(&id.uri) {
            Some(_) => Ok(id.uri),
            None => Err(invalid_request(format!(
                "cannot close non-opened: {}",
                id.uri
            ))),
        }
    }

    pub fn change(
        &mut self,
        doc_id: VersionedTextDocumentIdentifier,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<Url, jsonrpc::Error> {
        let new_version = doc_id.version;

        let document = match self.map.get_mut(&doc_id.uri) {
            Some(doc) => doc,
            None => {
                return Err(invalid_request(format!(
                    "cannot change non-opened: {}",
                    doc_id.uri
                )))
            }
        };

        if new_version < document.version {
            eprintln!(
                "new_version: {} < document_version: {}",
                new_version, document.version
            );
            return Err(invalid_request(
                "document version numbers shouldn't go backwards",
            ));
        }
        document.version = new_version;

        // Make an effort to apply all changes, even if one failed.
        let mut result = Ok(doc_id.uri);
        for change in changes {
            if let Err(e) = document.change(change) {
                result = Err(e);
            }
        }
        result
    }

    pub fn get_contents<'a>(&'a self, url: &Url) -> io::Result<Cow<'a, str>> {
        if let Some(document) = self.map.get(url) {
            return Ok(Cow::Borrowed(&document.text));
        }

        if let Ok(path) = url_to_path(url) {
            let mut text = String::new();
            let mut file = std::fs::File::open(path)?;
            file.read_to_string(&mut text)?;
            return Ok(Cow::Owned(text));
        }

        Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("URL not opened and schema is not 'file': {}", url),
        ))
    }

    pub fn read(&self, url: &Url) -> io::Result<Box<dyn io::Read>> {
        if let Some(document) = self.map.get(url) {
            return Ok(Box::new(Cursor::new(document.text.clone())) as Box<dyn io::Read>);
        }

        if let Ok(path) = url_to_path(url) {
            let file = std::fs::File::open(path)?;
            return Ok(Box::new(file) as Box<dyn io::Read>);
        }

        Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("URL not opened and schema is not 'file': {}", url),
        ))
    }
}

/// The internal representation of document contents received from the client.
struct Document {
    version: i32,
    text: Rc<String>,
}

impl Document {
    fn new(version: i32, text: String) -> Document {
        Document {
            version,
            text: Rc::new(text),
        }
    }

    fn change(&mut self, change: TextDocumentContentChangeEvent) -> Result<(), jsonrpc::Error> {
        // rangeLength is deprecated: https://github.com/Microsoft/language-server-protocol/issues/9
        let range = match change.range {
            Some(range) => range,
            _ => {
                // "If range and rangeLength are omitted the new text is
                // considered to be the full content of the document."
                self.text = Rc::new(change.text);
                return Ok(());
            }
        };

        let start_pos = total_offset(&self.text, range.start.line, range.start.character)?;
        let end_pos = total_offset(&self.text, range.end.line, range.end.character)?;
        Rc::make_mut(&mut self.text).replace_range(start_pos..end_pos, &change.text);
        Ok(())
    }
}

/// Find the offset into the given text at which the given zero-indexed line
/// number begins.
fn line_offset(text: &str, line_number: u32) -> Result<usize, jsonrpc::Error> {
    // Hopefully this logic isn't too far off.
    let mut start_pos = 0;
    for _ in 0..line_number {
        match text[start_pos..].find('\n') {
            Some(next_pos) => start_pos += next_pos + 1,
            None => return Err(invalid_request("line number apparently out of range")),
        }
    }
    Ok(start_pos)
}

fn total_offset(text: &str, line: u32, mut character: u32) -> Result<usize, jsonrpc::Error> {
    let start = line_offset(text, line)?;

    // column is measured in UTF-16 code units, which is really inconvenient.
    let mut chars = text[start..].chars();
    while character > 0 {
        if let Some(ch) = chars.next() {
            character = character.saturating_sub(ch.len_utf16() as u32);
        } else {
            break;
        }
    }
    Ok(text.len() - chars.as_str().len())
}

// Reverse of the above.
pub fn offset_to_position(text: &str, offset: usize) -> lsp_types::Position {
    let mut line = 0;
    let mut line_start = 0;
    loop {
        match text[line_start..].find('\n') {
            Some(next_pos) if line_start + next_pos < offset => line_start += next_pos + 1,
            _ => break,
        }
        line += 1;
    }

    let mut character = 0;
    for ch in text[line_start..offset].chars() {
        character += ch.len_utf16() as u32;
    }

    lsp_types::Position { line, character }
}

pub fn get_range(text: &str, range: lsp_types::Range) -> Result<&str, jsonrpc::Error> {
    Ok(
        &text[total_offset(text, range.start.line, range.start.character)?
            ..total_offset(text, range.end.line, range.end.character)?],
    )
}

pub fn find_word(text: &str, offset: usize) -> &str {
    // go left as far as we can
    let mut start = offset;
    loop {
        let mut start_next = start - 1;
        while !text.is_char_boundary(start_next) {
            start_next -= 1;
        }
        if !text[start_next..start]
            .chars()
            .next()
            .map_or(false, is_ident)
        {
            break;
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
            break;
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
    ('0'..='9').contains(&ch) || ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_'
}

/// An adaptation of `std::io::Cursor` which works on an `Rc<String>`, which
/// sadly does not satisfy `AsRef<u8>`.
struct Cursor {
    inner: Rc<String>,
    pos: u64,
}

impl Cursor {
    fn new(inner: Rc<String>) -> Cursor {
        Cursor { inner, pos: 0 }
    }
}

impl Read for Cursor {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = Read::read(&mut self.fill_buf()?, buf)?;
        self.pos += n as u64;
        Ok(n)
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let n = buf.len();
        Read::read_exact(&mut self.fill_buf()?, buf)?;
        self.pos += n as u64;
        Ok(())
    }
}

impl BufRead for Cursor {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        let amt = std::cmp::min(self.pos, self.inner.as_ref().len() as u64);
        Ok(&self.inner.as_bytes()[(amt as usize)..])
    }
    fn consume(&mut self, amt: usize) {
        self.pos += amt as u64;
    }
}
