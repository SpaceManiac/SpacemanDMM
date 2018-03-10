//! Utilities for managing the text document synchronization facilities of the
//! language server protocol.

use std::path::PathBuf;
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
        match self.map.insert(path, Document { version: doc.version, text: doc.text }) {
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

        // Make an effort to apply all changes, even if one failed.
        let mut result = Ok(());
        for change in changes {
            eprintln!("{}", change.text);
            let this_result = document.change(change);
            if result.is_ok() {
                result = this_result;
            }
        }
        document.version = new_version;
        result
    }
}

/// The internal representation of document contents received from the client.
struct Document {
    version: u64,
    text: String,
}

impl Document {
    fn change(&mut self, change: TextDocumentContentChangeEvent) -> Result<(), jsonrpc::Error> {
        let (_range, _range_length) = match (change.range, change.range_length) {
            (Some(a), Some(b)) => (a, b),
            _ => {
                // "If range and rangeLength are omitted the new text is
                // considered to be the full content of the document."
                self.text = change.text;
                return Ok(());
            }
        };
        // TODO: update the capabilities when this is implemented
        unimplemented!()
    }
}
