//! Debug visualizer for token, AST, and annotation spans.

use std::{collections::BTreeMap, path::Path};

use dreammaker::{Context, FileId, Lexer, Location, Severity};
use serde::Serialize;

pub fn main() {
    let mut context = Context::default();
    context.set_print_severity(Some(Severity::Info));

    let path = std::env::args().skip(1).next().unwrap();

    let mut vis = Visualizer::default();
    let content = std::fs::read_to_string(&path).unwrap();
    let fileid = context.file_list().register(Path::new(&path));
    for token in Lexer::new(&context, fileid, content.as_bytes()) {
        vis.add_spot(
            "1. Lexer",
            token.start,
            token.end,
            format!("{:?}", token.token),
        );
    }
    // NB: errors because the preprocessor is supposed to be in between.
    for token in
        dreammaker::IndentProcessor::new(&context, Lexer::new(&context, fileid, content.as_bytes()))
    {
        vis.add_spot(
            "3. Indenter",
            token.start,
            token.end,
            format!("{:?}", token.token),
        );
    }
    vis.add_file(fileid, path, content);

    print!("{}", vis);
}

#[derive(Default, Serialize)]
pub struct Visualizer {
    files: BTreeMap<FileId, File>,
    tabs: BTreeMap<String, Vec<Spot>>,
}

#[derive(Serialize)]
struct File {
    name: String,
    content: String,
}

#[derive(Serialize)]
struct Spot {
    start: Location,
    end: Location,
    text: String,
}

impl Visualizer {
    pub fn add_file(&mut self, id: FileId, name: String, content: String) {
        self.files.insert(id, File { name, content });
    }

    pub fn add_spot(&mut self, tab: &str, start: Location, end: Location, text: String) {
        let tab = match self.tabs.get_mut(tab) {
            Some(x) => x,
            None => self.tabs.entry(tab.to_owned()).or_default(),
        };
        tab.push(Spot { start, end, text });
    }
}

impl std::fmt::Display for Visualizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let template = include_str!("spans.html");
        let (before, after) = template.split_once("<!--DATA-->").unwrap();
        let ser = serde_json::to_string(&self)
            .unwrap()
            .replace("<", "\\u003c");
        write!(
            f,
            r#"{before}<script type="text/json" id="data">{ser}</script>{after}"#
        )
    }
}
