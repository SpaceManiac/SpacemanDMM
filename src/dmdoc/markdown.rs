//! Parser for

use pulldown_cmark::{self, Parser, Tag, Event};

/// A markdown parsed to HTML sections.
#[derive(Serialize, Default)]
pub struct DocBlock {
    /// Optional leading <h1>
    pub title: Option<String>,
    /// Leading <p>
    pub teaser: String,
    /// Optional remainder
    pub description: Option<String>,
}

pub fn render(markdown: &str) -> String {
    let mut buf = String::new();
    let parser = Parser::new_with_broken_link_callback(
        markdown,
        pulldown_cmark::OPTION_ENABLE_TABLES,
        Some(&::handle_crosslink)
    );
    push_html(&mut buf, parser);
    buf
}

pub fn parse_md_docblock(markdown: &str) -> Result<DocBlock, String> {
    let mut block = DocBlock::default();
    let mut parser = Parser::new_with_broken_link_callback(
        markdown,
        pulldown_cmark::OPTION_ENABLE_TABLES,
        Some(&::handle_crosslink)
    ).peekable();

    if let Some(&Event::Start(Tag::Header(1))) = parser.peek() {
        parser.next();
        let mut pieces = Vec::new();
        loop {
            match parser.next() {
                None | Some(Event::End(Tag::Header(1))) => break,
                Some(other) => pieces.push(other),
            }
        }

        let mut title = String::new();
        push_html(&mut title, pieces);
        block.title = Some(title);
    }

    if let Some(&Event::Start(Tag::Paragraph)) = parser.peek() {
        parser.next();
        let mut pieces = Vec::new();
        loop {
            match parser.next() {
                None | Some(Event::End(Tag::Paragraph)) => break,
                Some(other) => pieces.push(other),
            }
        }

        push_html(&mut block.teaser, pieces);
    } else {
        return Err(format!("teaser should be paragraph, not {:?}", parser.peek()));
    }

    if parser.peek().is_some() {
        let mut description = String::new();
        push_html(&mut description, parser);
        block.description = Some(description);
    }
    Ok(block)
}

fn push_html<'a, I: IntoIterator<Item=Event<'a>>>(buf: &mut String, iter: I) {
    pulldown_cmark::html::push_html(buf, iter.into_iter());
    trim_right(buf);
}

fn trim_right(buf: &mut String) {
    let len = buf.trim_right().len();
    buf.truncate(len);
}
