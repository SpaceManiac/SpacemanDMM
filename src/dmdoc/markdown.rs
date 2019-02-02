//! Parser for

use std::ops::Range;

use pulldown_cmark::{self, Parser, Tag, Event};

pub fn render(markdown: &str) -> String {
    let mut buf = String::new();
    push_html(&mut buf, parser(markdown));
    buf
}

/// A rendered markdown document with the teaser identified.
#[derive(Serialize)]
pub struct DocBlock {
    pub html: String,
    pub has_description: bool,
    teaser: Range<usize>,
}

impl DocBlock {
    pub fn parse(markdown: &str) -> Self {
        parse_main(parser(markdown).peekable())
    }

    pub fn parse_with_title(markdown: &str) -> (Option<String>, Self) {
        let mut parser = parser(markdown).peekable();
        (
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
                Some(title)
            } else {
                None
            },
            parse_main(parser),
        )
    }

    pub fn teaser(&self) -> &str {
        &self.html[self.teaser.clone()]
    }
}

fn parser(markdown: &str) -> Parser {
    Parser::new_with_broken_link_callback(
        markdown,
        pulldown_cmark::Options::ENABLE_TABLES,
        Some(&::handle_crosslink),
    )
}

fn parse_main(mut parser: ::std::iter::Peekable<Parser>) -> DocBlock {
    let mut html = String::new();
    let teaser;
    if let Some(&Event::Start(Tag::Paragraph)) = parser.peek() {
        push_html(&mut html, parser.next());
        let start = html.len();
        let mut pieces = Vec::new();
        loop {
            match parser.next() {
                None | Some(Event::End(Tag::Paragraph)) => break,
                Some(other) => pieces.push(other),
            }
        }
        push_html(&mut html, pieces);
        teaser = start..html.len();
        push_html(&mut html, Some(Event::End(Tag::Paragraph)));
    } else {
        teaser = 0..0;
    }

    let has_description = parser.peek().is_some();
    push_html(&mut html, parser);
    trim_right(&mut html);
    DocBlock { html, teaser, has_description }
}

fn push_html<'a, I: IntoIterator<Item=Event<'a>>>(buf: &mut String, iter: I) {
    pulldown_cmark::html::push_html(buf, iter.into_iter());
}

fn trim_right(buf: &mut String) {
    let len = buf.trim_end().len();
    buf.truncate(len);
}
