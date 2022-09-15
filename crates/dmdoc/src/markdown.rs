//! Parser for "doc-block" markdown documents.

use std::collections::VecDeque;
use std::ops::Range;

use pulldown_cmark::{self, BrokenLinkCallback, Event, HeadingLevel, Parser, Tag};

pub fn render<'string, 'func>(
    markdown: &'string str,
    broken_link_callback: BrokenLinkCallback<'string, 'func>,
) -> String {
    let mut buf = String::new();
    push_html(&mut buf, parser(markdown, broken_link_callback));
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
    pub fn parse<'string, 'func>(
        markdown: &'string str,
        broken_link_callback: BrokenLinkCallback<'string, 'func>,
    ) -> Self {
        parse_main(parser(markdown, broken_link_callback).peekable())
    }

    pub fn parse_with_title<'string, 'func>(
        markdown: &'string str,
        broken_link_callback: BrokenLinkCallback<'string, 'func>,
    ) -> (Option<String>, Self) {
        let mut parser = parser(markdown, broken_link_callback).peekable();
        (
            if let Some(&Event::Start(Tag::Heading(HeadingLevel::H1, _, _))) = parser.peek() {
                parser.next();
                let mut pieces = Vec::new();
                loop {
                    match parser.next() {
                        None | Some(Event::End(Tag::Heading(HeadingLevel::H1, _, _))) => break,
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

fn parser<'string, 'func>(
    markdown: &'string str,
    broken_link_callback: BrokenLinkCallback<'string, 'func>,
) -> Parser<'string, 'func> {
    Parser::new_with_broken_link_callback(
        markdown,
        pulldown_cmark::Options::ENABLE_TABLES | pulldown_cmark::Options::ENABLE_STRIKETHROUGH,
        broken_link_callback,
    )
}

fn parse_main(mut parser: std::iter::Peekable<Parser>) -> DocBlock {
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
    DocBlock {
        html,
        teaser,
        has_description,
    }
}

fn push_html<'a, I: IntoIterator<Item = Event<'a>>>(buf: &mut String, iter: I) {
    pulldown_cmark::html::push_html(
        buf,
        HeadingLinker {
            inner: iter.into_iter(),
            output: Default::default(),
        },
    );
}

fn trim_right(buf: &mut String) {
    let len = buf.trim_end().len();
    buf.truncate(len);
}

/// Iterator adapter which replaces Start(Heading) tags with HTML including
/// an anchor.
struct HeadingLinker<'a, I> {
    inner: I,
    output: VecDeque<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for HeadingLinker<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Event<'a>> {
        if let Some(output) = self.output.pop_front() {
            return Some(output);
        }

        let original = self.inner.next();
        if let Some(Event::Start(Tag::Heading(heading, _, _))) = original {
            let mut text_buf = String::new();

            for event in self.inner.by_ref() {
                if let Event::Text(ref text) = event {
                    text_buf.push_str(text.as_ref());
                }

                if let Event::End(Tag::Heading(_, _, _)) = event {
                    break;
                }

                self.output.push_back(event);
            }

            self.output
                .push_back(Event::Html(format!("</h{}>", heading).into()));
            return Some(Event::Html(
                format!("<h{} id=\"{}\">", heading, slugify(&text_buf)).into(),
            ));
        }
        original
    }
}

fn slugify(input: &str) -> String {
    let mut output = String::new();
    let mut want_dash = false;
    for ch in input.chars() {
        if ch == '\'' {
            continue;
        }
        for ch in ch.to_lowercase() {
            if !ch.is_alphanumeric() {
                if want_dash {
                    output.push('-');
                    want_dash = false;
                }
            } else {
                output.push(ch);
                want_dash = true;
            }
        }
    }
    let len = output.trim_end_matches('-').len();
    output.truncate(len);
    output
}
