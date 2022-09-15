use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, quote_spanned};
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

#[derive(Clone, Default)]
struct Header {
    attrs: Vec<Attribute>,
    path: Vec<Ident>,
}

impl Header {
    fn parse_mut(&mut self, input: ParseStream) -> Result<()> {
        self.attrs.extend(Attribute::parse_outer(input)?);

        if input.peek(Ident) {
            self.path.push(Ident::parse_any(input)?);
        }
        while input.peek(Token![/]) {
            input.parse::<Token![/]>()?;
            self.path.push(Ident::parse_any(input)?);
        }

        Ok(())
    }
}

impl Parse for Header {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut header = Self::default();
        header.parse_mut(input)?;
        Ok(header)
    }
}

struct ProcArgument {
    name: Ident,
}

impl Parse for ProcArgument {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = Ident::parse_any(input)?;
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<Expr>()?;
        }
        Ok(ProcArgument { name })
    }
}

enum EntryBody {
    None,
    Variable(Option<Box<Expr>>),
    Proc(Punctuated<ProcArgument, Token![,]>),
}

impl EntryBody {
    fn parse_with_path(path: &[Ident], input: ParseStream) -> Result<Self> {
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Ok(EntryBody::Variable(Some(Box::new(input.parse::<Expr>()?))))
        } else if input.peek(syn::token::Paren) {
            let content;
            parenthesized!(content in input);
            Ok(EntryBody::Proc(
                content.parse_terminated(ProcArgument::parse)?,
            ))
        } else if path.iter().any(|i| i == "var") {
            Ok(EntryBody::Variable(None))
        } else {
            Ok(EntryBody::None)
        }
    }
}

struct BuiltinEntry {
    header: Header,
    body: EntryBody,
}

impl Parse for BuiltinEntry {
    fn parse(input: ParseStream) -> Result<Self> {
        let header: Header = input.parse()?;
        let body = EntryBody::parse_with_path(&header.path, input)?;

        input.parse::<Token![;]>()?;
        Ok(BuiltinEntry { header, body })
    }
}

struct BuiltinsTable(Vec<BuiltinEntry>);

impl BuiltinsTable {
    fn parse_with_header_into(
        vec: &mut Vec<BuiltinEntry>,
        header: &Header,
        input: ParseStream,
    ) -> Result<()> {
        while !input.is_empty() {
            let mut new_header = header.clone();
            new_header.parse_mut(input)?;
            if input.peek(syn::token::Brace) {
                let content;
                braced!(content in input);
                BuiltinsTable::parse_with_header_into(vec, &new_header, &content)?;
            } else {
                let body = EntryBody::parse_with_path(&new_header.path, input)?;
                input.parse::<Token![;]>()?;
                vec.push(BuiltinEntry {
                    header: new_header,
                    body,
                });
            }
        }
        Ok(())
    }
}

impl Parse for BuiltinsTable {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut vec = Vec::new();
        BuiltinsTable::parse_with_header_into(&mut vec, &Default::default(), input)?;
        Ok(BuiltinsTable(vec))
    }
}

struct DocComment(LitStr);
impl Parse for DocComment {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![=]>()?;
        let lit = input.parse::<LitStr>()?;
        Ok(DocComment(lit))
    }
}

#[proc_macro]
pub fn builtins_table(input: TokenStream) -> TokenStream {
    let builtins = parse_macro_input!(input as BuiltinsTable).0;

    let mut output = Vec::new();
    for entry in builtins {
        let span = entry.header.path.first().unwrap().span();
        let lit_strs: Vec<_> = entry
            .header
            .path
            .into_iter()
            .map(|x| LitStr::new(&x.to_string(), x.span()))
            .collect();
        let path = quote! {
            &[ #(#lit_strs),* ]
        };

        let mut markdown = String::new();
        let mut markdown_span = None;

        let mut attr_calls = TokenStream2::new();
        for attr in entry.header.attrs {
            let attr_span = attr.span();
            let path = attr.path;
            let ident = &path.segments.last().unwrap().ident;
            if ident == "doc" {
                markdown_span = Some(attr_span);
                markdown.push_str(&syn::parse2::<DocComment>(attr.tokens).unwrap().0.value());
                markdown.push('\n');
            } else {
                attr_calls.extend(quote_spanned! { attr_span => .docs.#path });
                attr_calls.extend(attr.tokens);
            }
        }

        if let Some(markdown_span) = markdown_span {
            let lit = LitStr::new(&markdown, span);
            attr_calls.extend(quote_spanned! { markdown_span => .docs.doc(#lit) });
        }

        let line = match entry.body {
            EntryBody::None => {
                quote_spanned! { span =>
                    tree.add_builtin_type(#path) #attr_calls;
                }
            }
            EntryBody::Variable(None) => {
                quote_spanned! { span =>
                    tree.add_builtin_var(#path, None) #attr_calls;
                }
            }
            EntryBody::Variable(Some(expr)) => {
                quote_spanned! { span =>
                    tree.add_builtin_var(#path, Some(#expr)) #attr_calls;
                }
            }
            EntryBody::Proc(args) => {
                let args: Vec<_> = args
                    .into_iter()
                    .map(|x| LitStr::new(&x.name.to_string(), x.name.span()))
                    .collect();
                quote_spanned! { span =>
                    tree.add_builtin_proc(#path, &[ #(#args),* ]) #attr_calls;
                }
            }
        };
        output.push(line);
    }

    output.into_iter().flat_map(TokenStream::from).collect()
}
