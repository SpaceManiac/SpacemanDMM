use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::*;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use syn::punctuated::Punctuated;

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
        Ok(ProcArgument {
            name,
        })
    }
}

enum EntryBody {
    None,
    Variable(Option<Expr>),
    Proc(Punctuated<ProcArgument, Token![,]>),
}

struct BuiltinEntry {
    path: Vec<Ident>,
    body: EntryBody,
}

impl Parse for BuiltinEntry {
    fn parse(input: ParseStream) -> Result<Self> {
        let _attrs = Attribute::parse_outer(input)?;

        let ident = input.parse()?;
        let mut path = vec![ident];
        while input.peek(Token![/]) {
            input.parse::<Token![/]>()?;
            path.push(Ident::parse_any(input)?);
        }

        let body = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            EntryBody::Variable(Some(input.parse::<Expr>()?))
        } else if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            EntryBody::Proc(content.parse_terminated(ProcArgument::parse)?)
        } else if path.iter().any(|i| i == "var") {
            EntryBody::Variable(None)
        } else {
            EntryBody::None
        };

        input.parse::<Token![;]>()?.span;
        Ok(BuiltinEntry {
            path,
            body,
        })
    }
}

struct Builtins(Vec<BuiltinEntry>);
impl Parse for Builtins {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut vec = Vec::new();
        while !input.is_empty() {
            vec.push(input.parse()?);
        }
        Ok(Builtins(vec))
    }
}

#[proc_macro]
pub fn builtins_table(input: TokenStream) -> TokenStream {
    let builtins = parse_macro_input!(input as Builtins).0;

    let mut output = Vec::new();
    for entry in builtins {
        let span = entry.path.first().unwrap().span();
        let lit_strs: Vec<_> = entry.path.into_iter().map(|x| LitStr::new(&x.to_string(), x.span())).collect();
        let path = quote! {
            &[ #(#lit_strs),* ]
        };

        let line = match entry.body {
            EntryBody::None => {
                quote_spanned! { span =>
                    tree.add_builtin_entry(#path);
                }
            },
            EntryBody::Variable(None) => {
                quote_spanned! { span =>
                    tree.add_builtin_var(#path, None);
                }
            },
            EntryBody::Variable(Some(expr)) => {
                quote_spanned! { span =>
                    tree.add_builtin_var(#path, Some(#expr));
                }
            },
            EntryBody::Proc(args) => {
                let args: Vec<_> = args.into_iter().map(|x| LitStr::new(&x.name.to_string(), x.name.span())).collect();
                quote_spanned! { span =>
                    tree.add_builtin_proc(#path, &[ #(#args),* ]);
                }
            }
        };
        output.push(TokenStream::from(line));
    }

    output.into_iter().flat_map(|x| x).collect()
}
