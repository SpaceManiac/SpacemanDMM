extern crate proc_macro;
use proc_macro::*;

#[proc_macro]
pub fn entries(input: TokenStream) -> TokenStream {
    let mut output = Vec::new();
    let mut buffer = Vec::new();

    for tt in input {
        match tt {
            TokenTree::Punct(ref p) if p.as_char() == ';' => {
                output.push(TokenTree::Ident(Ident::new("one_entry", Span::call_site())));
                output.push(TokenTree::Punct(Punct::new('!', Spacing::Joint)));
                output.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, buffer.drain(..).collect())));
                output.push(tt);
            }
            other => buffer.push(other),
        }
    }

    output.into_iter().collect()
}
