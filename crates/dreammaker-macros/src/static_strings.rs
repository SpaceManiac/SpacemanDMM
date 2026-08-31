use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{punctuated::Punctuated, *};

pub fn static_strings(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input with Punctuated<LitStr, Token![,]>::parse_terminated);
    let mut macro_branches = TokenStream2::new();
    for each in input.iter() {
        let value = each.value();
        macro_branches.extend(quote! {
            (#value) => { const { $crate::ast::Ident::from_static(#value) } };
        });
    }
    quote! {
        static STATIC_STRINGS: phf::OrderedSet<&'static str> = phf::phf_ordered_set! { #input };

        #[macro_export]
        /// Get a compile-time-checked [Ident][crate::ast::Ident] for a builtin string.
        macro_rules! ident {
            #macro_branches
        }
    }
    .into()
}
