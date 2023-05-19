use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::*;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use proc_macro2::TokenStream as TokenStream2;

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
        Ok(ProcArgument {
            name,
        })
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
            Ok(EntryBody::Proc(content.parse_terminated(ProcArgument::parse)?))
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
        Ok(BuiltinEntry {
            header,
            body,
        })
    }
}

struct BuiltinsTable(Vec<BuiltinEntry>);

impl BuiltinsTable {
    fn parse_with_header_into(vec: &mut Vec<BuiltinEntry>, header: &Header, input: ParseStream) -> Result<()> {
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
        let lit_strs: Vec<_> = entry.header.path.into_iter().map(|x| LitStr::new(&x.to_string(), x.span())).collect();
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
            },
            EntryBody::Variable(None) => {
                quote_spanned! { span =>
                    tree.add_builtin_var(#path, None) #attr_calls;
                }
            },
            EntryBody::Variable(Some(expr)) => {
                quote_spanned! { span =>
                    tree.add_builtin_var(#path, Some(#expr)) #attr_calls;
                }
            },
            EntryBody::Proc(args) => {
                let args: Vec<_> = args.into_iter().map(|x| LitStr::new(&x.name.to_string(), x.name.span())).collect();
                quote_spanned! { span =>
                    tree.add_builtin_proc(#path, &[ #(#args),* ]) #attr_calls;
                }
            }
        };
        output.push(line);
    }

    output.into_iter().flat_map(TokenStream::from).collect()
}

#[proc_macro_derive(SyntaxEq)]
pub fn derive_syntax_eq(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

     // The name of the sruct.
    let name = &ast.ident;

    // Add a bound `T: SyntaxEq` to every type parameter T.
    let mut generics = ast.generics;
    for param in &mut generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            type_param.bounds.push(syn::parse_quote!(SyntaxEq));
        }
    }

    // Extract the generics of the struct/enum.
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Traverse the parsed data to generate the individual parts of the function.
    match ast.data {
        syn::Data::Enum(data_enum) => {
            if data_enum.variants.is_empty() {
                // Empty enums are easy to implement.
                let gen = quote! {
                    impl SyntaxEq for #name {
                        fn syntax_eq(&self, _: &Self) -> bool {
                            true
                        }
                    }
                };
                return gen.into()
            }

            let mut cmds = Vec::with_capacity(data_enum.variants.len());

            for variant in data_enum.variants.iter() {
                let ident = &variant.ident;

                match &variant.fields {
                    syn::Fields::Unnamed(unnamed_fields) => {
                        let num_fields = unnamed_fields.unnamed.len();

                        let mut field_idents = Vec::with_capacity(num_fields);
                        let mut field_idents_other = Vec::with_capacity(num_fields);
                        for i in 0..num_fields {
                            let field_ours = String::from("our")+&i.to_string();
                            let field_ident_ours = syn::parse_str::<syn::Ident>(&field_ours).unwrap();
                            let field_theirs = String::from("their")+&i.to_string();
                            let field_ident_theirs = syn::parse_str::<syn::Ident>(&field_theirs).unwrap();

                            field_idents.push(field_ident_ours);
                            field_idents_other.push(field_ident_theirs)
                        }

                        let mut field_cmds = Vec::with_capacity(num_fields);

                        for (i, _field) in unnamed_fields.unnamed.iter().enumerate() {
                            let field_ident_ours = &field_idents[i];
                            let field_ident_theirs = &field_idents_other[i];

                            field_cmds.push(quote! {
                                state &= SyntaxEq::syntax_eq(#field_ident_ours, #field_ident_theirs);
                            })
                        }

                        cmds.push(quote! {
                            Self::#ident(#(#field_idents,)*) => {
                                let mut state = true;
                                if let Self::#ident(#(#field_idents_other,)*) = other {
                                    #(#field_cmds)*

                                    state
                                } else {
                                    panic!("We just checked discriminant equality and now it's not!")
                                }
                            }
                        });
                    }
                    syn::Fields::Named(named_fields) => {
                        let num_fields = named_fields.named.len();

                        let mut field_idents = Vec::with_capacity(num_fields);

                        let mut field_cmds = Vec::with_capacity(num_fields);

                        let mut alias_cmds = Vec::with_capacity(num_fields);

                        for field in named_fields.named.iter() {
                            let field_ident = field.ident.as_ref().unwrap();
                            let field_ident_our = Ident::new(&format!("our_{}", field_ident), ident.span());

                            field_idents.push(field_ident);

                            alias_cmds.push(quote! {
                                let #field_ident_our = #field_ident;
                            });

                            field_cmds.push(quote! {
                                state &= SyntaxEq::syntax_eq(#field_ident_our, #field_ident);
                            })
                        }

                        cmds.push(quote! {
                            Self::#ident{#(#field_idents,)*} => {
                                let mut state = true;

                                #(#alias_cmds)*

                                if let Self::#ident{#(#field_idents,)*} = other {
                                    #(#field_cmds)*

                                    state
                                } else {
                                    panic!("We just checked discriminant equality and now it's not!")
                                }
                            }
                        });
                    }
                    syn::Fields::Unit => {
                        cmds.push(quote! {
                            Self::#ident => true,
                        });
                    }
                }
            }

            // Build the trait implementation
            let gen = quote! {
                impl #impl_generics SyntaxEq for #name #ty_generics #where_clause {
                    fn syntax_eq(&self, other: &Self) -> bool {
                        if std::mem::discriminant(self) != std::mem::discriminant(other) {
                            false
                        } else {
                            match self {
                                #(#cmds)*
                            }
                        }
                    }
                }
            };

            gen.into()
        }
        syn::Data::Union(_data_union) => panic!("Deriving SyntaxEq for unions is currently not supported."),
        syn::Data::Struct(data_struct) => {
            if data_struct.fields.is_empty() {
                // Empty structs are easy to implement.
                let gen = quote! {
                    impl SyntaxEq for #name {
                        fn syntax_eq(&self, other: &Self) -> bool {
                            true
                        }
                    }
                };

                return gen.into()
            }

            let mut cmds = Vec::with_capacity(data_struct.fields.len());

            let mut unidentified_fields_count = 0; // For newtypes

            for field in data_struct.fields.iter() {
                if let Some(ident) = field.ident.as_ref() {
                    cmds.push(quote! {
                        state &= SyntaxEq::syntax_eq(&self.#ident, &other.#ident);
                    });
                } else {
                    let current_index = syn::Index::from(unidentified_fields_count);
                    cmds.push(quote! {
                        state &= SyntaxEq::syntax_eq(&self.#current_index, &other.#current_index);
                    });

                    unidentified_fields_count += 1;
                }
            }

            // Build the trait implementation
            let gen = quote! {
                impl #impl_generics SyntaxEq for #name #ty_generics #where_clause {
                    fn syntax_eq(&self, other: &Self) -> bool {
                        let mut state = true;

                        #(#cmds)*

                        state
                    }
                }
            };

            gen.into()
        },
    }
}
