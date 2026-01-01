use proc_macro::TokenStream;
use quote::ToTokens;
use quote::quote;
use syn::parse::Parse;

mod attr_parsing;

/// Derive an implementation of [`axum_extra::routing::TypedPath`].
///
/// See that trait for more details.
///
/// [`axum_extra::routing::TypedPath`]: https://docs.rs/axum-extra/latest/axum_extra/routing/trait.TypedPath.html
#[proc_macro_derive(TypedMethod, attributes(typed_method))]
pub fn derive_typed_method(input: TokenStream) -> TokenStream {
    expand_with(input, typed_method::expand)
}

fn expand_with<F, I, K>(input: TokenStream, f: F) -> TokenStream
where
    F: FnOnce(I) -> syn::Result<K>,
    I: Parse,
    K: ToTokens,
{
    expand(syn::parse(input).and_then(f))
}

fn expand<T>(result: syn::Result<T>) -> TokenStream
where
    T: ToTokens,
{
    match result {
        Ok(tokens) => {
            let tokens = (quote! { #tokens }).into();
            if std::env::var_os("AXUM_MACROS_DEBUG").is_some() {
                eprintln!("{tokens}");
            }
            tokens
        }
        Err(err) => err.into_compile_error().into(),
    }
}

mod typed_method {
    use proc_macro2::{Span, TokenStream};
    use quote::{quote, quote_spanned};
    use syn::{ItemStruct, Token, parse::Parse, spanned::Spanned};

    use super::attr_parsing::Combine;

    // TODO: this should be a fork of axum_macros
    pub(crate) fn expand(item_struct: ItemStruct) -> syn::Result<TokenStream> {
        let ItemStruct {
            attrs,
            ident,
            generics,
            fields,
            ..
        } = &item_struct;

        if !generics.params.is_empty() || generics.where_clause.is_some() {
            return Err(syn::Error::new_spanned(
                generics,
                "`#[derive(TypedMethod)]` doesn't support generics",
            ));
        }

        let Attrs { method_filter } = super::attr_parsing::parse_attrs("typed_method", attrs)?;

        let method_filter = method_filter.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "Missing method filter: `#[typed_method(\"GET\")]`",
            )
        })?;

        let typed_path_impl = quote_spanned! {method_filter.span()=>
            #[automatically_derived]
            impl ::axum_typed_method::TypedMethod for #ident {
                const METHOD: ::axum::routing::MethodFilter = #method_filter;
            }
        };

        Ok(quote! (#typed_path_impl))
    }

    mod kw {
        syn::custom_keyword!(rejection);
    }

    #[derive(Default)]
    struct Attrs {
        method_filter: Option<syn::Path>,
    }

    impl Parse for Attrs {
        fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
            let mut method_filter = None;

            while !input.is_empty() {
                let lh = input.lookahead1();
                if input.peek(Token![crate])
                    || input.peek(Token![self])
                    || input.peek(Token![super])
                    || input.peek(Token![::])
                    || input.peek(syn::Ident)
                {
                    method_filter = Some(input.parse()?);
                } else {
                    return Err(lh.error());
                }

                let _ = input.parse::<Token![,]>();
            }

            Ok(Self { method_filter })
        }
    }

    impl Combine for Attrs {
        fn combine(mut self, other: Self) -> syn::Result<Self> {
            let Self { method_filter } = other;
            if let Some(method_filter) = method_filter {
                if self.method_filter.is_some() {
                    return Err(syn::Error::new_spanned(
                        method_filter,
                        "method filter specified more than once",
                    ));
                }
                self.method_filter = Some(method_filter);
            }

            Ok(self)
        }
    }

    // TODO
    // #[test]
    // fn ui() {
    //     crate::run_ui_tests("typed_path");
    // }
}
