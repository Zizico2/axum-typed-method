use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse::Parse;
use quote::quote;

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
    use quote::{format_ident, quote, quote_spanned};
    use syn::{Ident, ItemStruct, LitStr, Token, parse::Parse, spanned::Spanned};

    use super::attr_parsing::{Combine, combine_attribute, parse_parenthesized_attribute, second};

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

        let Attrs {
            method_filter,
            rejection,
        } = super::attr_parsing::parse_attrs("typed_method", attrs)?;

        let method_filter = method_filter.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "Missing method filter: `#[typed_method(\"GET\")]`",
            )
        })?;

        let rejection = rejection.map(second);

        match fields {
            syn::Fields::Named(_) => Ok(expand_named_fields(ident, method_filter, rejection)),
            syn::Fields::Unnamed(fields) => {
                expand_unnamed_fields(fields, ident, method_filter, rejection)
            }
            syn::Fields::Unit => expand_unit_fields(ident, method_filter, rejection),
        }
    }

    mod kw {
        syn::custom_keyword!(rejection);
    }

    #[derive(Default)]
    struct Attrs {
        method_filter: Option<syn::Path>,
        rejection: Option<(kw::rejection, syn::Path)>,
    }

    impl Parse for Attrs {
        fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
            let mut method_filter = None;
            let mut rejection = None;

            while !input.is_empty() {
                let lh = input.lookahead1();
                if lh.peek(LitStr) {
                    method_filter = Some(input.parse()?);
                } else if lh.peek(Ident) {
                    method_filter = Some(input.parse()?);
                } else if lh.peek(kw::rejection) {
                    parse_parenthesized_attribute(input, &mut rejection)?;
                } else {
                    return Err(lh.error());
                }

                let _ = input.parse::<Token![,]>();
            }

            Ok(Self {
                method_filter,
                rejection,
            })
        }
    }

    impl Combine for Attrs {
        fn combine(mut self, other: Self) -> syn::Result<Self> {
            let Self { method_filter, rejection } = other;
            if let Some(method_filter) = method_filter {
                if self.method_filter.is_some() {
                    return Err(syn::Error::new_spanned(
                        method_filter,
                        "method filter specified more than once",
                    ));
                }
                self.method_filter = Some(method_filter);
            }
            combine_attribute(&mut self.rejection, rejection)?;
            Ok(self)
        }
    }

    fn expand_named_fields(
        ident: &syn::Ident,
        method_filter: syn::Path,
        rejection: Option<syn::Path>,
    ) -> TokenStream {
        let typed_path_impl = quote_spanned! {method_filter.span()=>
            #[automatically_derived]
            impl ::axum_extra::routing::TypedMethod for #ident {
                const METHOD: ::axum::routing::MethodFilter = #method_filter;
            }
        };

        // TODO
        // let display_impl = quote_spanned! {method_filter.span()=>
        //     #[automatically_derived]
        //     impl ::std::fmt::Display for #ident {
        //         #[allow(clippy::unnecessary_to_owned)]
        //         fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        //             let Self { #(#captures,)* } = self;
        //             write!(
        //                 f,
        //                 #format_str,
        //                 #(
        //                     #captures = ::axum_extra::__private::utf8_percent_encode(
        //                         &#captures.to_string(),
        //                         ::axum_extra::__private::PATH_SEGMENT,
        //                     )
        //                 ),*
        //             )
        //         }
        //     }
        // };

        let rejection_assoc_type = rejection_assoc_type(&rejection);
        let map_err_rejection = map_err_rejection(&rejection);

        let from_request_impl = quote! {
            #[automatically_derived]
            impl<S> ::axum::extract::FromRequestParts<S> for #ident
            where
                S: Send + Sync,
            {
                type Rejection = #rejection_assoc_type;

                async fn from_request_parts(
                    parts: &mut ::axum::http::request::Parts,
                    state: &S,
                ) -> ::std::result::Result<Self, Self::Rejection> {
                    ::axum::extract::Path::from_request_parts(parts, state)
                        .await
                        .map(|path| path.0)
                        #map_err_rejection
                }
            }
        };

        quote! {
            #typed_path_impl
            // TODO
            // #display_impl
            #from_request_impl
        }
    }

    fn expand_unnamed_fields(
        fields: &syn::FieldsUnnamed,
        ident: &syn::Ident,
        method_filter: syn::Path,
        rejection: Option<syn::Path>,
    ) -> syn::Result<TokenStream> {
        let typed_path_impl = quote_spanned! {method_filter.span()=>
            #[automatically_derived]
            impl ::axum_extra::routing::TypedMethod for #ident {
                const METHOD: ::axum::routing::MethodFilter = #method_filter;
            }
        };

        // TODO
        // let display_impl = quote_spanned! {method_filter.span()=>
        //     #[automatically_derived]
        //     impl ::std::fmt::Display for #ident {
        //         #[allow(clippy::unnecessary_to_owned)]
        //         fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        //             let Self { #(#destructure_self)* } = self;
        //             write!(
        //                 f,
        //                 #format_str,
        //                 #(
        //                     #captures = ::axum_extra::__private::utf8_percent_encode(
        //                         &#captures.to_string(),
        //                         ::axum_extra::__private::PATH_SEGMENT,
        //                     )
        //                 ),*
        //             )
        //         }
        //     }
        // };

        let rejection_assoc_type = rejection_assoc_type(&rejection);
        let map_err_rejection = map_err_rejection(&rejection);

        let from_request_impl = quote! {
            #[automatically_derived]
            impl<S> ::axum::extract::FromRequestParts<S> for #ident
            where
                S: Send + Sync,
            {
                type Rejection = #rejection_assoc_type;

                async fn from_request_parts(
                    parts: &mut ::axum::http::request::Parts,
                    state: &S,
                ) -> ::std::result::Result<Self, Self::Rejection> {
                    ::axum::extract::Path::from_request_parts(parts, state)
                        .await
                        .map(|path| path.0)
                        #map_err_rejection
                }
            }
        };

        Ok(quote! {
            #typed_path_impl
            // TODO
            // #display_impl
            #from_request_impl
        })
    }

    fn simple_pluralize(count: usize, word: &str) -> String {
        if count == 1 {
            format!("{count} {word}")
        } else {
            format!("{count} {word}s")
        }
    }

    fn expand_unit_fields(
        ident: &syn::Ident,
        method_filter: syn::Path,
        rejection: Option<syn::Path>,
    ) -> syn::Result<TokenStream> {
        let typed_path_impl = quote_spanned! {method_filter.span()=>
            #[automatically_derived]
            impl ::axum_extra::routing::TypedMethod for #ident {
                const METHOD: ::axum::routing::MethodFilter = #method_filter;
            }
        };

        let display_impl = quote_spanned! {method_filter.span()=>
            #[automatically_derived]
            impl ::std::fmt::Display for #ident {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    write!(f, #method_filter)
                }
            }
        };

        let rejection_assoc_type = if let Some(rejection) = &rejection {
            quote! { #rejection }
        } else {
            quote! { ::axum::http::StatusCode }
        };
        let create_rejection = if let Some(rejection) = &rejection {
            quote! {
                Err(<#rejection as ::std::default::Default>::default())
            }
        } else {
            quote! {
                Err(::axum::http::StatusCode::NOT_FOUND)
            }
        };

        let from_request_impl = quote! {
            #[automatically_derived]
            impl<S> ::axum::extract::FromRequestParts<S> for #ident
            where
                S: Send + Sync,
            {
                type Rejection = #rejection_assoc_type;

                async fn from_request_parts(
                    parts: &mut ::axum::http::request::Parts,
                    _state: &S,
                ) -> ::std::result::Result<Self, Self::Rejection> {
                    if parts.uri.path() == <Self as ::axum_extra::routing::TypedMethod>::PATH {
                        Ok(Self)
                    } else {
                        #create_rejection
                    }
                }
            }
        };

        Ok(quote! {
            #typed_path_impl
            #display_impl
            #from_request_impl
        })
    }

    fn format_str_from_path(segments: &[Segment]) -> String {
        segments
            .iter()
            .map(|segment| match segment {
                Segment::Capture(capture, _) => format!("{{{capture}}}"),
                Segment::Static(segment) => segment.to_owned(),
            })
            .collect::<Vec<_>>()
            .join("/")
    }

    fn captures_from_path(segments: &[Segment]) -> Vec<syn::Ident> {
        segments
            .iter()
            .filter_map(|segment| match segment {
                Segment::Capture(capture, span) => Some(format_ident!("{}", capture, span = *span)),
                Segment::Static(_) => None,
            })
            .collect::<Vec<_>>()
    }

    fn parse_path(path: &LitStr) -> syn::Result<Vec<Segment>> {
        let value = path.value();
        if value.is_empty() {
            return Err(syn::Error::new_spanned(
                path,
                "paths must start with a `/`. Use \"/\" for root routes",
            ));
        } else if !path.value().starts_with('/') {
            return Err(syn::Error::new_spanned(path, "paths must start with a `/`"));
        }

        path.value()
            .split('/')
            .map(|segment| {
                if let Some(capture) = segment
                    .strip_prefix('{')
                    .and_then(|segment| segment.strip_suffix('}'))
                    .and_then(|segment| {
                        (!segment.starts_with('{') && !segment.ends_with('}')).then_some(segment)
                    })
                    .map(|capture| capture.strip_prefix('*').unwrap_or(capture))
                {
                    Ok(Segment::Capture(capture.to_owned(), path.span()))
                } else {
                    Ok(Segment::Static(segment.to_owned()))
                }
            })
            .collect()
    }

    enum Segment {
        Capture(String, Span),
        Static(String),
    }

    fn path_rejection() -> TokenStream {
        quote! {
            <::axum::extract::Path<Self> as ::axum::extract::FromRequestParts<S>>::Rejection
        }
    }

    fn rejection_assoc_type(rejection: &Option<syn::Path>) -> TokenStream {
        match rejection {
            Some(rejection) => quote! { #rejection },
            None => path_rejection(),
        }
    }

    fn map_err_rejection(rejection: &Option<syn::Path>) -> TokenStream {
        rejection
            .as_ref()
            .map(|rejection| {
                let path_rejection = path_rejection();
                quote! {
                    .map_err(|rejection| {
                        <#rejection as ::std::convert::From<#path_rejection>>::from(rejection)
                    })
                }
            })
            .unwrap_or_default()
    }

    // TODO
    // #[test]
    // fn ui() {
    //     crate::run_ui_tests("typed_path");
    // }
}
