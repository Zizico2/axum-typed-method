use syn::parse::Parse;

pub(crate) trait Combine: Sized {
    fn combine(self, other: Self) -> syn::Result<Self>;
}

pub(crate) fn parse_attrs<T>(ident: &str, attrs: &[syn::Attribute]) -> syn::Result<T>
where
    T: Combine + Default + Parse,
{
    attrs
        .iter()
        .filter(|attr| attr.meta.path().is_ident(ident))
        .map(|attr| attr.parse_args::<T>())
        .try_fold(T::default(), |out, next| out.combine(next?))
}
