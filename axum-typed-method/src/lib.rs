pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

pub mod router_ext {
    use axum::{
        Router,
        routing::{MethodFilter, on},
    };
    use axum_extra::routing::{SecondElementIs, TypedPath};

    /// Extension trait that adds additional methods to [`Router`].
    #[allow(clippy::return_self_not_must_use)]
    pub trait RouterExt<S>: sealed::Sealed {
        fn typed<H, T, P>(self, handler: H) -> Self
        where
            H: axum::handler::Handler<T, S>,
            T: SecondElementIs<P> + 'static,
            P: TypedMethod;

        // /// Add a typed `GET` route to the router.
        // ///
        // /// The path will be inferred from the first argument to the handler function which must
        // /// implement [`TypedPath`].
        // ///
        // /// See [`TypedPath`] for more details and examples.

        // fn typed_get<H, T, P>(self, handler: H) -> Self
        // where
        //     H: axum::handler::Handler<T, S>,
        //     T: SecondElementIs<P> + 'static,
        //     P: TypedPath;

        // /// Add a typed `DELETE` route to the router.
        // ///
        // /// The path will be inferred from the first argument to the handler function which must
        // /// implement [`TypedPath`].
        // ///
        // /// See [`TypedPath`] for more details and examples.
    }

    impl<S> RouterExt<S> for Router<S>
    where
        S: Clone + Send + Sync + 'static,
    {
        fn typed<H, T, P>(self, handler: H) -> Self
        where
            H: axum::handler::Handler<T, S>,
            T: SecondElementIs<P> + 'static,
            P: TypedMethod,
        {
            self.route(P::PATH, on(P::METHOD, handler))
        }
    }
    mod sealed {
        pub trait Sealed {}
        impl<S> Sealed for axum::Router<S> {}
    }
    pub trait TypedMethod: TypedPath {
        const METHOD: MethodFilter;
    }
}
pub use router_ext::{RouterExt, TypedMethod};
