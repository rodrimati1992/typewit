use typewit::{CallFn, type_fn};

use crate::misc_tests::test_utils::AssertEq;

mod private {
    typewit::type_fn!{
        pub struct Pub;

        impl () => u8;
        impl u8 => u16;
    }
}

#[test]
fn test_nested_for_binder() {
    typewit::type_fn!{
        struct NestedForBinder;

        impl<T> (for<'a> fn(&'a T)) => T
    }

    let _: AssertEq<CallFn<NestedForBinder, fn(&u8)>, u8>;

}


#[test]
fn test_trailing_commas() {
    macro_rules! test_case {
        (
            ($($comma_struct_generics:tt)?)
            ($($comma_struct_where:tt)?)
            ($($comma_fn_generics:tt)?)
            ($($comma_fn_where:tt)?)
        ) => ({
            {
                type_fn!{
                    struct TrailingLt<'a $($comma_struct_generics)?>;

                    impl<'b $($comma_fn_generics)?> &'b () => (&'a u8, &'b u16)
                }

                fn _takes_lt<'c, 'd>() {
                    let _: AssertEq<
                        CallFn<TrailingLt<'c>, &'d ()>,
                        (&'c u8, &'d u16),
                    >;
                }
            }
            {
                type_fn!{
                    struct TrailingTy<T $($comma_struct_generics)?>
                    where T: IntoIterator $($comma_struct_where)?;
    
                    impl<U $($comma_fn_generics)?> U => (T::IntoIter, U::Item)
                    where U: Iterator $($comma_fn_where)?
                }
    
                let _: AssertEq<
                    CallFn<TrailingTy<Vec<u8>>, std::ops::Range<usize>>,
                    (std::vec::IntoIter<u8>, usize),
                >;
            }
            {
                type_fn!{
                    struct TrailingConst<const N: usize $($comma_struct_generics)?>;
    
                    impl<const M: usize$($comma_fn_generics)?> [u8; M] => ([(); N], [(); M])
                }
    
                let _: AssertEq<
                    CallFn<TrailingConst<3>, [u8; 5]>,
                    ([(); 3], [(); 5]),
                >;
            }
        })
    }

    // generated with:
    // let perms = [",", " "];
    // for (a, b, c, d) in itertools::iproduct!(perms, perms, perms, perms) {
    //     println!("test_case!{{({a})({b})({c})({d})}}");
    // }
    test_case!{(,)(,)(,)(,)}
    test_case!{(,)(,)(,)( )}
    test_case!{(,)(,)( )(,)}
    test_case!{(,)(,)( )( )}
    test_case!{(,)( )(,)(,)}
    test_case!{(,)( )(,)( )}
    test_case!{(,)( )( )(,)}
    test_case!{(,)( )( )( )}
    test_case!{( )(,)(,)(,)}
    test_case!{( )(,)(,)( )}
    test_case!{( )(,)( )(,)}
    test_case!{( )(,)( )( )}
    test_case!{( )( )(,)(,)}
    test_case!{( )( )(,)( )}
    test_case!{( )( )( )(,)}
    test_case!{( )( )( )( )}

}

#[cfg(feature = "rust_1_61")]
#[test]
fn test_all_generics_are_usable() {
    type_fn!{
        struct AllGenerics<'a, T, const N: usize>;

        impl<'b, U, const M: usize> ([&'a T; N], [&'b U; M]) => [&'b U; N]
    }

    fn _with_lifetimes<'a, 'b>() {
        let _: AssertEq<
            CallFn<AllGenerics<'a, u8, 1>, ([&'a u8; 1], [&'b u16; 2])>,
            [&'b u16; 1]
        >;

        // Inferring the first tuple element type, since it's determined by AllGenerics.
        let _: AssertEq<
            CallFn<AllGenerics<'a, u8, 1>, (_, [&'b u16; 2])>,
            [&'b u16; 1]
        >;
    }
}

#[test]
fn test_that_bounds_are_included() {
    use std::ops::Add;

    type Type<T> = T;

    {
        type_fn!{
            struct StructBound<T: IntoIterator>;

            impl () => T::Item
        }
        let _: AssertEq<CallFn<StructBound<Vec<u8>>, ()>, u8>;
    }
    {
        type Array<T, const N: usize> = [T; N];
        type_fn!{
            struct StructDeepBound0<T: IntoIterator<Item = Array<u8, N>>, const N: usize>;

            impl () => T::IntoIter
        }
        let _: AssertEq<
            CallFn<StructDeepBound0<Vec<[u8; 5]>, 5>, ()>,
            std::vec::IntoIter<[u8; 5]>,
        >;
    }
    {
        type_fn!{
            // Testing the presence of the `<<` and `>>` tokens in the middle of bounds,
            struct StructDeepBound1<T: Add<Type<<[u8; 1] as IntoIterator>::Item>>,>;

            impl () => T::Output
        }
        let _: AssertEq<CallFn<StructDeepBound1<&u8>, ()>, u8>;
    }
    {
        type_fn!{
            // Testing the presence of `>>` token at the end of bounds,
            struct StructDeepBound2<T: Add<Type<u16>> >;

            impl () => T::Output
        }
        let _: AssertEq<CallFn<StructDeepBound2<&u16>, ()>, u16>;
    }
    {
        type_fn!{
            // Testing the presence of `>>` token at the end of generics,
            struct StructDeepBound3<T: Add<Type<u16> >>;

            impl () => T::Output
        }
        let _: AssertEq<CallFn<StructDeepBound3<&u16>, ()>, u16>;
    }
    {
        type_fn!{
            struct StructWhereBound<T>
            where T: IntoIterator;

            impl () => T::Item
        }
        let _: AssertEq<CallFn<StructWhereBound<Vec<u8>>, ()>, u8>;
    }
    {
        type_fn!{
            struct FnBound;

            impl<T: IntoIterator> T => T::Item
        }
        let _: AssertEq<CallFn<FnBound, Vec<u16>>, u16>;
    }
    {
        type_fn!{
            struct FnWhereBound;

            impl<T> T => T::Item
            where T: IntoIterator
        }
        let _: AssertEq<CallFn<FnWhereBound, Vec<u16>>, u16>;
    }
}

#[test]
fn test_multifunc() {
    macro_rules! test_cases {
        ($($semi:tt)?) => {
            {
                type_fn!{
                    struct WithoutFor;

                    impl () => u8;
                    impl u8 => u16 $($semi)?
                }

                let _: AssertEq<CallFn<WithoutFor, ()>, u8>;
                let _: AssertEq<CallFn<WithoutFor, u8>, u16>;
            }
            {
                type_fn!{
                    struct WitFor;

                    impl () => u8;
                    impl<T> (T,) => Vec<T> $($semi)?
                }

                let _: AssertEq<CallFn<WitFor, ()>, u8>;
                let _: AssertEq<CallFn<WitFor, (bool,)>, Vec<bool>>;
            }
        };
    }

    test_cases!{}
    test_cases!{;}
}

#[test]
fn test_attributes() {
    type_fn!{
        #[derive(PartialEq)]
        struct Foo;

        #[cfg(all())]
        impl () => u8;

        #[cfg(all())]
        impl<T> (T,) => T;

        // If this was included, it would error due to being an overlapping impl
        #[cfg(any())]
        impl<T> T => Vec<T>;
    }

    assert!(Foo == Foo);

    let _: AssertEq<CallFn<Foo, ()>, u8>;
    let _: AssertEq<CallFn<Foo, (u16,)>, u16>;

}

#[test]
fn test_vis() {
    let _: AssertEq<CallFn<private::Pub, ()>, u8>;
    let _: AssertEq<CallFn<private::Pub, u8>, u16>;
}


#[test]
fn test_empty_everything() {
    {
        typewit::type_fn! {
            struct Empty<>where;
            impl<> i8 => i16
            where
        }

        let _: AssertEq<CallFn<Empty, i8>, i16>;
    }
    {
        typewit::type_fn! {
            struct EmptyBounds<'a:, T:>where;
            impl<'b:,U:> (&'b (), U) => (&'a u8, &'b u16, T, U)
            where
        }

        fn _with_lifetimes<'a, 'b>() {
            let _: AssertEq<
                CallFn<EmptyBounds<'a, bool>, (&'b (), char)>, 
                (&'a u8, &'b u16, bool, char),
            >;
        }
    }
}


#[test]
fn test_multiple_lifetimes() {
    type_fn!{
        struct WithLts<'a, 'b: 'a, 'c: 'a + 'static, T>;

        impl () => ()
    }

    fn _with_lifetime<'a, 'b: 'a>() {
        let _: WithLts<'a, 'b, 'static, ()>;
    }
}

#[test]
fn test_comma_between_generic_kinds() {
    {
        type_fn!{
            struct WithLt<'a, const S: usize>;

            impl<'b , const Z: usize> [&'b (); Z] => ([&'a u8; S], [&'b u16; Z])
        }

        fn _takes_lt<'c, 'd>() {
            let _: AssertEq<
                CallFn<WithLt<'c, 3>, [&'d (); 5]>,
                ([&'c u8; 3], [&'d u16; 5]),
            >;
        }
    }
    #[cfg(feature = "rust_1_61")]
    {
        type_fn!{
            struct WithTy<T, const S: usize>;

            impl<U , const Z: usize> [U; Z] => ([T; Z], [U; S])
        }

        let _: AssertEq<CallFn<WithTy<u8, 3>, [u16; 5]>, ([u8; 5], [u16; 3])>;
    }
    {
        struct Pair<const A: usize, const B: usize>;
        type_fn!{
            struct WithConst<const N: usize, const S: usize>;

            impl<const M: usize, const Z: usize> Pair<M, Z> => (Pair<N, M>, Pair<S, Z>)
        }

        let _: AssertEq<
            CallFn<WithConst<3, 5>, Pair<8, 13>>,
            (Pair<3, 8>, Pair<5, 13>),
        >;
    }
}

#[test]
fn test_defaulted_params() {
    {
        type_fn!{
            struct TyWithBound<T: IntoIterator = Vec<u8>>;

            impl () => T::Item
        }

        let _: AssertEq<TyWithBound, TyWithBound<Vec<u8>>>;
        let _: AssertEq<CallFn<TyWithBound, ()>, u8>;
        let _: AssertEq<CallFn<TyWithBound<>, ()>, u8>;
        let _: AssertEq<CallFn<TyWithBound<Vec<u16>>, ()>, u16>;
    }
    {
        type_fn!{
            struct TyWithoutBound<T = u8>;

            impl () => (T, T)
        }

        let _: AssertEq<TyWithoutBound, TyWithoutBound<u8>>;
        let _: AssertEq<CallFn<TyWithoutBound, ()>, (u8, u8)>;
        let _: AssertEq<CallFn<TyWithoutBound<>, ()>, (u8, u8)>;
        let _: AssertEq<CallFn<TyWithoutBound<isize>, ()>, (isize, isize)>;
    }
    #[cfg(feature = "rust_1_61")]
    {
        type_fn!{
            struct WithConst<const N: usize = 3>;

            impl<T> T => [T; N]
        }

        let _: AssertEq<WithConst, WithConst<3>>;
        let _: AssertEq<CallFn<WithConst, bool>, [bool; 3]>;
        let _: AssertEq<CallFn<WithConst<>, usize>, [usize; 3]>;
        let _: AssertEq<CallFn<WithConst<8>, u16>, [u16; 8]>;
    }
}

#[test]
fn test_construction() {
    let _: private::Pub = private::Pub::NEW;

    typewit::type_fn!{
        struct Unit;

        impl () => u8;
        impl u8 => u16;
    }
    typewit::type_fn!{
        struct WithLtParam<'a>;

        impl () => u8;
        impl u8 => u16;
    }
    typewit::type_fn!{
        struct WithTyParam<T>;

        impl () => u8;
        impl u8 => u16;
    }
    typewit::type_fn!{
        struct WithConstParam<const N: usize>;

        impl () => u8;
        impl u8 => u16;
    }

    let Unit = Unit::NEW;
    let Unit = Unit;
    let WithLtParam(_) = WithLtParam::NEW;
    let WithTyParam(_) = WithTyParam::<u8>::NEW;
    let WithConstParam = WithConstParam::<0>::NEW;
    let WithConstParam = WithConstParam::<0>;

}



