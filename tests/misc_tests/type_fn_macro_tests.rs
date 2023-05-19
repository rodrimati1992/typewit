use typewit::{CallFn, type_fn};

use crate::misc_tests::test_utils::AssertEq;

mod private {
    typewit::type_fn!{
        pub struct Pub;

        () => u8;
        u8 => u16;
    }
}

#[test]
fn test_nested_for_binder() {
    typewit::type_fn!{
        struct NestedForBinder;

        for[T] (for<'a> fn(&'a T)) => T
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
            type_fn!{
                struct Func[T $($comma_struct_generics)?]
                where[T: Copy $($comma_struct_where)?];

                for[U $($comma_fn_generics)?] U => (T, U)
                where[T: Clone $($comma_fn_where)?]
            }

            let _: AssertEq<CallFn<Func<u8>, u16>, (u8, u16)>;
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

#[test]
fn test_all_generics_are_usable() {
    type_fn!{
        struct AllGenerics['a, T, const N: usize];

        for['b, U, const M: usize] ([&'a T; N], [&'b U; M]) => [&'b U; N]
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
    {
        type_fn!{
            struct StructBound[T: IntoIterator];

            () => T::Item
        }
        let _: AssertEq<CallFn<StructBound<Vec<u8>>, ()>, u8>;
    }
    {
        type_fn!{
            struct StructWhereBound[T]
            where[T: IntoIterator];

            () => T::Item
        }
        let _: AssertEq<CallFn<StructWhereBound<Vec<u8>>, ()>, u8>;
    }
    {
        type_fn!{
            struct FnBound;

            for[T: IntoIterator] T => T::Item
        }
        let _: AssertEq<CallFn<FnBound, Vec<u16>>, u16>;
    }
    {
        type_fn!{
            struct FnWhereBound;

            for[T] T => T::Item
            where[T: IntoIterator]
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

                    () => u8;
                    u8 => u16 $($semi)?
                }

                let _: AssertEq<CallFn<WithoutFor, ()>, u8>;
                let _: AssertEq<CallFn<WithoutFor, u8>, u16>;
            }
            {
                type_fn!{
                    struct WitFor;

                    () => u8;
                    for[T] (T,) => Vec<T> $($semi)?
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
        () => u8;

        #[cfg(all())]
        for[T] (T,) => T;

        // If this was included, it would error due to being an overlapping impl
        #[cfg(any())]
        for[T] T => Vec<T>;
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
fn test_construction() {
    let _: private::Pub = private::Pub::NEW;

    typewit::type_fn!{
        struct Unit;

        () => u8;
        u8 => u16;
    }
    typewit::type_fn!{
        struct WithLtParam['a];

        () => u8;
        u8 => u16;
    }
    typewit::type_fn!{
        struct WithTyParam[T];

        () => u8;
        u8 => u16;
    }
    typewit::type_fn!{
        struct WithConstParam[const N: usize];

        () => u8;
        u8 => u16;
    }

    let Unit = Unit::NEW;
    let Unit = Unit;
    let WithLtParam(_) = WithLtParam::NEW;
    let WithTyParam(_) = WithTyParam::<u8>::NEW;
    let WithConstParam = WithConstParam::<0>::NEW;
    let WithConstParam = WithConstParam::<0>;

}



