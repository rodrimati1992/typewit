// This module tests many possible permutations of syntax that the 
// simple_type_witness macro *could* parse incorrectly.
// 
// this mostly tests these parts of the syntax:
// - where clauses that don't wrap the predicates in `[]`
// - generic parameters of the enum

use typewit::{Identity, HasTypeWitness, MakeTypeWitness, simple_type_witness};

use std::{
    cmp::PartialEq,
    fmt::Debug,
};

type Type<T> = T;

trait Trivial {
    type Type: ?Sized;
}

impl<T: ?Sized> Trivial for T {
    type Type = T;
}


#[test]
fn empty_generics_trivial_where_clause() {
    macro_rules! test_case_inner {
        ([$($generics:tt)*] [$($vari_gens:tt)*]) => ({
            simple_type_witness! {
                enum TestCase $($generics)* {
                    A = u8,
                    B $($vari_gens)* = u16,
                }
            }

            let _: TestCase<u8> = MakeTypeWitness::MAKE;
            let _: TestCase<u16> = MakeTypeWitness::MAKE;
        });
    }

    macro_rules! test_case {
        ($($generics:tt)*) => (
            test_case_inner!{[] []}
            test_case_inner!{[$($generics)*] []}
            test_case_inner!{[] [$($generics)*]}
            test_case_inner!{[$($generics)*] [$($generics)*]}
        );
    }

    test_case!{}
    test_case!{<>}
    test_case!{<> where}
    test_case!{where}
    test_case!{where [u8; 0]:}


}


// ensurs that the where clause it terminated correctly, 
// even if `>` and `<` characters are tokenized as joint with other characters.
#[test]
fn lt_gt_token_permutations() {
    macro_rules! test_case {
        ($($generics:tt)*) => ({
            simple_type_witness! {
                enum TestCase $($generics)* {
                    A = u8,
                    B = Option<u16>,
                }
            }

            let _: TestCase<u8> = MakeTypeWitness::MAKE;
            let _: TestCase<Option<u16>> = MakeTypeWitness::MAKE;
        });
    }

    // // KEEP THE SPACES AROUND THE `<` AND `>`!
    test_case!{where Type< u8< >>:}
    test_case!{where Type< u8< > >:}
    test_case!{where <<u8 as Identity>::Type as Identity>::Type:}
    test_case!{where u8: Trivial<Type =< u8 as Identity>::Type >}
    test_case!{where u8: Trivial<Type<>= <u8 as Identity>::Type >}
    test_case!{where u8: Trivial<Type<>=<u8 as Identity>::Type >}
    test_case!{where u8: Trivial<Type<> =< u8 as Identity>::Type >}
    test_case!{where u8: Trivial<Type<> =<<u8 as Identity>::Type as Identity>::Type >}
    test_case!{where u8: Trivial<Type< >= <u8 as Identity>::Type >}
    test_case!{where u8: Trivial<Type< >=<u8 as Identity>::Type >}
}


// ensurs that braces inside the where clause work.
#[test]
fn parse_braced_const_param() {
    trait UsizeReq<const N: usize> {}

    impl UsizeReq<0> for () {}
    impl UsizeReq<1> for () {}
    impl UsizeReq<2> for () {}

    trait TypeReq<T> {}

    impl<T> TypeReq<T> for () {}

    struct Usize<const N: usize>;


    macro_rules! test_case {
        ($($bound:tt)*) => ({
            simple_type_witness! {
                enum TestCase<const N: usize> 
                where
                    (): $($bound)*
                {
                    A = [u8; N],
                    B = Option<u16>,
                }
            }

            let _: TestCase<0, [u8; 0]> = MakeTypeWitness::MAKE;
            let _: TestCase<1, Option<u16>> = MakeTypeWitness::MAKE;
        });
    }

    test_case!{UsizeReq<{ 1 + 1 }>}
    test_case!{UsizeReq<{ 1 + 1 },>}
    test_case!{TypeReq<Usize<{ 1 + 1 }>>}
}


#[test]
#[cfg(feature = "rust_1_61")]
fn all_supported_generics_syntax() {
    struct TestVariWhere;

    typewit::simple_type_witness!{
        enum FullExAngBrack<'a, 'b: 'a, T: 'a + Copy, const N: usize> 
        where
            T: PartialEq
        {
            // the four permutations of:
            // - replacing the generic arguments 
            // - putting a where clause on the variant
            U8 = &'a &'b u8,
            Opt<'a, 'b, T, 0> where T: Debug = Option<T>,
            Array<'a, 'b, (), N> = [u32; N],
            Tup where 
                T: IntoIterator<Item = u8>,
                [();{
                    impl TestVariWhere {
                        fn is_inside_where() -> u8 {
                            123
                        }
                    }
                    0
                }]:
            = (Option<T>,),
        }
    }

    const fn func<'a, 'b, T, const N: usize, W>() -> W
    where
        T: 'a + Copy + PartialEq,
        'b: 'a,
        W: HasTypeWitness<FullExAngBrack<'a, 'b, T, N, W>>
    {
        match W::WITNESS {
            FullExAngBrack::U8(te) => te.to_left(&&5),
            FullExAngBrack::Opt(te) => te.to_left(None::<T>),
            FullExAngBrack::Array(te) => te.to_left([8; N]),
            FullExAngBrack::Tup(te) => te.to_left((None,)),
        }
    }

    {
        // have to specify all the generic arguments,
        // because they can't be inferred.
        let ret: &&u8 = func::<(), 0, &&u8>();
        assert_eq!(ret, &&5);
    }
    {
        let ret: [u32; 2] = func();
        assert_eq!(ret, [8, 8]);
    }
    {
        let ret: Option<u32> = func();
        assert_eq!(ret, None::<u32>);
    }
    {
        let ret: (Option<Option<u8>>,) = func::<_, 0, _>();
        assert_eq!(ret, (None::<Option<u8>>,));
    }
    
    assert_eq!(TestVariWhere::is_inside_where(), 123);
}

