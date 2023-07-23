#![allow(unused_lifetimes)]

use crate::misc_tests::test_utils::assert_type;

use typewit::{HasTypeWitness, MakeTypeWitness, TypeEq};

use core::fmt::Debug;

mod stw_generics_parsing;

mod privacy {
    typewit::simple_type_witness!{
        pub(super) enum Foo { U8 = u8 }
    }
}

#[test]
fn test_privacy() {
    use privacy::Foo;
    let Foo::U8(_) = MakeTypeWitness::MAKE;
}


typewit::simple_type_witness!{
    enum Simplest { U8 = u8 }
}

#[test]
fn test_simplest() {
    fn _foo<T>(te: Simplest<T>) {
        // ensuring that the type parameter goes on the left
        let Simplest::U8(TypeEq::<T, u8>{..}) = te;
    }
    let Simplest::U8(_) = <Simplest<u8> as MakeTypeWitness>::MAKE;
}


//////////////////////////////
// Generic parameter parsing

typewit::simple_type_witness!{
    enum LifetimeBounds['a, 'b: 'a, 'c: 'b + 'a] {
        U8 = (&'a (), &'b (), &'c ()) 
    }
}

#[test]
fn lifetime_bounds() {
    fn _foo<'a, 'b: 'a, 'c: 'b>() {
        let _: LifetimeBounds<'a, 'b, 'c, _> = MakeTypeWitness::MAKE;
    }
}


typewit::simple_type_witness!{
    enum TypeParams['a, T, U:, V: 'a, W: 'a + 'a, X: Debug, Y: 'a + Debug] {
        U8 = (&'a (), T, U, V, W, X, Y) 
    }
}

#[test]
fn type_params() {
    fn _foo<'a, T, U, V: 'a, W: 'a + 'a, X: Debug, Y: 'a + Debug>() {
        let _: TypeParams<'a, T, U, V, W, X, Y, _> = MakeTypeWitness::MAKE;
    }
}

#[cfg(feature = "rust_1_61")]
typewit::simple_type_witness!{
    enum ConstParamsA['a, const X: usize] {
        U8 = &'a u8 
    }
}
#[cfg(feature = "rust_1_61")]
typewit::simple_type_witness!{
    enum ConstParamsB[const X: usize] {
        U8 = u8 
    }
}

#[cfg(feature = "rust_1_61")]
#[test]
fn const_params() {
    fn _foo<'a, const X: usize>() {
        let _: ConstParamsA<'a, X, _> = MakeTypeWitness::MAKE;
        let _: ConstParamsB<X, _> = MakeTypeWitness::MAKE;
    }
}

//////////////////////////////

pub struct WC;

typewit::simple_type_witness!{
    enum WhereClauseA[T]
    where []
    {
        U8 where[
            T: Copy,
            [(); {
                impl WC { const A: u8 = 3; }
                0
            }]:
        ] = T
    }
}

typewit::simple_type_witness!{
    enum WhereClauseB[T]
    where [ T: Debug] {
        U8 where[T: Copy] = T
    }
}

typewit::simple_type_witness!{
    enum WhereClauseC[T]
    where [T: Debug,]
    {
        U8 where[
            T: Copy, 
            [(); {
                impl WC { const C: u8 = 34; }
                0
            }]:
        ] = T
    }
}

#[test]
fn test_where_clause() {
    fn _a<T: Copy>() {
        let _: WhereClauseA<T, T> = MakeTypeWitness::MAKE;
    }
    fn _b_and_c<T: Debug + Copy>() {
        let _: WhereClauseB<T, T> = MakeTypeWitness::MAKE;
        let _: WhereClauseC<T, T> = MakeTypeWitness::MAKE;
    }

    assert_eq!(WC::A, 3);
    assert_eq!(WC::C, 34);
}



//////////////////////////////

#[cfg(feature = "rust_1_61")]
typewit::simple_type_witness!{
    enum ReplacedArgsConst[const N: usize] {
        U8[0] = u8,
        Array = [u8; N],
    }
}

#[cfg(feature = "rust_1_61")]
#[test]
fn replaced_args_const() {
    let te = ReplacedArgsConst::MAKE;
    let _: u8 = match te {
        ReplacedArgsConst::U8(te) => te.to_left(3),
        ReplacedArgsConst::Array(_) => unreachable!(),
    };
    assert_type::<_, ReplacedArgsConst<0, u8>>(te);
}

#[cfg(feature = "rust_1_61")]
typewit::simple_type_witness!{
    enum ReplacedArgsMore['a, T: 'a, const N: usize] {
        U8['a, (), 0] = u8,
        Opt['a, T, 1] = Option<T>,
        Array = [&'a T; N],
    }
}

#[cfg(feature = "rust_1_61")]
#[test]
fn replaced_args_more() {
    {
        let te = ReplacedArgsMore::MAKE;
        let _: u8 = match te {
            ReplacedArgsMore::U8(te) => te.to_left(3),
            _ => unreachable!(),
        };
        assert_type::<_, ReplacedArgsMore<(), 0, u8>>(te);
    }
    {
        let te = ReplacedArgsMore::MAKE;
        let _: Option<u64> = match te {
            ReplacedArgsMore::Opt(te) => te.to_left(None),
            _ => unreachable!(),
        };
        assert_type::<_, ReplacedArgsMore<u64, 1, Option<u64>>>(te);
    }
}


//////////////////////////////

typewit::simple_type_witness!{
    #[derive(Debug, PartialEq)]
    enum EnumAttr { U8 = u8 }
}

#[test]
fn enum_attr() {
    assert_eq!(EnumAttr::<u8>::MAKE, EnumAttr::<u8>::MAKE);
}

//////////////////////////////

#[cfg(feature = "rust_1_61")]
typewit::simple_type_witness!{
    enum FullEx['a, T, const N: usize] 
    where[ T: Copy ]
    {
        // the four permutations of:
        // - replacing the generic arguments 
        // - putting a where clause on the variant
        U8 = &'a u8,
        Opt['a, T, 0] where [T: Debug] = Option<T>,
        Array['a, (), N] = [u32; N],
        Tup where [T: Copy] = (Option<T>,),
    }
}

#[cfg(feature = "rust_1_61")]
#[test]
fn full_ex_test() {
    const fn func<'a, T, const N: usize, W>() -> W
    where
        T: Copy,
        W: HasTypeWitness<FullEx<'a, T, N, W>>
    {
        match W::WITNESS {
            FullEx::U8(te) => te.to_left(&5),
            FullEx::Opt(te) => te.to_left(None::<T>),
            FullEx::Array(te) => te.to_left([8; N]),
            FullEx::Tup(te) => te.to_left((None,)),
        }
    }

    {
        // have to specify all the generic arguments,
        // because they can't be inferred.
        let ret: &u8 = func::<(), 0, &u8>();
        assert_eq!(ret, &5);
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
        let ret: (Option<u32>,) = func::<_, 0, _>();
        assert_eq!(ret, (None::<u32>,));
    }
}

//////////////////////////////


typewit::simple_type_witness!{
    enum CfgedOut {
        #[cfg(all())]
        U8 = u8,
        /// Removed variant
        #[cfg(any())]
        U16 = u16,
    }
}


#[test]
fn cfged_out_varians() {
    fn cfged<T: HasTypeWitness<CfgedOut<T>>>() -> T {
        match T::WITNESS {
            CfgedOut::U8(te) => te.to_left(3),
        }
    }

    assert_eq!(cfged::<u8>(), 3);
}


//////////////////////////////


typewit::simple_type_witness!{
    derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)
    enum AllDerives {U8 = u8, U16 = u16}
}

struct NoImpls;

#[test]
fn test_all_derives() {
    use std::{
        cmp::{PartialEq, Eq, PartialOrd, Ord},
        fmt::Debug,
        hash::Hash,
    };

    fn assert_impld<T>()
    where 
        T: Debug + Copy + Clone + PartialEq + Eq + PartialOrd + Ord + Hash
    {}

    assert_impld::<AllDerives<NoImpls>>()
    
}


//////////////////////////////


// This ensures that all generic parameters can be used in the where clause,
#[cfg(feature = "rust_1_61")]
typewit::simple_type_witness!{
    derive(Copy, Clone)
    enum UsingGenericsInWhereClause<'a, T: 'a, const N: usize>
    where
        [&'a T; N]: Clone,
        __Wit: Clone,
    {
        U8 = [&'a T; N],
        U16['static, (), 0] 
        where 
            [&'a T; N]: Copy,
            __Wit: Copy,
        = u16,
    }
}


//////////////////////////////

#[test]
fn derives_and_cfg_on_variants() {
    typewit::simple_type_witness!{
        derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)
        enum AllDerives {
            #[cfg(all())]
            U8 = u8,
            /// docs above
            #[cfg(any())]
            U16 = u16,
            U32 = u32,
        }
    }

    fn _foo<T>(ad: AllDerives<T>) {
        match ad {
            AllDerives::U8{..} => {}
            AllDerives::U32{..} => {}
        }
    }

    assert_eq!(AllDerives::<u8>::MAKE, AllDerives::<u8>::MAKE);
}

//////////////////////////////


#[test]
fn cfg_attributes_on_generics() {
    {
        typewit::simple_type_witness!{
            enum AnyCfg[#[cfg(any())] T] {
                U8 = u8,
                U16 = u16,
            }
        }
        let _: AnyCfg<u8> = MakeTypeWitness::MAKE;
        let _: AnyCfg<u16> = MakeTypeWitness::MAKE;
    }
    {
        typewit::simple_type_witness!{
            enum AllCfg[#[cfg(all())] T] {
                U8<()> = u8,
                Generic = (T,),
            }

        }
        let _: AllCfg<(), u8> = MakeTypeWitness::MAKE;
        let _: AllCfg<u16, (u16,)> = MakeTypeWitness::MAKE;
    }
}


//////////////////////////////


#[test]
#[cfg(feature = "rust_1_61")]
fn defaulted_generic_param() {
    type Pair<T, U> = (T, U);

    macro_rules! test_case {
        (($($comma:tt)?)) => {
            {
                typewit::simple_type_witness!{
                    enum DefButOne[T, U = u8, const N: usize = 10] {
                        Arr[T, (), N $($comma)?] = [T; N],
                        U32<Pair<(), ()>$($comma)?> = u32,
                        Unary<Pair<i8, i16,>, U> = (U,),
                    }
                }

                fn make<T, U, const N: usize, Wit>(_: Wit) -> DefButOne<T, U, N, Wit> 
                where
                    DefButOne<T, U, N, Wit>: MakeTypeWitness
                {
                    MakeTypeWitness::MAKE
                }

                assert_type::<_, DefButOne<u64, (), 4, [u64; 4]>>(make([0u64; 4]));
                assert_type::<_, DefButOne<Pair<(), ()>, u8, 10, u32>>(make(0u32));
                assert_type::<_, DefButOne<Pair<i8, i16,>, bool, 10, (bool,)>>(make((false,)));
            }

            {
                typewit::simple_type_witness!{
                    enum DefAll[T = u8, const N: usize = 10] {
                        Arr[T, N $($comma)?] = [T; N],
                        U32<> = u32,
                        Unary<T $($comma)?> = (T,),
                    }
                }

                fn make<T, const N: usize, Wit>(_: Wit) -> DefAll<T, N, Wit> 
                where
                    DefAll<T, N, Wit>: MakeTypeWitness
                {
                    MakeTypeWitness::MAKE
                }


                assert_type::<_, DefAll<u64, 4, [u64; 4]>>(make([0u64; 4]));
                assert_type::<_, DefAll<u8, 10, u32>>(make(0u32));
                assert_type::<_, DefAll<bool, 10, (bool,)>>(make((false,)));
            }
        };
    }
    
    test_case!{(,)}
    test_case!{()}
}



