use std::{
    cmp::{PartialEq, Eq, PartialOrd, Ord},
    fmt::Debug,
    hash::Hash,
};

fn assert_all_impld<T>()
where 
    T: Debug + Copy + Clone + PartialEq + Eq + PartialOrd + Ord + Hash
{}

fn assert_cmp_impld<T>()
where 
    T: PartialEq + Eq + PartialOrd 
{}


struct NoImpls;


typewit::simple_type_witness!{
    derive()
    enum NoDerives {U8 = u8, U16 = u16}
}

typewit::simple_type_witness!{
    derive(PartialEq)
    enum OnlyPartialEq {U8 = u8, U16 = u16}
}

typewit::simple_type_witness!{
    derive(PartialEq, PartialOrd)
    enum OnlyPartialEqOrd {U8 = u8, U16 = u16}
}

typewit::simple_type_witness!{
    derive(PartialEq, Eq, PartialOrd)
    enum OnlyEqPartialOrd {U8 = u8, U16 = u16}
}

typewit::simple_type_witness!{
    derive(PartialEq, Eq, PartialOrd, Ord)
    enum AllCmpTraits {U8 = u8, U16 = u16}
}

fn main(){
    assert_all_impld::<NoDerives<NoImpls>>();

    assert_cmp_impld::<OnlyPartialEq<NoImpls>>();

    assert_cmp_impld::<OnlyPartialEqOrd<NoImpls>>();

    assert_cmp_impld::<OnlyEqPartialOrd<NoImpls>>();

    assert_all_impld::<AllCmpTraits<NoImpls>>();
}

