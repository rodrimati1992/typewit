use typewit::TypeCmp;
use typewit::const_marker::{self as cm, ConstMarker, ConstMarkerEq, Char, U8};



#[test]
fn equals_works_with_generic_of_type() {
    const fn call_equals<Of, L, R>(l: &L, r: &R) -> TypeCmp<L, R>
    where
        L: ConstMarkerEq<Of = Of>,
        R: ConstMarkerEq<Of = Of>,
    {
        cm::equals(l, r)
    }

    assert!(matches!(call_equals(&Char::<'3'>, &Char::<'3'>), TypeCmp::Eq(_)));
    assert!(matches!(call_equals(&Char::<'3'>, &Char::<'5'>), TypeCmp::Ne(_)));

    assert!(matches!(call_equals(&U8::<3>, &U8::<3>), TypeCmp::Eq(_)));
    assert!(matches!(call_equals(&U8::<3>, &U8::<5>), TypeCmp::Ne(_)));

}

#[test]
fn cm_equals_works_with_generic_of_type() {
    const fn call_equals<Of, L, R>(_l: &L, _r: &R) -> TypeCmp<L, R>
    where
        L: ConstMarkerEq<Of = Of>,
        R: ConstMarkerEq<Of = Of>,
    {
        cm::CmEquals::<L, R>::VAL
    }

    assert!(matches!(call_equals(&Char::<'3'>, &Char::<'3'>), TypeCmp::Eq(_)));
    assert!(matches!(call_equals(&Char::<'3'>, &Char::<'5'>), TypeCmp::Ne(_)));

    assert!(matches!(call_equals(&U8::<3>, &U8::<3>), TypeCmp::Eq(_)));
    assert!(matches!(call_equals(&U8::<3>, &U8::<5>), TypeCmp::Ne(_)));

}












