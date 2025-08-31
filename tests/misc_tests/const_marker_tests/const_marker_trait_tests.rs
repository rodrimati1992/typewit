use typewit::const_marker::{self as cm, ConstMarker, ConstMarkerOf};


/////////////////////////////////////////////////////////
// Making sure that ConstMarker can be used as the bound of an associated type

trait ConstAssocOf {
    type Of;
    type C: ConstMarkerOf<Self::Of>;
}

struct One;
struct Two;

impl ConstAssocOf for One {
    type Of = u8;
    type C = cm::U8<1>;
}

impl ConstAssocOf for Two {
    type Of = u16;
    type C = cm::U16<2>;
}

const fn generic_fn<T, C: ConstMarkerOf<T>>() -> T {
    <C as ConstMarker>::VAL
}
const fn const_assoc_fn<T: ConstAssocOf>() -> T::Of {
    generic_fn::<T::Of, T::C>()
}


#[test]
fn assoc_type_of_test() {
    assert_eq!(const_assoc_fn::<One>(), 1u8);
    assert_eq!(const_assoc_fn::<Two>(), 2u16);
}

/////////////////////////////////////////////////////////


