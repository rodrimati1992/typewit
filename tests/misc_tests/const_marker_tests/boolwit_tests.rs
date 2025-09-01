use typewit::const_marker::{Bool, BoolWitG};
use typewit::TypeEq;

#[test]
fn is_true_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> bool {
        wit.is_true()
    }

    assert!(inner(BoolWitG::True(TypeEq::NEW)));
    assert!(!inner(BoolWitG::False(TypeEq::NEW)));
}

#[test]
fn is_false_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> bool {
        wit.is_false()
    }

    assert!(!inner(BoolWitG::True(TypeEq::NEW)));
    assert!(inner(BoolWitG::False(TypeEq::NEW)));
}

#[test]
fn to_true_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> Option<TypeEq<B, Bool<true>>> {
        wit.to_true()
    }

    assert_eq!(inner(BoolWitG::True(TypeEq::NEW)), Some(TypeEq::new::<Bool<true>>()));
    assert_eq!(inner(BoolWitG::False(TypeEq::NEW)), None);
}

#[test]
fn to_false_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> Option<TypeEq<B, Bool<false>>> {
        wit.to_false()
    }

    assert_eq!(inner(BoolWitG::True(TypeEq::NEW)), None);
    assert_eq!(inner(BoolWitG::False(TypeEq::NEW)), Some(TypeEq::new::<Bool<false>>()));
}


#[test]
fn unwrap_true_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<true>> {
        wit.unwrap_true()
    }

    assert_eq!(inner(BoolWitG::True(TypeEq::NEW)), TypeEq::new::<Bool<true>>());
}

#[test]
#[should_panic]
fn unwrap_true_on_false_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<true>> {
        wit.unwrap_true()
    }

    let _ = inner(BoolWitG::False(TypeEq::NEW));
}

#[test]
fn expect_true_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<true>> {
        wit.expect_true("what?")
    }

    assert_eq!(inner(BoolWitG::True(TypeEq::NEW)), TypeEq::new::<Bool<true>>());
}

#[test]
#[should_panic]
fn expect_true_on_false_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<true>> {
        wit.expect_true("oh no...")
    }

    let _ = inner(BoolWitG::False(TypeEq::NEW));
}


#[test]
fn unwrap_false_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<false>> {
        wit.unwrap_false()
    }

    assert_eq!(inner(BoolWitG::False(TypeEq::NEW)), TypeEq::new::<Bool<false>>());
}

#[test]
#[should_panic]
fn unwrap_false_on_true_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<false>> {
        wit.unwrap_false()
    }

    let _ = inner(BoolWitG::True(TypeEq::NEW));
}

#[test]
fn expect_false_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<false>> {
        wit.expect_false("what")
    }

    assert_eq!(inner(BoolWitG::False(TypeEq::NEW)), TypeEq::new::<Bool<false>>());
}

#[test]
#[should_panic]
fn expect_false_on_true_test() {
    const fn inner<B>(wit: BoolWitG<B>) -> TypeEq<B, Bool<false>> {
        wit.expect_false("what the ffff")
    }

    let _ = inner(BoolWitG::True(TypeEq::NEW));
}
