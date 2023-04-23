use typewit::{
    type_fn::{GRef, GRefMut},
    TypeEq,
};

use crate::misc_tests::test_utils::{assert_type, assert_type_eq};

const fn _to_left_to_right<L, R>(te: TypeEq<L, R>, l: L, r: R) {
    let l2: L = te.to_left(r);
    let r2: R = te.to_right(l);
    std::mem::forget(l2);
    std::mem::forget(r2);
}

#[test]
fn to_left_to_right() {
    const TE: TypeEq<String, String> = TypeEq::new();
    assert_eq!(TE.to_left("3".to_string()), "3");
    assert_eq!(TE.to_right("5".to_string()), "5");
}

#[test]
fn flip_method() {
    fn flipper<L, R>(te: TypeEq<L, R>) {
        assert_type_eq(te.flip(), te);
        let _ = |te: TypeEq<u8, u16>| -> TypeEq<u16, u8> { te.flip() };
    }
    flipper(TypeEq::new::<u8>());

    const TE: TypeEq<String, String> = TypeEq::new().flip();
    assert_eq!(TE.to_left("3".to_string()), "3");
    assert_eq!(TE.to_right("5".to_string()), "5");
}

#[test]
fn to_left_to_right_unchecked() {
    const TE: TypeEq<Vec<u8>, Vec<u8>> = unsafe { TypeEq::new_unchecked() };
    assert_eq!(TE.to_left(vec![3]), [3]);
    assert_eq!(TE.to_right(vec![5, 8]), [5, 8]);
}

#[test]
fn assert_type_eq_constructor_types() {
    assert_type::<_, TypeEq<u8, u8>>(TypeEq::<u8, _>::NEW);
    assert_type::<_, TypeEq<i8, i8>>(TypeEq::<_, i8>::NEW);
    
    assert_type::<_, TypeEq<i16, i16>>(TypeEq::new::<i16>());
    assert_type::<_, TypeEq<(), ()>>(TypeEq::new::<()>());

    unsafe {
        assert_type::<_, TypeEq<u8, u16>>(TypeEq::<u8, u16>::new_unchecked());
        assert_type::<_, TypeEq<(), bool>>(TypeEq::<(), bool>::new_unchecked());
    }
}

#[test]
fn map_test() {
    assert_type_eq(TypeEq::new::<u8>().map(GRef::NEW), TypeEq::new::<&u8>());
    assert_type_eq(TypeEq::new::<u8>().map(GRefMut::NEW), TypeEq::new::<&mut u8>());
    const fn te_map<'a, T>(te: TypeEq<T, u8>, x: &'a T) -> &'a u8 {
        te.map(GRef::NEW).to_right(x)
    }
    assert_eq!(te_map(TypeEq::new::<u8>(), &8), &8u8);
}

#[test]
fn project_test() {
    assert_type_eq(TypeEq::new::<u8>().project::<GRef<'_>>(), TypeEq::new::<&u8>());
    assert_type_eq(TypeEq::new::<u8>().project::<GRefMut<'_>>(), TypeEq::new::<&mut u8>());
    const fn te_map<'a, T>(te: TypeEq<T, u8>, x: &'a mut T) -> &'a mut u8 {
        te.project::<GRefMut<'_>>().to_right(x)
    }
    assert_eq!(te_map(TypeEq::new::<u8>(), &mut 8), &mut 8u8);
}

#[test]
fn in_ref_test() {
    assert_type_eq(TypeEq::new::<u8>().in_ref(), TypeEq::new::<&u8>());
    assert_eq!(TypeEq::new::<u8>().in_ref().to_right(&5u8), &5u8);
    const fn in_ref<T>(te: TypeEq<T, u8>, x: &T) -> &u8 {
        te.in_ref().to_right(x)
    }
    assert_eq!(in_ref(TypeEq::new::<u8>(), &8), &8u8);
}

#[test]
fn in_mut_test() {
    assert_type_eq(TypeEq::new::<u8>().in_mut(), TypeEq::new::<&mut u8>());
    assert_eq!(TypeEq::new::<u8>().in_mut().to_right(&mut 5u8), &mut 5u8);
    #[cfg(feature = "mut_refs")]
    {
        const fn in_mut<T>(te: TypeEq<T, u8>, x: &mut T) -> &mut u8 {
            te.in_mut().to_right(x)
        }
        assert_eq!(in_mut(TypeEq::new::<u8>(), &mut 8), &mut 8u8);
    }
}

#[cfg(feature = "alloc")]
#[test]
fn in_box_test() {    
    assert_type_eq(TypeEq::new::<u8>().in_box(), TypeEq::new::<Box<u8>>());
    assert_eq!(TypeEq::new::<u8>().in_box().to_right(Box::new(5u8)), Box::new(5u8));
}

