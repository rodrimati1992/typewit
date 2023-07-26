use typewit::const_marker::Str;
use typewit::{TypeEq, TypeNe};

#[test]
fn test_str_const_marker() {
    fn _ensure_correct_types<const L: &'static str, const R: &'static str>() {
        match Str::<L>.eq(Str::<R>) {
            Ok(TypeEq::<Str<L>, Str<R>>{..}) => {}
            Err(TypeNe::<Str<L>, Str<R>>{..}) => {}
        }
    }

    assert!(Str::<"">.eq(Str::<"">).is_ok());
    assert!(Str::<"">.eq(Str::<"0">).is_err());
    assert!(Str::<"0">.eq(Str::<"">).is_err());
    assert!(Str::<"0">.eq(Str::<"0">).is_ok());
    assert!(Str::<"0">.eq(Str::<"1">).is_err());
    assert!(Str::<"1">.eq(Str::<"0">).is_err());
    assert!(Str::<"0">.eq(Str::<"0, 1">).is_err());
    assert!(Str::<"0, 1">.eq(Str::<"0">).is_err());
    assert!(Str::<"0, 1">.eq(Str::<"1">).is_err());
    assert!(Str::<"0, 1">.eq(Str::<"0, 1">).is_ok());
    assert!(Str::<"0, 1">.eq(Str::<"0, 2">).is_err());
}



