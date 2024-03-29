use typewit::const_marker::Str;
use typewit::{TypeCmp, TypeEq, TypeNe};

#[allow(unused_braces)]
#[test]
fn test_str_const_marker() {
    fn _ensure_correct_types<const L: &'static str, const R: &'static str>() {
        match Str::<L>.equals(Str::<R>) {
            TypeCmp::Eq(TypeEq::<Str<L>, Str<R>>{..}) => {}
            TypeCmp::Ne(TypeNe::<Str<L>, Str<R>>{..}) => {}
        }
    }

    assert!(Str::<{""}>.equals(Str::<{""}>).is_eq());
    assert!(Str::<{""}>.equals(Str::<{"0"}>).is_ne());
    assert!(Str::<{"0"}>.equals(Str::<{""}>).is_ne());
    assert!(Str::<{"0"}>.equals(Str::<{"0"}>).is_eq());
    assert!(Str::<{"0"}>.equals(Str::<{"1"}>).is_ne());
    assert!(Str::<{"1"}>.equals(Str::<{"0"}>).is_ne());
    assert!(Str::<{"0"}>.equals(Str::<{"0, 1"}>).is_ne());
    assert!(Str::<{"0, 1"}>.equals(Str::<{"0"}>).is_ne());
    assert!(Str::<{"0, 1"}>.equals(Str::<{"1"}>).is_ne());
    assert!(Str::<{"0, 1"}>.equals(Str::<{"0, 1"}>).is_eq());
    assert!(Str::<{"0, 1"}>.equals(Str::<{"0, 2"}>).is_ne());
}


#[test]
fn str_slice_eq_test() {
    use typewit::const_marker::slice::StrSlice as StrS;

    assert!(StrS::<{&[]}>.equals(StrS::<{&[]}>).is_eq());
    assert!(StrS::<{&[""]}>.equals(StrS::<{&[""]}>).is_eq());
    assert!(StrS::<{&[]}>.equals(StrS::<{&[""]}>).is_ne());
    assert!(StrS::<{&[""]}>.equals(StrS::<{&[]}>).is_ne());

    // length 1
    assert!(StrS::<{&["foo"]}>.equals(StrS::<{&["foo"]}>).is_eq());
    assert!(StrS::<{&["foo"]}>.equals(StrS::<{&["bar"]}>).is_ne());

    // length 2
    assert!(StrS::<{&["foo", "bar"]}>.equals(StrS::<{&["foo", "bar"]}>).is_eq());
    assert!(StrS::<{&["foo", "bar"]}>.equals(StrS::<{&["foo", "baz"]}>).is_ne());
    assert!(StrS::<{&["foo", "bar"]}>.equals(StrS::<{&["foo", "bbr"]}>).is_ne());
    assert!(StrS::<{&["foo", "bar"]}>.equals(StrS::<{&["foo", "car"]}>).is_ne());

    // length 3
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.equals(StrS::<{&["foo", "foo2", "bar"]}>).is_eq());
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.equals(StrS::<{&["foo", "foo2", "baz"]}>).is_ne());
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.equals(StrS::<{&["foo", "foo2", "bbr"]}>).is_ne());
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.equals(StrS::<{&["foo", "foo2", "car"]}>).is_ne());
}

macro_rules! test_val_perms {
    ($T:ident, $a:expr, $b:expr, $c:expr, $d:expr) => {
        assert!($T::<{&[]}>.equals($T::<{&[]}>).is_eq(), "0:");

        assert!($T::<{&[]}>.equals($T::<{&[$a]}>).is_ne(), "1:");
        assert!($T::<{&[$a]}>.equals($T::<{&[]}>).is_ne(), "2:");
        assert!($T::<{&[$b]}>.equals($T::<{&[$b ]}>).is_eq(), "3:");
        assert!($T::<{&[$a]}>.equals($T::<{&[$a]}>).is_eq(), "4:");

        assert!($T::<{&[$d, $d]}>.equals($T::<{&[$d]}>).is_ne(), "5:");
        assert!($T::<{&[$d, $d]}>.equals($T::<{&[$c]}>).is_ne(), "6:");
        assert!($T::<{&[$d, $d]}>.equals($T::<{&[$d, $c]}>).is_ne(), "7:");
        assert!($T::<{&[$c, $c]}>.equals($T::<{&[$c, $d]}>).is_ne(), "8:");
        assert!($T::<{&[$d, $d]}>.equals($T::<{&[$d, $d]}>).is_eq(), "9:");
        assert!($T::<{&[$c, $c]}>.equals($T::<{&[$c, $c]}>).is_eq(), "10:");

    };
}


#[test]
fn bool_slice_test() {
    use typewit::const_marker::slice::BoolSlice;

    test_val_perms!{BoolSlice, false, true, false, true}
}

#[test]
fn char_slice_test() {
    use typewit::const_marker::slice::CharSlice;

    test_val_perms!{CharSlice, '0', '个', '\u{100000}', char::MAX}
}

#[test]
fn u8_slice_test() {
    use typewit::const_marker::slice::U8Slice;

    test_val_perms!{U8Slice, 0, 0x1, 0x22, 0x33}
}

#[test]
fn u16_slice_test() {
    use typewit::const_marker::slice::U16Slice;

    test_val_perms!{U16Slice, 0, 0x55, 0x888, 0x1321}
}

#[test]
fn u32_slice_test() {
    use typewit::const_marker::slice::U32Slice;

    test_val_perms!{U32Slice, 0, 0x34, 0x5589, 0x2358_1321}
}

#[test]
fn u64_slice_test() {
    use typewit::const_marker::slice::U64Slice;

    test_val_perms!{U64Slice, 0, 0x34, 0x5589, 0x2358_1321_3455_89_D4}
}

#[test]
fn u128_slice_test() {
    use typewit::const_marker::slice::U128Slice;

    test_val_perms!{
        U128Slice, 
        0, 
        0x34, 
        0x5589, 
        0x2358_1321_3455_89_D4
    }
}

#[test]
fn usize_slice_test() {
    use typewit::const_marker::slice::UsizeSlice;

    test_val_perms!{UsizeSlice, 0, 0x55, 0x888, 0x1321}
}

#[test]
fn i8_slice_test() {
    use typewit::const_marker::slice::I8Slice;

    test_val_perms!{I8Slice, 0, 2, -1, i8::MAX}
}

#[test]
fn i16_slice_test() {
    use typewit::const_marker::slice::I16Slice;

    test_val_perms!{I16Slice, 0, 3, -2, i16::MIN}
}

#[test]
fn i32_slice_test() {
    use typewit::const_marker::slice::I32Slice;

    test_val_perms!{I32Slice, 0, 5, -3, i32::MAX}
}

#[test]
fn i64_slice_test() {
    use typewit::const_marker::slice::I64Slice;

    test_val_perms!{I64Slice, 0, 8, -5, i64::MIN}
}

#[test]
fn i128_slice_test() {
    use typewit::const_marker::slice::I128Slice;

    test_val_perms!{I128Slice, 0, 13, -8, i128::MAX}
}

#[test]
fn isize_slice_test() {
    use typewit::const_marker::slice::IsizeSlice;

    test_val_perms!{IsizeSlice, 0, 21, -13, isize::MIN}
}