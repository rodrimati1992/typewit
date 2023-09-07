use typewit::const_marker::Str;
use typewit::{TypeEq, TypeNe};

#[allow(unused_braces)]
#[test]
fn test_str_const_marker() {
    fn _ensure_correct_types<const L: &'static str, const R: &'static str>() {
        match Str::<L>.eq(Str::<R>) {
            Ok(TypeEq::<Str<L>, Str<R>>{..}) => {}
            Err(TypeNe::<Str<L>, Str<R>>{..}) => {}
        }
    }

    assert!(Str::<{""}>.eq(Str::<{""}>).is_ok());
    assert!(Str::<{""}>.eq(Str::<{"0"}>).is_err());
    assert!(Str::<{"0"}>.eq(Str::<{""}>).is_err());
    assert!(Str::<{"0"}>.eq(Str::<{"0"}>).is_ok());
    assert!(Str::<{"0"}>.eq(Str::<{"1"}>).is_err());
    assert!(Str::<{"1"}>.eq(Str::<{"0"}>).is_err());
    assert!(Str::<{"0"}>.eq(Str::<{"0, 1"}>).is_err());
    assert!(Str::<{"0, 1"}>.eq(Str::<{"0"}>).is_err());
    assert!(Str::<{"0, 1"}>.eq(Str::<{"1"}>).is_err());
    assert!(Str::<{"0, 1"}>.eq(Str::<{"0, 1"}>).is_ok());
    assert!(Str::<{"0, 1"}>.eq(Str::<{"0, 2"}>).is_err());
}


#[test]
fn str_slice_eq_test() {
    use typewit::const_marker::slice::StrSlice as StrS;

    assert!(StrS::<{&[]}>.eq(StrS::<{&[]}>).is_ok());
    assert!(StrS::<{&[""]}>.eq(StrS::<{&[""]}>).is_ok());
    assert!(StrS::<{&[]}>.eq(StrS::<{&[""]}>).is_err());
    assert!(StrS::<{&[""]}>.eq(StrS::<{&[]}>).is_err());

    // length 1
    assert!(StrS::<{&["foo"]}>.eq(StrS::<{&["foo"]}>).is_ok());
    assert!(StrS::<{&["foo"]}>.eq(StrS::<{&["bar"]}>).is_err());

    // length 2
    assert!(StrS::<{&["foo", "bar"]}>.eq(StrS::<{&["foo", "bar"]}>).is_ok());
    assert!(StrS::<{&["foo", "bar"]}>.eq(StrS::<{&["foo", "baz"]}>).is_err());
    assert!(StrS::<{&["foo", "bar"]}>.eq(StrS::<{&["foo", "bbr"]}>).is_err());
    assert!(StrS::<{&["foo", "bar"]}>.eq(StrS::<{&["foo", "car"]}>).is_err());

    // length 3
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.eq(StrS::<{&["foo", "foo2", "bar"]}>).is_ok());
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.eq(StrS::<{&["foo", "foo2", "baz"]}>).is_err());
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.eq(StrS::<{&["foo", "foo2", "bbr"]}>).is_err());
    assert!(StrS::<{&["foo", "foo2", "bar"]}>.eq(StrS::<{&["foo", "foo2", "car"]}>).is_err());
}

macro_rules! test_val_perms {
    ($T:ident, $a:expr, $b:expr, $c:expr, $d:expr) => {
        assert!($T::<{&[]}>.eq($T::<{&[]}>).is_ok(), "0:");

        assert!($T::<{&[]}>.eq($T::<{&[$a]}>).is_err(), "1:");
        assert!($T::<{&[$a]}>.eq($T::<{&[]}>).is_err(), "2:");
        assert!($T::<{&[$b]}>.eq($T::<{&[$b ]}>).is_ok(), "3:");
        assert!($T::<{&[$a]}>.eq($T::<{&[$a]}>).is_ok(), "4:");

        assert!($T::<{&[$d, $d]}>.eq($T::<{&[$d]}>).is_err(), "5:");
        assert!($T::<{&[$d, $d]}>.eq($T::<{&[$c]}>).is_err(), "6:");
        assert!($T::<{&[$d, $d]}>.eq($T::<{&[$d, $c]}>).is_err(), "7:");
        assert!($T::<{&[$c, $c]}>.eq($T::<{&[$c, $d]}>).is_err(), "8:");
        assert!($T::<{&[$d, $d]}>.eq($T::<{&[$d, $d]}>).is_ok(), "9:");
        assert!($T::<{&[$c, $c]}>.eq($T::<{&[$c, $c]}>).is_ok(), "10:");

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