use crate::{
    TypeEq,
    TypeNe,
};

super::declare_const_param_type! {
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "nightly_const_marker")))]
    Str(&'static str)

    fn eq(l, r) { u8_slice_eq(l.as_bytes(), r.as_bytes()) };
}


macro_rules! cmp_slice_of {
    ($left:ident, $right:ident) => {
        if $left.len() != $right.len() {
            false
        } else {
            let mut i = 0;
            loop {
                if i == $left.len() {
                    break true;
                } else if $left[i] != $right[i] {
                    break false;
                };
                i += 1;
            }
        }
    };
} use cmp_slice_of;

const fn u8_slice_eq(left: &[u8], right: &[u8]) -> bool {
    cmp_slice_of!(left, right)
}



#[test]
fn u8_slice_test() {
    assert!(u8_slice_eq(b"", b""));
    assert!(!u8_slice_eq(b"", b"0"));
    assert!(!u8_slice_eq(b"0", b""));
    assert!(u8_slice_eq(b"0", b"0"));
    assert!(!u8_slice_eq(b"0", b"1"));
    assert!(!u8_slice_eq(b"1", b"0"));
    assert!(!u8_slice_eq(b"0", b"0, 1"));
    assert!(!u8_slice_eq(b"0, 1", b"0"));
    assert!(!u8_slice_eq(b"0, 1", b"1"));
    assert!(u8_slice_eq(b"0, 1", b"0, 1"));
    assert!(!u8_slice_eq(b"0, 1", b"0, 2"));
}