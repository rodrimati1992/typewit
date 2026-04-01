use typewit::const_marker::U8;

use core::cmp::{Ord, PartialOrd, Ordering};

use core::hash::{BuildHasher, Hasher};

use std::collections::hash_map::RandomState;


#[test]
fn test_partial_cmp() {
    assert_eq!(U8::<0>.partial_cmp(&U8::<0>), Some(Ordering::Equal));
    assert_eq!(U8::<0>.partial_cmp(&U8::<1>), Some(Ordering::Less));
    assert_eq!(U8::<1>.partial_cmp(&U8::<0>), Some(Ordering::Greater));
}

#[test]
fn test_cmp() {
    assert_eq!(U8::<0>.cmp(&U8::<0>), Ordering::Equal);
    assert_eq!(U8::<1>.cmp(&U8::<1>), Ordering::Equal);
}


fn hash_one<T: std::hash::Hash>(this: &mut RandomState, x: T) -> u64 {
    let mut hasher = this.build_hasher();
    x.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn test_hash() {
    use core::hash::BuildHasher;

    let bh = &mut RandomState::new();

    assert_eq!(hash_one(bh, U8::<0>), hash_one(bh, 0u8));
    assert_eq!(hash_one(bh, U8::<1>), hash_one(bh, 1u8));
    assert_eq!(hash_one(bh, U8::<42>), hash_one(bh, 42u8));
    assert_eq!(hash_one(bh, U8::<128>), hash_one(bh, 128u8));
    assert_eq!(hash_one(bh, U8::<255>), hash_one(bh, 255u8));
}








