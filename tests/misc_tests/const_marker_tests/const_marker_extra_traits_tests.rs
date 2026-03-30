use typewit::const_marker::U8;

use core::cmp::{Ord, PartialOrd, Ordering};


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

#[test]
fn test_hash() {
    use core::hash::BuildHasher;

    let bh = std::hash::RandomState::new();

    assert_eq!(bh.hash_one(U8::<0>), bh.hash_one(0u8));
    assert_eq!(bh.hash_one(U8::<1>), bh.hash_one(1u8));
    assert_eq!(bh.hash_one(U8::<42>), bh.hash_one(42u8));
    assert_eq!(bh.hash_one(U8::<128>), bh.hash_one(128u8));
    assert_eq!(bh.hash_one(U8::<255>), bh.hash_one(255u8));
}








