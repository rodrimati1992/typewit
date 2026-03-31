use typewit::const_marker::{Char, U8};

#[test]
fn test_deserialize_u8() {
    assert_eq!(serde_json::from_str::<U8<0>>("0").ok(), Some(U8::<0>));
    assert_eq!(serde_json::from_str::<U8<34>>("34").ok(), Some(U8::<34>));

    assert_eq!(serde_json::from_str::<U8<34>>("33").ok(), None);
    assert_eq!(serde_json::from_str::<U8<0>>("1").ok(), None);
    
    let err = serde_json::from_str::<U8<0>>("1")
        .map_err(|e| format!("{e:?}"))
        .unwrap_err();
    assert!(err.contains("expected `0` found `1`"), "{err:?}");

    assert_eq!(serde_json::from_str::<U8<0>>("{}").ok(), None);
    assert_eq!(serde_json::from_str::<U8<0>>(r#""foo""#).ok(), None);
}

#[test]
fn test_serialize_u8() {
    assert_eq!(serde_json::to_string(&U8::<0>).unwrap(), "0");
    assert_eq!(serde_json::to_string(&U8::<34>).unwrap(), "34");
}

