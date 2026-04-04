use typewit::const_marker::{Bool, Char, U8};

#[cfg(feature = "adt_const_marker")]
use typewit::const_marker::Str;

macro_rules! deserialize_err {
    ($ty:ty, $from_str:expr) => {
        serde_json::from_str::<$ty>($from_str)
            .map_err(|e| e.to_string())
            .unwrap_err()
    }
}


#[test]
fn test_deserialize_u8() {
    assert_eq!(serde_json::from_str::<U8<0>>("0").ok(), Some(U8::<0>));
    assert_eq!(serde_json::from_str::<U8<34>>("34").ok(), Some(U8::<34>));

    assert_eq!(serde_json::from_str::<U8<34>>("33").ok(), None);
    assert_eq!(serde_json::from_str::<U8<0>>("1").ok(), None);
    
    let err = deserialize_err!(U8<0>, "1");
    assert!(err.contains("expected `0`"), "{err:?}");

    assert_eq!(serde_json::from_str::<U8<0>>("{}").ok(), None);
    assert_eq!(serde_json::from_str::<U8<0>>(r#""foo""#).ok(), None);
}

#[test]
fn test_deserialize_bool() {
    assert_eq!(serde_json::from_str::<Bool<false>>("false").ok(), Some(Bool::<false>));
    assert_eq!(serde_json::from_str::<Bool<true >>("true").ok(), Some(Bool::<true >));

    let err = deserialize_err!(Bool<false>, "true");
    assert!(err.contains("expected `false`"), "{err:?}");

    assert_eq!(serde_json::from_str::<Bool<false>>("\"false\"").ok(), None);
    assert_eq!(serde_json::from_str::<Bool<true>>("\"true\"").ok(), None);
    assert_eq!(serde_json::from_str::<Bool<true>>("{}").ok(), None);
    assert_eq!(serde_json::from_str::<Bool<true>>(r#""foo""#).ok(), None);
}



#[cfg(feature = "adt_const_marker")]
#[test]
fn test_deserialize_str() {
    assert_eq!(serde_json::from_str::<Str<"foo">>(r#""foo""#).ok(), Some(Str::<"foo">));
    assert_eq!(serde_json::from_str::<Str<"34">>(r#""34""#).ok(), Some(Str::<"34">));

    assert_eq!(serde_json::from_str::<Str<"34">>(r#""33""#).ok(), None);
    assert_eq!(serde_json::from_str::<Str<"foo">>(r#""1""#).ok(), None);
    
    let err = deserialize_err!(Str<"foo">, "1");
    assert!(err.contains(r#"expected the value `"foo"`"#), "{err:?}");

    assert_eq!(serde_json::from_str::<Str<"foo">>("foo").ok(), None);
    assert_eq!(serde_json::from_str::<Str<"foo">>("{}").ok(), None);
    assert_eq!(serde_json::from_str::<Str<"foo">>("[]").ok(), None);
}

#[cfg(feature = "adt_const_marker")]
macro_rules! slice_case {
    (ok $const_arg:expr, $from_str:expr) => (
        assert_eq!(
            serde_json::from_str::<S<{$const_arg}>>($from_str).ok(), 
            Some(S::<{$const_arg}>),
        );
        assert_eq!(
            &*serde_json::to_string(&S::<{$const_arg}>).unwrap(), 
            $from_str,
        );
    );
    (err $const_arg:expr, $from_str:expr) => (
        assert_eq!(
            serde_json::from_str::<S<{$const_arg}>>($from_str).ok(), 
            None,
        );
    );
}

#[cfg(feature = "adt_const_marker")]
#[test]
fn test_serde_str_slice() {
    use typewit::const_marker::slice::StrSlice as S;

    slice_case!{ok &[], r#"[]"#}
    slice_case!{err &[], r#"["foo"]"#}
    
    slice_case!{err &["foo"], r#"[]"#}
    slice_case!{err &["foo"], r#"["f"]"#}
    slice_case!{ok &["foo"], r#"["foo"]"#}
    slice_case!{err &["foo"], r#"["foo", "bar"]"#}
    
    slice_case!{err &["foo", "bar"], r#"["foo"]"#}
    slice_case!{err &["foo", "bar"], r#"["foo","ba"]"#}
    slice_case!{ok &["foo", "bar"], r#"["foo","bar"]"#}
    slice_case!{err &["foo", "bar"], r#"["foo","bar","baz"]"#}

    slice_case!{err &["foo", "bar"], r#"{}"#}
    slice_case!{err &["foo", "bar"], r#"3"#}
}

#[cfg(feature = "adt_const_marker")]
#[test]
fn test_serde_u8_slice() {
    use typewit::const_marker::slice::U8Slice as S;
    
    slice_case!{err &[3, 5], r#"[3]"#}
    slice_case!{err &[3, 5], r#"[3,1]"#}
    slice_case!{ok &[3, 5], r#"[3,5]"#}
    slice_case!{err &[3, 5], r#"[3,5,8]"#}
    
    slice_case!{err &[3, 5], r#"{}"#}
    slice_case!{err &[3, 5], r#"3"#}

    {
        let err = deserialize_err!(S<{&[3, 5]}>, "[3]");
        assert!(err.contains("too short"), "{err:?}");
    }
    {
        let err = deserialize_err!(S<{&[3, 5]}>, "[3, 5, 8]");
        assert!(err.contains("1 too many elements"), "{err:?}");
    }
    {
        let err = deserialize_err!(S<{&[3, 5]}>, "[3, 5, 8, 13, 21, 34, 55]");
        assert!(err.contains("5 too many elements"), "{err:?}");
    }
}


#[test]
fn test_serialize_u8() {
    assert_eq!(serde_json::to_string(&U8::<0>).unwrap(), "0");
    assert_eq!(serde_json::to_string(&U8::<34>).unwrap(), "34");
}

#[test]
fn test_serialize_char() {
    assert_eq!(serde_json::to_string(&Char::<'#'>).unwrap(), r##""#""##);
    assert_eq!(serde_json::to_string(&Char::<'T'>).unwrap(), r##""T""##);
    assert_eq!(serde_json::to_string(&Char::<'A'>).unwrap(), r##""A""##);
    assert_eq!(serde_json::to_string(&Char::<'G'>).unwrap(), r##""G""##);
}

#[test]
fn test_serialize_bool() {
    assert_eq!(serde_json::to_string(&Bool::<false>).unwrap(), "false");
    assert_eq!(serde_json::to_string(&Bool::<true>).unwrap(), "true");
}

#[cfg(feature = "adt_const_marker")]
#[test]
fn test_serialize_str() {
    assert_eq!(serde_json::to_string(&Str::<"0">).unwrap(), r#""0""#);
    assert_eq!(serde_json::to_string(&Str::<"34">).unwrap(), r#""34""#);
    assert_eq!(serde_json::to_string(&Str::<"foo">).unwrap(), r#""foo""#);
}


struct Buffer([u8; 300]);

impl Buffer {    
    // tests that T roundtrips through a non-self-describing format
    #[track_caller]
    fn roundtrips_nsd<T>(&mut self, val: &T) 
    where
        T: serde_::Serialize + serde_::de::DeserializeOwned + core::cmp::Eq + core::fmt::Debug
    {
        let serialized = postcard::to_slice(val, &mut self.0).unwrap();
        let deserialized = postcard::from_bytes(serialized).unwrap();
        assert_eq!(val, &deserialized);
    }
}


#[test]
fn primitives_nonselfdescribing_test() {
    let mut bf = Buffer([0; 300]);
    
    bf.roundtrips_nsd(&3u8);
    bf.roundtrips_nsd(&5u16);
    bf.roundtrips_nsd(&8u32);
    bf.roundtrips_nsd(&13u64);
    bf.roundtrips_nsd(&64u128);
    
    bf.roundtrips_nsd(&-64i128);
    bf.roundtrips_nsd(&-13i64);
    bf.roundtrips_nsd(&-8i32);
    bf.roundtrips_nsd(&-5i16);
    bf.roundtrips_nsd(&-3i8);
    bf.roundtrips_nsd(&0i8);    
    bf.roundtrips_nsd(&3i8);
    bf.roundtrips_nsd(&5i16);
    bf.roundtrips_nsd(&8i32);
    bf.roundtrips_nsd(&13i64);
    bf.roundtrips_nsd(&64i128);
    
    bf.roundtrips_nsd(&false);
    bf.roundtrips_nsd(&true);
    
    bf.roundtrips_nsd(&'A');
    bf.roundtrips_nsd(&'#');
    bf.roundtrips_nsd(&'é');
}


#[test]
#[cfg(feature = "adt_const_marker")]
fn str_and_slice_nonselfdescribing_test() {
    use typewit::const_marker::slice::{StrSlice, U8Slice};

    let mut bf = Buffer([0; 300]);

    bf.roundtrips_nsd(&Str::<"">);
    bf.roundtrips_nsd(&Str::<"foo">);
    
    bf.roundtrips_nsd(&StrSlice::<{&[]}>);
    bf.roundtrips_nsd(&StrSlice::<{&["foo", "bar"]}>);
    
    bf.roundtrips_nsd(&U8Slice::<{&[]}>);
    bf.roundtrips_nsd(&U8Slice::<{&[3, 5, 8]}>);


}
