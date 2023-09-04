use crate::{
    all_init_bytes::slice_as_bytes,
    TypeEq,
    TypeNe,
};

super::declare_const_param_type! {
    /// # Example
    /// 
    /// Using this marker type to implement dispatching of fields by name.
    /// 
    /// ```rust
    /// #![feature(adt_const_params)]
    /// 
    /// use typewit::{const_marker::Str, MakeTypeWitness};
    /// 
    /// let value = Stuff {
    ///     foo: 3,
    ///     bar: "hello",
    /// };
    /// 
    /// assert_eq!(value.get::<"foo">(), &3);
    /// assert_eq!(value.get::<"bar">(), &"hello");
    /// 
    /// pub struct Stuff<'a> {
    ///     foo: u32,
    ///     bar: &'a str,
    /// }
    /// 
    /// impl<'a> Stuff<'a> {
    ///     const fn get<const S: &'static str>(&self) -> &<Self as Field<S>>::Type 
    ///     where
    ///         FieldWit<Str<S>>: MakeTypeWitness,
    ///         Self: Field<S>,
    ///     {
    ///         let func = FnFieldTy::<Self>::NEW;
    /// 
    ///         match FieldWit::MAKE {
    ///             FieldWit::Foo(te) => te.map(func).in_ref().to_left(&self.foo),
    ///             FieldWit::Bar(te) => te.map(func).in_ref().to_left(&self.bar),
    ///         }
    ///     }
    /// }
    /// 
    /// typewit::type_fn! {
    ///     struct FnFieldTy<Struct>;
    ///  
    ///     impl<const S: &'static str> Str<S> => <Struct as Field<S>>::Type
    ///     where Struct: Field<S>
    /// }
    /// 
    /// trait Field<const S: &'static str> {
    ///     type Type: ?Sized;
    /// }
    /// impl<'a> Field<"foo"> for Stuff<'a> {
    ///     type Type = u32;
    /// }
    /// impl<'a> Field<"bar"> for Stuff<'a> {
    ///     type Type = &'a str;
    /// }
    /// 
    /// typewit::simple_type_witness! {
    ///     // the #[non_exhaustive] is necessary to be able to add fields to Stuff
    ///     #[non_exhaustive]
    ///     pub enum FieldWit {
    ///         Foo = Str<"foo">,
    ///         Bar = Str<"bar">,
    ///     }
    /// }
    /// 
    /// ```
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "nightly_const_marker")))]
    Str(&'static str)

    fn eq(l, r) { u8_slice_eq(l.as_bytes(), r.as_bytes()) };
}

super::declare_const_param_type! {
    BoolSlice(&'static [bool])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    CharSlice(&'static [char])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    U8Slice(&'static [u8])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    U16Slice(&'static [u16])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    U32Slice(&'static [u32])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    U64Slice(&'static [u64])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    U128Slice(&'static [u128])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    UsizeSlice(&'static [usize])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    I8Slice(&'static [i8])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    I16Slice(&'static [i16])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    I32Slice(&'static [i32])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    I64Slice(&'static [i64])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    I128Slice(&'static [i128])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}
super::declare_const_param_type! {
    IsizeSlice(&'static [isize])
    fn eq(l, r) { u8_slice_eq(slice_as_bytes(l), slice_as_bytes(r)) };
}

const fn u8_slice_eq(left: &[u8], right: &[u8]) -> bool {
    if left.len() != right.len() {
        false
    } else {
        let mut i = 0;
        while i != left.len() {
            if left[i] != right[i] {
                return false;
            }
            i += 1;
        }
        true
    }
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