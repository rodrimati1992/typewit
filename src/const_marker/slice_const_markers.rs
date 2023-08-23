use crate::{
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