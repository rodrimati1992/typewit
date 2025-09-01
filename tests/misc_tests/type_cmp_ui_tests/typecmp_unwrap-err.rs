use typewit::{TypeCmp, TypeEq, TypeNe};

const NE: TypeNe<u8, i8> = typewit::type_ne!(u8, i8);
const EQ: TypeEq<u8, u8> = TypeEq::NEW;

const _: TypeNe<u8, u8> = TypeCmp::Eq(EQ).unwrap_ne();

const _: TypeNe<u8, u8> = TypeCmp::Eq(EQ).expect_ne("testing testing 123");

const _: TypeEq<u8, i8> = TypeCmp::Ne(NE).unwrap_eq();

const _: TypeEq<u8, i8> = TypeCmp::Ne(NE).expect_eq("testing testing 456");



fn main() {}