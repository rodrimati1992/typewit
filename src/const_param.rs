//! Marker types for passing const parameters as types.

use crate::{
    TypeEq,
    TypeNe,
};



macro_rules! declare_const_param_type {
    (
        $(#[$struct_docs:meta])*
        $struct:ident($prim:ty)

        $(
            $(#[$eq_docs:meta])*
            fn eq;
        )?
    ) => {
        #[doc = concat!(
            "Marker type for passing `const VAL: ", stringify!($prim),
            "` as a type parameter."
        )]
        $(#[$struct_docs])*
        #[derive(Debug, Copy, Clone)]
        pub struct $struct<const VAL: $prim>;

        impl<const VAL: $prim> $struct<VAL> {
            /// Compares `self` and `other` for equality.
            ///
            /// Returns:
            /// - `Ok(TypeEq)`: if `VAL == OTHER`
            /// - `Err(TypeNe)`: if `VAL != OTHER`
            ///
            $($(#[$eq_docs])*)?
            #[inline(always)]
            pub const fn eq<const OTHER: $prim>(
                self, 
                _other: $struct<OTHER>,
            ) -> Result<
                TypeEq<$struct<VAL>, $struct<OTHER>>,
                TypeNe<$struct<VAL>, $struct<OTHER>>,
            > {
                struct Helper<const L: $prim, const R: $prim>;

                impl<const L: $prim, const R: $prim> Helper<L, R> {
                    const EQ: Result<
                        TypeEq<$struct<L>, $struct<R>>,
                        TypeNe<$struct<L>, $struct<R>>,
                    > = if L == R {
                        // SAFETY: `L == R` (both are std types with sensible Eq impls)
                        // therefore `$struct<L> == $struct<R>`
                        unsafe {
                            Ok(TypeEq::<$struct<L>, $struct<R>>::new_unchecked())
                        }
                    } else {
                        // SAFETY: `L != R` (both are std types with sensible Eq impls)
                        // therefore `$struct<L> != $struct<R>`
                        unsafe {
                            Err(TypeNe::<$struct<L>, $struct<R>>::new_unchecked())
                        }
                    };
                }

                Helper::<VAL, OTHER>::EQ
            }
        }

    };
}


declare_const_param_type!{Bool(bool)}
declare_const_param_type!{Char(char)}

declare_const_param_type!{U8(u8)}
declare_const_param_type!{U16(u16)}
declare_const_param_type!{U32(u32)}
declare_const_param_type!{U64(u64)}
declare_const_param_type!{U128(u128)}
declare_const_param_type!{Usize(usize)}

declare_const_param_type!{I8(i8)}
declare_const_param_type!{I16(i16)}
declare_const_param_type!{I32(i32)}
declare_const_param_type!{I64(i64)}
declare_const_param_type!{I128(i128)}
declare_const_param_type!{Isize(isize)}

