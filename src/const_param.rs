//! Marker types for passing constants as type arguments.

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

declare_const_param_type!{
    Usize(usize)

    /// # Examples
    /// 
    /// ### Array
    /// 
    /// This example demonstrates how `Usize` can be used to 
    /// fallibly return a 2 element long array
    /// in a function that returns an array of any length.
    /// 
    /// ```rust
    /// use typewit::{const_param::Usize, TypeEq};
    /// 
    /// assert_eq!(try_from_pair::<_, 0>((3, 5)), Err((3, 5)));
    /// assert_eq!(try_from_pair::<_, 1>((3, 5)), Err((3, 5)));
    /// assert_eq!(try_from_pair::<_, 2>((3, 5)), Ok([3, 5]));
    /// assert_eq!(try_from_pair::<_, 3>((3, 5)), Err((3, 5)));
    /// 
    /// 
    /// const fn try_from_pair<T: Copy, const LEN: usize>(pair: (T, T)) -> Result<[T; LEN], (T, T)> {
    ///     match Usize::<LEN>.eq(Usize::<2>) {
    ///         // `te_len` is a `TypeEq<Usize<LEN>, Usize<2>>`
    ///         Ok(te_len) => Ok(
    ///             TypeEq::new::<T>() // `TypeEq<T, T>`
    ///                 .in_array(te_len) // `TypeEq<[T; LEN], [T; 2]>`
    ///                 .to_left([pair.0, pair.1]) // Goes from `[T; 2]` to `[T; LEN]`
    ///         ),
    ///         Err(_) => Err(pair),
    ///     }
    /// }
    /// 
    /// ```
    /// 
    /// ### Struct
    /// 
    /// This example demonstrates how `Usize` can be used to pass a 
    /// const-generic struct to a function expecting a concrete type of that struct.
    /// 
    /// ```rust
    /// use typewit::{const_param::Usize, TypeEq, TypeFn};
    /// 
    /// assert_eq!(mutate(Array([])), Array([]));
    /// assert_eq!(mutate(Array([3])), Array([3]));
    /// assert_eq!(mutate(Array([3, 5])), Array([3, 5]));
    /// assert_eq!(mutate(Array([3, 5, 8])), Array([8, 5, 3])); // reversed!
    /// assert_eq!(mutate(Array([3, 5, 8, 13])), Array([3, 5, 8, 13]));
    /// 
    /// 
    /// #[derive(Debug, PartialEq)]
    /// struct Array<const CAP: usize>([u32; CAP]);
    /// 
    /// const fn mutate<const LEN: usize>(arr: Array<LEN>) -> Array<LEN> {
    ///     match Usize::<LEN>.eq(Usize::<3>) {
    ///         // `te_len` is a `TypeEq<Usize<LEN>, Usize<3>>`
    ///         Ok(te_len) => {
    ///             // `te` is a `TypeEq<Array<LEN>, Array<3>>`
    ///             let te = te_len.project::<GArray>();
    /// 
    ///             // `te.to_right(...)` here goes from `Array<LEN>` to `Array<3>`
    ///             let ret = reverse3(te.to_right(arr));
    /// 
    ///             // `te.to_left(...)` here goes from `Array<3>` to `Array<LEN>`
    ///             te.to_left(ret)
    ///         }
    ///         Err(_) => arr,
    ///     }
    /// }
    /// 
    /// const fn reverse3(Array([a, b, c]): Array<3>) -> Array<3> {
    ///     Array([c, b, a])
    /// }
    /// 
    /// // Type-level function to project `Usize<LEN>` to `Array<LEN>`
    /// struct GArray;
    /// 
    /// impl<const LEN: usize> TypeFn<Usize<LEN>> for GArray {
    ///     type Output = Array<LEN>;
    /// }
    /// ```
    fn eq;
}

declare_const_param_type!{I8(i8)}
declare_const_param_type!{I16(i16)}
declare_const_param_type!{I32(i32)}
declare_const_param_type!{I64(i64)}
declare_const_param_type!{I128(i128)}
declare_const_param_type!{Isize(isize)}

