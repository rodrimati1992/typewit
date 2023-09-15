// $type_cmp is either TypeEq or TypeNe
macro_rules! declare_type_cmp_helpers {($_:tt $type_cmp_ty:ident $tyfn:ident $callfn:ident) => {
    macro_rules! projected_type_cmp {
        ($type_cmp:expr, $L:ty, $R:ty, $F:ty) => ({
            // safety: 
            // This macro takes a `$type_cmp_ty<$L, $R>` value,
            // which implies `$type_cmp_ty<$callfn<F, $L>, $callfn<F, $R>>`
            unsafe {
                __ProjectVars::<$F, $L, $R> {
                    te: $type_cmp,
                    projected_te: $type_cmp_ty::new_unchecked(),
                }.projected_te
            }
        })
    }

    struct __ProjectVars<F, L: ?Sized, R: ?Sized> 
    where
        InvokeAlias<F>: $tyfn<L> + $tyfn<R>
    {
        #[allow(dead_code)]
        te: $type_cmp_ty<L, R>,

        //         $type_cmp_ty<L, R> 
        // implies $type_cmp_ty<$callfn<F, L>, $callfn<F, R>>
        projected_te: $type_cmp_ty<$callfn<InvokeAlias<F>, L>, $callfn<InvokeAlias<F>, R>>,
    }

    #[cfg(feature = "inj_type_fn")]
    macro_rules! unprojected_type_cmp {
        ($type_cmp:expr, $L:ty, $R:ty, $F:ty) => ({
            // safety: 
            // This macro takes a `$type_cmp_ty<$L, $R>` value,
            // which implies `$type_cmp_ty<UncallFn<F, $L>, UncallFn<F, $R>>`
            //  
            // The properties section of RevTypeFn guarantees this for 
            // both TypeEq and TypeNe
            unsafe {
                __UnprojectVars::<$F, $L, $R> {
                    te: $type_cmp,
                    unprojected_te: $type_cmp_ty::new_unchecked(),
                }.unprojected_te
            }
        })
    }

    #[cfg(feature = "inj_type_fn")]
    struct __UnprojectVars<F, L: ?Sized, R: ?Sized> 
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        #[allow(dead_code)]
        te: $type_cmp_ty<L, R>,

        //         $type_cmp_ty<L, R> 
        // implies $type_cmp_ty<UncallFn<F, L>, UncallFn<F, R>>
        //  
        // The properties section of RevTypeFn guarantees this for 
        // both TypeEq and TypeNe
        unprojected_te: $type_cmp_ty<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>,
    }


    macro_rules! zip_impl {
        // Using `:ident` to prevent usage of macros,
        // which can expand to different values on each use
        ($_( $type_cmp:ident [$L:ident, $R:ident] ),* $_(,)*) => {
            $_(
                let _te: $type_cmp_ty<$L, $R> = $type_cmp;
            )*

            // SAFETY: 
            // `$type_cmp_ty<$L, $R>` for every passed `$type_cmp`
            // implies `$type_cmp_ty<(L0, L1, ...), (R0, R1, ...)>`
            unsafe {
                $type_cmp_ty::<($_($L,)*), ($_($R,)*)>::new_unchecked()
            }
        }
    }


    // Equivalent to `type_cmp.zip(other_type_eq).project::<Func>()`,
    // defined to ensure that methods which do zip+project have 0 overhead in debug builds.
    macro_rules! zip_project {
        // Since `$L0`, `$L1`,`$R0`, and `$R1` are all used only once,
        // it's safe to declare them as `:ty` (safe against malicious type macros).
        (
            $left_type_eq:expr,
            $right_type_eq:expr,
            $F: ty,
            ($L0:ty, $R0:ty),
            ($L1:ty, $R1:ty),
        ) => ({
            __ZipProjectVars::<$F, $L0, $R0, $L1, $R1> {
                left_te: $left_type_eq,
                right_te: $right_type_eq,
                projected_te: {
                    // SAFETY: 
                    // `$type_cmp_ty<$L0, $R0>` and `$type_cmp_ty<$L1, $R1>` 
                    // implies `$type_cmp_ty<($L0, $L1), ($R0, $R1)>`,
                    // 
                    // Using `$F` only once, as a type argument,
                    // to protect against type-position macros that expand to 
                    // different types on each use.
                    unsafe {
                        $type_cmp_ty::new_unchecked()
                    }
                }
            }.projected_te
        });
    }

    struct __ZipProjectVars<F, L0, R0, L1, R1> 
    where
        F: $tyfn<(L0, L1)> + $tyfn<(R0, R1)>
    {
        #[allow(dead_code)]
        left_te: $type_cmp_ty<L0, R0>,

        #[allow(dead_code)]
        right_te: $type_cmp_ty<L1, R1>,

        //         ($type_cmp_ty<L0, R0>, $type_cmp_ty<L1, R1>) 
        // implies $type_cmp_ty<(L0, L1), (R0, R1)> 
        // implies $type_cmp_ty<$callfn<F, (L0, L1)>, $callfn<F, (R0, R1)>>
        projected_te: $type_cmp_ty<$callfn<F, (L0, L1)>, $callfn<F, (R0, R1)>>,
    }
}} pub(crate) use declare_type_cmp_helpers;


