macro_rules! __declare_const_marker_additional_impls {
    ($struct:ident($prim:ty)) => {
        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "const_marker_extra_impls")))]
        impl<const L: $prim, const R: $prim> core::cmp::PartialOrd<$struct<R>> for $struct<L> {
            fn partial_cmp(&self, _: &$struct<R>) -> Option<core::cmp::Ordering> {
                Some(core::cmp::Ord::cmp(&L, &R))
            }
        }

        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "const_marker_extra_impls")))]
        impl<const VAL: $prim> core::cmp::Ord for $struct<VAL> {
            fn cmp(&self, _: &Self) -> core::cmp::Ordering {
                core::cmp::Ordering::Equal
            }
        }

        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "const_marker_extra_impls")))]
        impl<const VAL: $prim> core::hash::Hash for $struct<VAL> {
            fn hash<H>(&self, state: &mut H)
            where
                H: core::hash::Hasher,
            {
                core::hash::Hash::hash(&VAL, state)
            }
        }
    }
} pub(crate) use __declare_const_marker_additional_impls;


