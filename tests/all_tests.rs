#![deny(unused_mut)]
#![cfg_attr(feature = "adt_const_marker", feature(adt_const_params))]
#![cfg_attr(feature = "adt_const_marker", allow(incomplete_features))]
#![cfg_attr(feature = "nightly_mut_refs", feature(const_mut_refs))]

mod misc_tests {
    #[cfg(feature = "const_marker")]
    mod const_marker_tests;

    mod ui_tests;
    mod typeeq_tests;
    mod simple_type_witness_macro_tests;
    mod polymatch_tests;
    mod test_utils;
    mod type_fn_macro_tests;
    
    #[cfg(feature = "inj_type_fn")]
    mod inj_type_fn_macro_tests;
    
    mod type_ne_tests;
    
    mod type_identity;
}
