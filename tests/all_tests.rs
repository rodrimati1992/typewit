#![deny(unused_mut)]
#![cfg_attr(feature = "adt_const_marker", feature(adt_const_params))]
#![cfg_attr(feature = "adt_const_marker", feature(unsized_const_params))]
#![cfg_attr(feature = "adt_const_marker", allow(incomplete_features))]

mod misc_tests {
    mod test_utils;

    mod const_marker_tests;

    #[cfg(feature = "rust_1_65")]
    mod generic_fns_tests;

    mod ui_tests;
    mod type_cmp_tests;
    mod typeeq_tests;
    mod simple_type_witness_macro_tests;
    mod polymatch_tests;
    mod type_fn_macro_tests;
    
    mod inj_type_fn_macro_tests;
    
    mod type_ne_tests;
    
    mod type_identity;
}
