#![deny(unused_mut)]
#![cfg_attr(feature = "nightly_mut_refs", feature(const_mut_refs))]

mod misc_tests {
    #[cfg(feature = "const_marker")]
    mod const_marker_tests;

    mod ui_tests;
    mod typeeq_tests;
    mod simple_type_witness_macro_tests;
    mod test_utils;
    mod type_fn_macro_tests;
}
