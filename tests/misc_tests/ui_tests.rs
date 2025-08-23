// use this command for ui tests:
// 
// clear;clear; env TRYBUILD=overwrite cargo test --no-default-features \
// --features="rust_stable alloc __ui_tests"
// (remember to run it with and without the proc_macros feature)





#[cfg(feature = "__ui_tests")]
#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    for dir in [
        "misc_ui_tests",
        "stw_ui_tests",
        "type_eq_ui_tests",
        "type_fn_ui_tests",
        "type_ne_ui_tests",
    ] {
        #[cfg(not(feature = "proc_macros"))]
        t.compile_fail(format!("tests/misc_tests/{}/*-err.rs", dir));

        #[cfg(feature = "proc_macros")]
        t.compile_fail(format!("tests/misc_tests/{}/*-pm_err.rs", dir));

        t.pass(format!("tests/misc_tests/{}/*fine.rs", dir));
    }
}
