#[cfg(feature = "__ui_tests")]
#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    for dir in [
        "type_eq_ui_tests",
    ] {
        t.compile_fail(format!("tests/misc_tests/{}/*err.rs", dir));
        t.pass(format!("tests/misc_tests/{}/*fine.rs", dir));
    }
}
