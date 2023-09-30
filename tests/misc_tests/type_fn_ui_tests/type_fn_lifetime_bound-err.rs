typewit::type_fn!{
    struct WithLts<'a, 'b: 'a, 'c: 'a + 'static, 'd: 'static + 'a>;

    impl () => ()
}

fn _with_lifetime<'a, 'b, 'c, 'd>() {
    let _: WithLts<'a, 'b, 'c, 'd>;
}

fn main() {}