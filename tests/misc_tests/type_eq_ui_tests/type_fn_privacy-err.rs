mod private {
    typewit::type_fn!{
        struct Unit;

        impl u8 => u16;
    }

    typewit::type_fn!{
        struct WithParams<'a, T, const N: usize>;

        impl u8 => T;
    }
}

fn main(){
    let _: private::Unit;
    let _: private::WithParams::<'static, u8, 0>;
}