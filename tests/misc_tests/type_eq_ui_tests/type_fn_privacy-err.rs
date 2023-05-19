mod private {
    typewit::type_fn!{
        struct Unit;

        u8 => u16;
    }

    typewit::type_fn!{
        struct WithParams['a, T, const N: usize];

        u8 => T;
    }
}

fn main(){
    let _: private::Unit;
    let _: private::WithParams::<'static, u8, 0>;
}