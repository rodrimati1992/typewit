use typewit::MakeTypeWitness;

use core::fmt::Debug;


typewit::simple_type_witness!{
    enum TypeParams['a, T, U:, V: 'a, W: 'a + 'static, X: Sized, Y: 'a + Sized] {
        U8 = (&'a (), T, U, V, W, X, Y) 
    }
}

fn _foo<'a, T, U, V, W, X, Y>() {
    let _: TypeParams<'a, T, U, V, W, X, Y, _> = MakeTypeWitness::MAKE;
}

fn main(){}