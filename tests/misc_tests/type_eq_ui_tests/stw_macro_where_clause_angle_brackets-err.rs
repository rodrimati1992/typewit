use typewit::MakeTypeWitness;

use core::fmt::Debug;

typewit::simple_type_witness!{
    enum WhereClauseA<T>
    where 
    {
        U8 where T: Copy = T
    }
}

typewit::simple_type_witness!{
    enum WhereClauseB<T>
    where T: Debug
    {
        U8 where T: Copy, = T
    }
}

typewit::simple_type_witness!{
    enum WhereClauseC<T>
    where T: Debug,
    {
        U8 where T: Copy = T
    }
}

fn _a<T>() {
    let _: WhereClauseA<T, T> = MakeTypeWitness::MAKE;
    let _: WhereClauseB<T, T> = MakeTypeWitness::MAKE;
    let _: WhereClauseC<T, T> = MakeTypeWitness::MAKE;
}

fn main(){}