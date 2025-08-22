
typewit::simple_type_witness! {
    derive(Equals)
    enum WithLt<'a> { A = &'a () }
}

typewit::simple_type_witness! {
    derive(Equals)
    enum WithTypes<T> { A = T }
}

typewit::simple_type_witness! {
    derive(Equals)
    enum WithConst<const X: usize> { A = u8 }
}

typewit::simple_type_witness! {
    derive(Equals)
    enum WithAllGenerics<'a, T, const X: usize> { A = &'a [T; X] }
}

typewit::simple_type_witness! {
    derive(Equals)
    enum WithWhereClause<'a, T, const X: usize> 
    where
        T: Copy
    {
        A = &'a [T; X] 
    }
}

typewit::simple_type_witness! {
    derive(Equals)
    enum WithDefaultedArg<'a, T, const X: usize> {
        A<'a, T, 0> = &'a [T] 
    }
}


typewit::simple_type_witness! {
    derive(Equals)
    enum WithEverything<'a, T, const X: usize> 
    where
        T: Copy
    {
        A<'a, T, 0> = &'a [T],
        B = [u8; X],
    }
}


fn main() {}

