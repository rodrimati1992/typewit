
typewit::simple_type_witness!{
    enum TwoU8s {First = u8, Second = u8 }
}

typewit::simple_type_witness!{
    enum OrConcrete<T> {Vectu8 = Vec<u8>, Generic = T }
}



fn main() {}