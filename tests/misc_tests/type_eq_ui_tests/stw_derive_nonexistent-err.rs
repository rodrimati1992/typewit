
typewit::simple_type_witness!{
    derive(Serialize)
    enum NoDerives {U8 = u8, U16 = u16}
}