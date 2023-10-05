typewit::simple_type_witness!{
    enum TypeParams {
        #[asdkaspokaopskd]
        U8 = u8,
        /// Some docs
        #[cfg(any())]
        U16 = u16,
        /// Some docs
        #[cfg(all())]
        U16 = u16,
    }
}

fn main(){}