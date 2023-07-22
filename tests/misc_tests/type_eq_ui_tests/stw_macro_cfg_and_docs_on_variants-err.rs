//! some docs
#![deny(missing_docs)]
// attempts to ensure that all attributes are emitted for variants

/// 
pub mod _0 {
    typewit::simple_type_witness! {
        pub enum EnumWithOnlyDocAttrs {
            Bar = u8,
            /// 
            Baz = u16,
            ///
            Qux = u32,
        }        
    }
}

/// 
pub mod _1 {
    typewit::simple_type_witness! {
        pub enum EnumWithOnlyDocAttrs {
            ///
            Bar = u8,
            Baz = u16,
            ///
            Qux = u32,
        }        
    }
}

/// 
pub mod _2 {
    typewit::simple_type_witness! {
        pub enum EnumWithOnlyDocAttrs {
            ///
            Bar = u8,
            ///
            Baz = u16,
            Qux = u32,
        }        
    }
}


/// 
pub mod _3 {
    typewit::simple_type_witness! {
        pub enum EnumWithCfgAndOtherAttrs {
            Bar = u8,
            #[cfg(any())]
            Baz = u16,
            /// 
            Qux = u32,
        }        
    }
}

/// 
pub mod _4 {
    typewit::simple_type_witness! {
        pub enum EnumWithCfgAndOtherAttrs {
            /// 
            Bar = u8,
            #[cfg(any())]
            Baz = u16,
            Qux = u32,
        }        
    }
}

/// 
pub mod _should_build_0 {
    typewit::simple_type_witness! {
        ///
        pub enum ShouldBuild {
            /// 
            #[cfg(all())]
            Bar = u8,
            #[cfg(any())]
            Baz = u16,
            #[cfg(all())]
            ///
            Qux = u32,
        }        
    }
}

/// 
pub mod _should_build_1 {
    typewit::simple_type_witness! {
        ///
        pub enum ShouldBuild {
            /// 
            Bar = u8,
            ///
            Baz = u16,
            ///
            Qux = u32,
        }        
    }
}

fn main(){}

