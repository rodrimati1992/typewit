use core::fmt::{self, Debug, Display};

macro_rules! __declare_const_marker_serde_impls {
    ($struct:ident($prim:ty)) => {
        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "serde")))]
        impl<'de, const VAL: $prim> serde_::Deserialize<'de> for $struct<VAL> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde_::Deserializer<'de>,
            {
                let found = <$prim as serde_::Deserialize<'de>>::deserialize(deserializer)?;
                if found == VAL {
                    Ok(Self)
                } else {
                    Err(serde_::de::Error::custom(DeserErrorMsg {
                        expected: VAL,
                        found,
                    }))
                }
            }
        }

        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "serde")))]
        impl<const VAL: $prim> serde_::Serialize for $struct<VAL> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde_::Serializer,
            {
                serde_::Serialize::serialize(&VAL, serializer)
            }
        }
    }
} 

pub(crate) use __declare_const_marker_serde_impls;

pub(crate) struct DeserErrorMsg<T> {
    pub(crate) expected: T,
    pub(crate) found: T,
}

impl<T: Debug> Display for DeserErrorMsg<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "expected `{:?}` found `{:?}`",
            self.expected,
            self.found,
        )
    }
}

