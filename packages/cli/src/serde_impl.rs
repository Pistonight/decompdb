use std::sync::Arc;

use cu::pre::*;

/// Deserializable regex
#[derive(Debug, Clone, Deref, DerefMut)]
#[repr(transparent)]
pub struct Regex(regex::Regex);

impl<'de> Deserialize<'de> for Regex {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        return d.deserialize_str(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Regex;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a regular expression")
            }
            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                match regex::Regex::new(v) {
                    Err(e) => Err(serde::de::Error::custom(format!(
                        "invalid regular expression '{v}': {e}"
                    ))),
                    Ok(x) => Ok(Regex(x)),
                }
            }
        }
    }
}

/// Serializable and deserializable shared string
#[derive(Clone, PartialEq, Eq, Hash, Deref, DerefMut, Display, DebugCustom)]
#[display("{}", self.0)]
#[debug("{:?}", self.0)]
#[repr(transparent)]
pub struct ArcStr(Arc<str>);
impl ArcStr {
    pub fn new(value: &str) -> Self {
        Self(value.into())
    }
}
impl From<&str> for ArcStr {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}
impl AsRef<str> for ArcStr {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Serialize for ArcStr {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(ser)
    }
}

impl<'de> Deserialize<'de> for ArcStr {
    fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        return de.deserialize_str(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = ArcStr;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "string")
            }
            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                Ok(ArcStr::new(v))
            }
        }
    }
}
