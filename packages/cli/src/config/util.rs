use cu::pre::*;
use regex::Regex;

/// Deserializable regex
#[derive(Debug, Clone, Deref, DerefMut)]
#[repr(transparent)]
pub struct CfgRegex(Regex);

impl<'de> Deserialize<'de> for CfgRegex {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        return d.deserialize_str(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = CfgRegex;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a regular expression")
            }
            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                match Regex::new(v) {
                    Err(e) => Err(serde::de::Error::custom(format!(
                        "invalid regular expression '{v}': {e}"
                    ))),
                    Ok(x) => Ok(CfgRegex(x)),
                }
            }
        }
    }
}
