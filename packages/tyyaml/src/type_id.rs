use cu::pre::*;

use crate::TreeRepr;

/// A TyYAML Type ID, which is either a primitive type ([`Prim`]),
/// or a named type.
///
/// The `serde::Serialize` and print to TyYAML implementation
/// will put double quotes around named types, while the `to_string` implementation
/// will not.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Prim(Prim),
    Named(String),
}

impl From<Prim> for Ty {
    fn from(value: Prim) -> Self {
        Self::Prim(value)
    }
}

impl Ty {
    pub fn to_tyyaml(&self) -> String {
        let mut s = String::new();
        self.write_tyyaml(&mut s);
        s
    }
    /// Write the Type ID to a TyYAML buffer.
    pub fn write_tyyaml(&self, buf: &mut String) {
        use std::fmt::Write;
        match self {
            Self::Prim(ty) => write!(buf, "{ty}").unwrap(),
            Self::Named(ty) => write!(buf, "'\"{ty}\"'").unwrap(),
        }
    }
}

impl TreeRepr for Ty {
    fn serialize_spec(&self) -> cu::Result<String> {
        Ok(self.to_tyyaml())
    }
    fn deserialize_void() -> Self {
        Self::Prim(Prim::Void)
    }

    fn deserialize_spec(spec: &str) -> cu::Result<Self> {
        if spec.starts_with('"') {
            cu::ensure!(spec.ends_with('"'), "unterminated quoted Ty spec: '{spec}'");
            return Ok(Ty::Named(spec[1..spec.len() - 1].to_string()));
        }
        cu::check!(
            Prim::from_str(spec).map(Self::Prim),
            "invalid primitive Ty: '{spec}'"
        )
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prim(ty) => ty.fmt(f),
            Self::Named(name) => name.fmt(f),
        }
    }
}
impl Serialize for Ty {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        match self {
            Self::Prim(ty) => ty.serialize(ser),
            Self::Named(name) => ser.serialize_str(&format!("\"{name}\"")),
        }
    }
}
impl<'de> Deserialize<'de> for Ty {
    fn deserialize<D: serde::Deserializer<'de>>(der: D) -> Result<Self, D::Error> {
        struct Visitor;
        impl serde::de::Visitor<'_> for Visitor {
            type Value = Ty;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a primitive type, or a named type surrounded by quotes")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match Ty::deserialize_spec(v) {
                    Ok(x) => Ok(x),
                    Err(e) => Err(serde::de::Error::custom(format!(
                        "failed to deserialize Ty: {e:?}"
                    ))),
                }
            }
        }

        der.deserialize_str(Visitor)
    }
}

/// A primitive TyYAML type id.
///
/// The `to_str`, `to_string` (`Display`), `serde::Serialize` to YAML, and the TyYAML representation,
/// all have the same output.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Prim {
    Void,
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    F128,
}

impl Prim {
    pub fn iter() -> impl Iterator<Item = Prim> {
        [
            Self::Void,
            Self::Bool,
            Self::U8,
            Self::U16,
            Self::U32,
            Self::U64,
            Self::U128,
            Self::I8,
            Self::I16,
            Self::I32,
            Self::I64,
            Self::I128,
            Self::F32,
            Self::F64,
            Self::F128,
        ]
        .into_iter()
    }
    /// Convert self to string representation
    ///
    /// The `to_str`, `to_string` (`Display`), `serde::Serialize` to YAML, and the TyYAML representation,
    /// all have the same output.
    pub const fn to_str(&self) -> &'static str {
        match self {
            Prim::Void => "void",
            Prim::Bool => "bool",
            Prim::U8 => "u8",
            Prim::U16 => "u16",
            Prim::U32 => "u32",
            Prim::U64 => "u64",
            Prim::U128 => "u128",
            Prim::I8 => "i8",
            Prim::I16 => "i16",
            Prim::I32 => "i32",
            Prim::I64 => "i64",
            Prim::I128 => "i128",
            Prim::F32 => "f32",
            Prim::F64 => "f64",
            Prim::F128 => "f128",
        }
    }

    pub const fn to_cpp(&self) -> &'static str {
        match self {
            Prim::Void => "void",
            Prim::Bool => "bool",
            Prim::U8 => "uint8_t",
            Prim::U16 => "uint16_t",
            Prim::U32 => "uint32_t",
            Prim::U64 => "uint64_t",
            Prim::U128 => "uint128_t",
            Prim::I8 => "int8_t",
            Prim::I16 => "int16_t",
            Prim::I32 => "int32_t",
            Prim::I64 => "int64_t",
            Prim::I128 => "int128_t",
            Prim::F32 => "float32_t",
            Prim::F64 => "float64_t",
            Prim::F128 => "float128_t",
        }
    }

    /// Convert from string representation to self.
    pub fn from_str(x: &str) -> Option<Self> {
        Some(match x {
            "void" => Prim::Void,
            "bool" => Prim::Bool,
            "u8" => Prim::U8,
            "u16" => Prim::U16,
            "u32" => Prim::U32,
            "u64" => Prim::U64,
            "u128" => Prim::U128,
            "i8" => Prim::I8,
            "i16" => Prim::I16,
            "i32" => Prim::I32,
            "i64" => Prim::I64,
            "i128" => Prim::I128,
            "f32" => Prim::F32,
            "f64" => Prim::F64,
            "f128" => Prim::F128,
            _ => return None,
        })
    }

    pub const fn byte_size(self) -> Option<u32> {
        Some(match self {
            Prim::Void => return None,
            Prim::Bool => 1,
            Prim::U8 => 1,
            Prim::U16 => 2,
            Prim::U32 => 4,
            Prim::U64 => 8,
            Prim::U128 => 16,
            Prim::I8 => 1,
            Prim::I16 => 2,
            Prim::I32 => 4,
            Prim::I64 => 8,
            Prim::I128 => 16,
            Prim::F32 => 4,
            Prim::F64 => 8,
            Prim::F128 => 16,
        })
    }
}

impl std::fmt::Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_str().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typrim_to_string() -> cu::Result<()> {
        assert_eq!(yaml::stringify(&Prim::Void)?.trim(), Prim::Void.to_string());
        assert_eq!(yaml::stringify(&Prim::Bool)?.trim(), Prim::Bool.to_string());
        assert_eq!(yaml::stringify(&Prim::U8)?.trim(), Prim::U8.to_string());
        assert_eq!(yaml::stringify(&Prim::U16)?.trim(), Prim::U16.to_string());
        assert_eq!(yaml::stringify(&Prim::U32)?.trim(), Prim::U32.to_string());
        assert_eq!(yaml::stringify(&Prim::U64)?.trim(), Prim::U64.to_string());
        assert_eq!(yaml::stringify(&Prim::U128)?.trim(), Prim::U128.to_string());
        assert_eq!(yaml::stringify(&Prim::I8)?.trim(), Prim::I8.to_string());
        assert_eq!(yaml::stringify(&Prim::I16)?.trim(), Prim::I16.to_string());
        assert_eq!(yaml::stringify(&Prim::I32)?.trim(), Prim::I32.to_string());
        assert_eq!(yaml::stringify(&Prim::I64)?.trim(), Prim::I64.to_string());
        assert_eq!(yaml::stringify(&Prim::I128)?.trim(), Prim::I128.to_string());
        assert_eq!(yaml::stringify(&Prim::F32)?.trim(), Prim::F32.to_string());
        assert_eq!(yaml::stringify(&Prim::F64)?.trim(), Prim::F64.to_string());
        assert_eq!(yaml::stringify(&Prim::F128)?.trim(), Prim::F128.to_string());
        Ok(())
    }
}
