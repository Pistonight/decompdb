use cu::pre::*;

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
    /// Write the Type ID to a TyYAML buffer.
    pub fn write_tyyaml(&self, buf: &mut String) {
        use std::fmt::Write;
        match self {
            Self::Prim(ty) => write!(buf, "{ty}").unwrap(),
            Self::Named(ty) => write!(buf, "'\"{ty}\"'").unwrap(),
        }
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
                if v.starts_with('"') {
                    if !v.ends_with('"') {
                        return Err(serde::de::Error::custom(
                            "malformated TYPE_ID in TyYAML TYPE",
                        ));
                    }
                    return Ok(Ty::Named(v[1..v.len() - 1].to_string()));
                }
                match Prim::from_str(v) {
                    Some(ty) => Ok(Ty::Prim(ty)),
                    None => Err(serde::de::Error::custom(
                        "malformated TYPE_ID in TyYAML TYPE",
                    )),
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
