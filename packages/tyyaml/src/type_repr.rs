use cu::pre::*;

use crate::{Prim, Ty, TyTree};

/// A TyYAML Type representation.
///
/// In TyYAML, types are represented as a sequence, which starts with a base type,
/// and the rest of the sequence builds up the whole type.
///
/// The `to_tyyaml` serialization will use the recommended YAML formatting, which
/// is what you see in documentation. The `to_string` implementation will print
/// a CPP-like type syntax, but note pointer-to-member types might not be the same syntax if the
/// pointee or return type is an array or a pointer-to-subroutine type.
pub type Type = TyTree<Ty>;
impl From<Prim> for Type {
    fn from(value: Prim) -> Self {
        Self::Base(Ty::Prim(value))
    }
}

impl Type {
    pub fn named(x: impl Into<String>) -> Self {
        Self::Base(Ty::Named(x.into()))
    }
    pub fn to_tyyaml(&self) -> String {
        let mut out = String::new();
        self.write_tyyaml(&mut out);
        out
    }
    pub fn write_tyyaml(&self, buf: &mut String) {
        buf.push_str("[ ");
        self.write_tyyaml_internal(buf);
        buf.push_str(" ]");
    }
    fn write_tyyaml_internal(&self, buf: &mut String) {
        use std::fmt::Write as _;
        match self {
            Self::Base(ty) => ty.write_tyyaml(buf),
            Self::Array(ty, len) => {
                ty.write_tyyaml_internal(buf);
                write!(buf, ",[{len}]").unwrap();
            }
            Self::Ptr(ty) => {
                ty.write_tyyaml_internal(buf);
                buf.push_str(",'*'");
            }
            Self::Sub(args) => {
                let mut iter = args.iter();
                let retty = iter.next().expect("missing return type in subroutine type");
                retty.write_tyyaml_internal(buf);

                buf.push_str(",'()',[");
                write_tyyaml_args(iter, buf);
                buf.push_str("]");
            }
            Self::Ptmd(base, pointee) => {
                pointee.write_tyyaml_internal(buf);
                buf.push_str(",");
                base.write_tyyaml(buf);
                buf.push_str(",'::','*'");
            }
            Self::Ptmf(base, args) => {
                let mut iter = args.iter();
                let retty = iter
                    .next()
                    .expect("missing return type in pointer-to-member-function type");
                retty.write_tyyaml_internal(buf);

                buf.push_str(",");
                base.write_tyyaml(buf);
                buf.push_str(",'::','()',[");
                write_tyyaml_args(iter, buf);
                buf.push_str("],'*'");
            }
        }
        fn write_tyyaml_args<'a, I: Iterator<Item = &'a Type>>(mut iter: I, buf: &mut String) {
            if let Some(first) = iter.next() {
                first.write_tyyaml(buf);
                for arg in iter {
                    buf.push_str(",");
                    arg.write_tyyaml(buf);
                }
            }
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Base(ty) => write!(f, "{ty}"),
            Self::Array(ty, len) => write!(f, "{ty}[{len}]"),
            Self::Ptr(ty) => {
                if let Self::Sub(args) = ty.as_ref() {
                    let mut iter = args.iter();
                    let retty = iter
                        .next()
                        .expect("missing return type in pointer-to-subroutine type");
                    write!(f, "{retty} (*)(")?;

                    write_tyyaml_args(iter, f)?;
                    write!(f, ")")
                } else {
                    write!(f, "{ty}*")
                }
            }
            Self::Sub(args) => {
                let mut iter = args.iter();
                let retty = iter.next().expect("missing return type in subroutine type");
                write!(f, "{retty}(")?;

                write_tyyaml_args(iter, f)?;
                write!(f, ")")
            }
            // note that this will not be the correct CPP type syntax
            // if pointee is a pointer-to-subroutine type
            Self::Ptmd(base, pointee) => write!(f, "{pointee} {base}::*"),
            Self::Ptmf(base, args) => {
                let mut iter = args.iter();
                let retty = iter
                    .next()
                    .expect("missing return type in pointer-to-member-function type");
                write!(f, "{retty} ({base}::*)(")?;

                write_tyyaml_args(iter, f)?;
                write!(f, ")")
            }
        };
        fn write_tyyaml_args<'a, I: Iterator<Item = &'a Type>>(
            mut iter: I,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            if let Some(first) = iter.next() {
                write!(f, "{first}")?;
                for arg in iter {
                    write!(f, ", {arg}")?;
                }
            }
            Ok(())
        }
    }
}
impl Serialize for Type {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq as _;
        let mut seq = ser.serialize_seq(None)?;
        self.serialize_internal(&mut seq)?;
        seq.end()
    }
}
#[doc(hidden)]
impl Type {
    fn serialize_internal<S: serde::ser::SerializeSeq>(&self, seq: &mut S) -> Result<(), S::Error> {
        match self {
            Type::Base(ty) => seq.serialize_element(ty)?,
            Type::Array(ty, len) => {
                ty.serialize_internal(seq)?;
                seq.serialize_element(&[len])?;
            }
            Type::Ptr(ty) => {
                ty.serialize_internal(seq)?;
                seq.serialize_element("*")?;
            }
            Type::Sub(args) => {
                let retty = args.get(0).expect("missing return type in subroutine type");
                retty.serialize_internal(seq)?;

                seq.serialize_element("()")?;
                seq.serialize_element(&args[1..])?;
            }
            Type::Ptmd(base, pointee) => {
                pointee.serialize_internal(seq)?;
                seq.serialize_element(base)?;
                seq.serialize_element("::")?;
                seq.serialize_element("*")?;
            }
            Type::Ptmf(base, args) => {
                let retty = args
                    .get(0)
                    .expect("missing return type in pointer-to-member-function type");
                retty.serialize_internal(seq)?;

                seq.serialize_element(base)?;
                seq.serialize_element("::")?;
                seq.serialize_element("()")?;
                seq.serialize_element(&args[1..])?;
                seq.serialize_element("*")?;
            }
        }
        Ok(())
    }
}

impl<'de> Deserialize<'de> for Type {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        return deserializer.deserialize_seq(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Type;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a TyYAML TYPE")
            }
            fn visit_seq<A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
            ) -> Result<Self::Value, A::Error> {
                let Some(base) = seq.next_element::<Ty>()? else {
                    return Err(serde::de::Error::custom("missing base type in TyYAML TYPE"));
                };
                self.continue_visit(seq, Type::Base(base))
            }
        }
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Spec<'a> {
            Str(&'a str),
            Len([usize; 1]),
        }
        impl Visitor {
            fn continue_visit<'de, A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
                mut base: Type,
            ) -> Result<Type, A::Error> {
                'visit_loop: loop {
                    let Some(spec) = seq.next_element::<Spec<'_>>()? else {
                        return Ok(base);
                    };
                    let spec = match spec {
                        Spec::Str(x) => x,
                        Spec::Len(len) => {
                            // array
                            base = Type::Array(Box::new(base), len[0]);
                            continue 'visit_loop;
                        }
                    };
                    // pointer
                    if spec == "*" {
                        base = Type::Ptr(Box::new(base));
                        continue 'visit_loop;
                    }
                    // subroutine
                    if spec == "()" {
                        let Some(SubroutineVec(mut args)) = seq.next_element()? else {
                            return Err(serde::de::Error::custom(
                                "missing parameter list in TyYAML subroutine TYPE",
                            ));
                        };
                        *args
                            .get_mut(0)
                            .expect("missing return type in TyYAML subroutine") = base;

                        base = Type::Sub(args);
                        continue 'visit_loop;
                    }
                    let m = if spec.starts_with('"') {
                        if !spec.ends_with('"') {
                            return Err(serde::de::Error::custom(
                                "malformated TYPE_ID in TyYAML TYPE",
                            ));
                        }
                        Ty::Named(spec[1..spec.len() - 1].to_string())
                    } else {
                        let Some(m) = Prim::from_str(spec) else {
                            return Err(serde::de::Error::custom(
                                "malformated TYPE_ID in TyYAML TYPE",
                            ));
                        };
                        Ty::Prim(m)
                    };
                    if seq.next_element::<&str>()? != Some("::") {
                        return Err(serde::de::Error::custom(
                            "missing member spec ('::') in TyYAML pointer-to-member TYPE",
                        ));
                    }
                    let Some(ptm_spec) = seq.next_element::<&str>()? else {
                        return Err(serde::de::Error::custom(
                            "missing spec after '::' in TyYAML pointer-to-member TYPE",
                        ));
                    };
                    if ptm_spec == "*" {
                        base = Type::Ptmd(m, Box::new(base));
                        continue 'visit_loop;
                    }
                    if ptm_spec == "()" {
                        let Some(SubroutineVec(mut args)) = seq.next_element()? else {
                            return Err(serde::de::Error::custom(
                                "missing parameter list in TyYAML pointer-to-member-function TYPE",
                            ));
                        };
                        *args
                            .get_mut(0)
                            .expect("missing return type in TyYAML pointer-to-member-function") =
                            base;
                        // consume the last ptr spec
                        if seq.next_element::<&str>()? != Some("*") {
                            return Err(serde::de::Error::custom(
                                "missing pointer spec in TyYAML pointer-to-member-function TYPE",
                            ));
                        }

                        base = Type::Ptmf(m, args);
                        continue 'visit_loop;
                    }
                    return Err(serde::de::Error::custom("malformed TyYAML TYPE"));
                }
            }
        }
        struct SubroutineVec(Vec<Type>);
        impl<'de> Deserialize<'de> for SubroutineVec {
            fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                deserializer.deserialize_seq(SubroutineVecVisitor)
            }
        }
        struct SubroutineVecVisitor;
        impl<'de> serde::de::Visitor<'de> for SubroutineVecVisitor {
            type Value = SubroutineVec;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "subroutine parameters in a TyYAML TYPE")
            }
            fn visit_seq<A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
            ) -> Result<Self::Value, A::Error> {
                let mut v = match seq.size_hint() {
                    None => Vec::with_capacity(4),
                    Some(x) => Vec::with_capacity(x + 1),
                };
                // push a dummy value to take space for the return value
                v.push(Type::Base(Ty::Prim(Prim::Void)));
                while let Some(base) = seq.next_element::<Type>()? {
                    v.push(base);
                }
                Ok(SubroutineVec(v))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_type(t: impl Into<Type>, str_repr: &str, tyyaml_repr: &str) -> cu::Result<()> {
        let t = t.into();
        assert_eq!(t.to_string(), str_repr, "str repr mismatch for {t:?}");
        assert_eq!(t.to_tyyaml(), tyyaml_repr, "tyyaml repr mismatch for {t:?}");
        // deserialize tyyaml should become the type
        let tyyaml_parsed = yaml::parse::<Type>(tyyaml_repr)?;
        assert_eq!(t, tyyaml_parsed, "tyyaml parse mismatch");
        // serialize and deserialize should yield the same
        let stringified = yaml::stringify(&t)?;
        let reparsed = yaml::parse::<Type>(&stringified)?;
        assert_eq!(t, reparsed, "stringify re-parse mismatch");
        Ok(())
    }

    #[test]
    fn test_type_parsing_basic() -> cu::Result<()> {
        test_type(Prim::Void, "void", "[ void ]")?;
        test_type(Prim::Bool, "bool", "[ bool ]")?;
        test_type(Prim::U8, "u8", "[ u8 ]")?;
        test_type(Prim::U16, "u16", "[ u16 ]")?;
        test_type(Prim::U32, "u32", "[ u32 ]")?;
        test_type(Prim::U64, "u64", "[ u64 ]")?;
        test_type(Prim::I8, "i8", "[ i8 ]")?;
        test_type(Prim::I16, "i16", "[ i16 ]")?;
        test_type(Prim::I32, "i32", "[ i32 ]")?;
        test_type(Prim::I64, "i64", "[ i64 ]")?;
        test_type(Prim::F32, "f32", "[ f32 ]")?;
        test_type(Prim::F64, "f64", "[ f64 ]")?;
        test_type(Type::ptr(Prim::Void), "void*", "[ void,'*' ]")?;
        test_type(Type::ptr(Prim::Bool), "bool*", "[ bool,'*' ]")?;
        test_type(Type::ptr(Prim::U8), "u8*", "[ u8,'*' ]")?;
        test_type(Type::ptr(Prim::U16), "u16*", "[ u16,'*' ]")?;
        test_type(Type::ptr(Prim::U32), "u32*", "[ u32,'*' ]")?;
        test_type(Type::ptr(Prim::U64), "u64*", "[ u64,'*' ]")?;
        test_type(Type::ptr(Prim::I8), "i8*", "[ i8,'*' ]")?;
        test_type(Type::ptr(Prim::I16), "i16*", "[ i16,'*' ]")?;
        test_type(Type::ptr(Prim::I32), "i32*", "[ i32,'*' ]")?;
        test_type(Type::ptr(Prim::I64), "i64*", "[ i64,'*' ]")?;
        test_type(Type::ptr(Prim::F32), "f32*", "[ f32,'*' ]")?;
        test_type(Type::ptr(Prim::F64), "f64*", "[ f64,'*' ]")?;
        test_type(Type::array(Prim::Void, 5), "void[5]", "[ void,[5] ]")?;
        test_type(Type::array(Prim::Bool, 5), "bool[5]", "[ bool,[5] ]")?;
        test_type(Type::array(Prim::U8, 5), "u8[5]", "[ u8,[5] ]")?;
        test_type(Type::array(Prim::U16, 5), "u16[5]", "[ u16,[5] ]")?;
        test_type(Type::array(Prim::U32, 5), "u32[5]", "[ u32,[5] ]")?;
        test_type(Type::array(Prim::U64, 5), "u64[5]", "[ u64,[5] ]")?;
        test_type(Type::array(Prim::I8, 5), "i8[5]", "[ i8,[5] ]")?;
        test_type(Type::array(Prim::I16, 5), "i16[5]", "[ i16,[5] ]")?;
        test_type(Type::array(Prim::I32, 5), "i32[5]", "[ i32,[5] ]")?;
        test_type(Type::array(Prim::I64, 5), "i64[5]", "[ i64,[5] ]")?;
        test_type(Type::array(Prim::F32, 5), "f32[5]", "[ f32,[5] ]")?;
        test_type(Type::array(Prim::F64, 5), "f64[5]", "[ f64,[5] ]")?;
        test_type(
            Type::ptr(Type::ptr(Prim::Void)),
            "void**",
            "[ void,'*','*' ]",
        )?;
        test_type(
            Type::ptr(Type::ptr(Prim::Bool)),
            "bool**",
            "[ bool,'*','*' ]",
        )?;
        test_type(Type::ptr(Type::ptr(Prim::U8)), "u8**", "[ u8,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::U16)), "u16**", "[ u16,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::U32)), "u32**", "[ u32,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::U64)), "u64**", "[ u64,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::I8)), "i8**", "[ i8,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::I16)), "i16**", "[ i16,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::I32)), "i32**", "[ i32,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::I64)), "i64**", "[ i64,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::F32)), "f32**", "[ f32,'*','*' ]")?;
        test_type(Type::ptr(Type::ptr(Prim::F64)), "f64**", "[ f64,'*','*' ]")?;
        test_type(
            Type::array(Type::array(Prim::Void, 5), 5),
            "void[5][5]",
            "[ void,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::Bool, 5), 5),
            "bool[5][5]",
            "[ bool,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::U8, 5), 5),
            "u8[5][5]",
            "[ u8,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::U16, 5), 5),
            "u16[5][5]",
            "[ u16,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::U32, 5), 5),
            "u32[5][5]",
            "[ u32,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::U64, 5), 5),
            "u64[5][5]",
            "[ u64,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::I8, 5), 5),
            "i8[5][5]",
            "[ i8,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::I16, 5), 5),
            "i16[5][5]",
            "[ i16,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::I32, 5), 5),
            "i32[5][5]",
            "[ i32,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::I64, 5), 5),
            "i64[5][5]",
            "[ i64,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::F32, 5), 5),
            "f32[5][5]",
            "[ f32,[5],[5] ]",
        )?;
        test_type(
            Type::array(Type::array(Prim::F64, 5), 5),
            "f64[5][5]",
            "[ f64,[5],[5] ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::Void, 5)),
            "void[5]*",
            "[ void,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::Bool, 5)),
            "bool[5]*",
            "[ bool,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::U8, 5)),
            "u8[5]*",
            "[ u8,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::U16, 5)),
            "u16[5]*",
            "[ u16,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::U32, 5)),
            "u32[5]*",
            "[ u32,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::U64, 5)),
            "u64[5]*",
            "[ u64,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::I8, 5)),
            "i8[5]*",
            "[ i8,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::I16, 5)),
            "i16[5]*",
            "[ i16,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::I32, 5)),
            "i32[5]*",
            "[ i32,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::I64, 5)),
            "i64[5]*",
            "[ i64,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::F32, 5)),
            "f32[5]*",
            "[ f32,[5],'*' ]",
        )?;
        test_type(
            Type::ptr(Type::array(Prim::F64, 5)),
            "f64[5]*",
            "[ f64,[5],'*' ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::Void), 5),
            "void*[5]",
            "[ void,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::Bool), 5),
            "bool*[5]",
            "[ bool,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::U8), 5),
            "u8*[5]",
            "[ u8,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::U16), 5),
            "u16*[5]",
            "[ u16,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::U32), 5),
            "u32*[5]",
            "[ u32,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::U64), 5),
            "u64*[5]",
            "[ u64,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::I8), 5),
            "i8*[5]",
            "[ i8,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::I16), 5),
            "i16*[5]",
            "[ i16,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::I32), 5),
            "i32*[5]",
            "[ i32,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::I64), 5),
            "i64*[5]",
            "[ i64,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::F32), 5),
            "f32*[5]",
            "[ f32,'*',[5] ]",
        )?;
        test_type(
            Type::array(Type::ptr(Prim::F64), 5),
            "f64*[5]",
            "[ f64,'*',[5] ]",
        )?;

        test_type(Type::named("Foo"), "Foo", r#"[ '"Foo"' ]"#)?;
        test_type(Type::ptr(Type::named("Foo")), "Foo*", r#"[ '"Foo"','*' ]"#)?;
        test_type(
            Type::ptr(Type::ptr(Type::named("Foo"))),
            "Foo**",
            r#"[ '"Foo"','*','*' ]"#,
        )?;
        test_type(
            Type::array(Type::array(Type::named("Foo"), 7), 8),
            "Foo[7][8]",
            r#"[ '"Foo"',[7],[8] ]"#,
        )?;
        test_type(
            Type::ptr(Type::array(Type::named("Foo"), 7)),
            "Foo[7]*",
            r#"[ '"Foo"',[7],'*' ]"#,
        )?;
        test_type(
            Type::array(Type::ptr(Type::named("Foo")), 8),
            "Foo*[8]",
            r#"[ '"Foo"','*',[8] ]"#,
        )?;

        Ok(())
    }

    macro_rules! test_sub_type {
        ( ( $($arg:expr),* $(,)? ) -> $retty:expr, $str_repr:literal, $str_repr_ptr:literal, $yaml_repr:literal, $yaml_repr_ptr:literal ) => {{
            let sub_type = Type::Sub(vec![
                $retty.into(),
                $($arg.into()),*
            ]);
            test_type(sub_type.clone(), $str_repr, $yaml_repr)?;
            let sub_type_ptr = Type::ptr(sub_type);
            test_type(sub_type_ptr, $str_repr_ptr, $yaml_repr_ptr)?;
        }}
    }

    #[test]
    fn test_type_parsing_subroutine() -> cu::Result<()> {
        test_sub_type! { () -> Prim::Void, "void()", "void (*)()", "[ void,'()',[] ]", "[ void,'()',[],'*' ]" };
        test_sub_type! { () -> Type::named("Foo"), "Foo()", "Foo (*)()", r#"[ '"Foo"','()',[] ]"#, r#"[ '"Foo"','()',[],'*' ]"# };
        test_sub_type! { () -> Type::ptr(Prim::Void), "void*()", "void* (*)()", "[ void,'*','()',[] ]", "[ void,'*','()',[],'*' ]" };
        test_sub_type! { (Prim::Bool) -> Prim::Void, "void(bool)", "void (*)(bool)", "[ void,'()',[[ bool ]] ]", "[ void,'()',[[ bool ]],'*' ]" };
        test_sub_type! { (Prim::Bool, Type::ptr(Prim::Bool)) -> Prim::Void, "void(bool, bool*)", "void (*)(bool, bool*)", "[ void,'()',[[ bool ],[ bool,'*' ]] ]", "[ void,'()',[[ bool ],[ bool,'*' ]],'*' ]" };
        Ok(())
    }

    #[test]
    fn test_type_parsing_ptmd() -> cu::Result<()> {
        test_type(
            Type::ptmd(Ty::Named("A".to_string()), Prim::U64),
            "u64 A::*",
            r#"[ u64,'"A"','::','*' ]"#,
        )?;
        test_type(
            Type::ptmd(
                Ty::Named("A".to_string()),
                Type::ptr(Type::Sub(vec![Prim::U64.into(), Prim::Bool.into()])),
            ),
            "u64 (*)(bool) A::*",
            r#"[ u64,'()',[[ bool ]],'*','"A"','::','*' ]"#,
        )?;
        Ok(())
    }

    #[test]
    fn test_type_parsing_ptmf() -> cu::Result<()> {
        test_type(
            Type::ptmf(Ty::Named("A".to_string()), vec![Prim::U64.into()]),
            "u64 (A::*)()",
            r#"[ u64,'"A"','::','()',[],'*' ]"#,
        )?;
        test_type(
            Type::ptmf(
                Ty::Named("A".to_string()),
                vec![
                    Type::ptr(Type::Sub(vec![Prim::U64.into(), Prim::Bool.into()])),
                    Prim::U64.into(),
                ],
            ),
            "u64 (*)(bool) (A::*)(u64)",
            r#"[ u64,'()',[[ bool ]],'*','"A"','::','()',[[ u64 ]],'*' ]"#,
        )?;
        Ok(())
    }
}
