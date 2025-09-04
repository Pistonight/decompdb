use crate::{Prim, Tree, Ty};

/// A TyYAML Type representation.
///
/// In TyYAML, types are represented as a sequence, which starts with a base type,
/// and the rest of the sequence builds up the whole type.
///
/// The `to_tyyaml` serialization will use the recommended YAML formatting, which
/// is what you see in documentation. The `to_string` implementation will print
/// a CPP-like type syntax, but note pointer-to-member types might not be the same syntax if the
/// pointee or return type is an array or a pointer-to-subroutine type.
pub type TyYaml = Tree<Ty>;
impl From<Prim> for TyYaml {
    fn from(value: Prim) -> Self {
        Tree::Base(Ty::Prim(value))
    }
}

impl TyYaml {
    pub fn named(x: impl Into<String>) -> Self {
        Tree::Base(Ty::Named(x.into()))
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
            Tree::Base(ty) => ty.write_tyyaml(buf),
            Tree::Array(ty, len) => {
                Self::write_tyyaml_internal(ty, buf);
                write!(buf, ",[{len}]").unwrap();
            }
            Tree::Ptr(ty) => {
                Self::write_tyyaml_internal(ty, buf);
                buf.push_str(",'*'");
            }
            Tree::Sub(args) => {
                let mut iter = args.iter();
                let retty = iter.next().expect("missing return type in subroutine type");
                Self::write_tyyaml_internal(retty, buf);

                buf.push_str(",'()',[");
                write_tyyaml_args(iter, buf);
                buf.push_str("]");
            }
            Tree::Ptmd(base, pointee) => {
                Self::write_tyyaml_internal(pointee, buf);
                buf.push_str(",");
                base.write_tyyaml(buf);
                buf.push_str(",'::','*'");
            }
            Tree::Ptmf(base, args) => {
                let mut iter = args.iter();
                let retty = iter
                    .next()
                    .expect("missing return type in pointer-to-member-function type");
                Self::write_tyyaml_internal(retty, buf);

                buf.push_str(",");
                base.write_tyyaml(buf);
                buf.push_str(",'::','()',[");
                write_tyyaml_args(iter, buf);
                buf.push_str("],'*'");
            }
        }
        fn write_tyyaml_args<'a, I: Iterator<Item = &'a Tree<Ty>>>(mut iter: I, buf: &mut String) {
            if let Some(first) = iter.next() {
                TyYaml::write_tyyaml(first, buf);
                for arg in iter {
                    buf.push_str(",");
                    TyYaml::write_tyyaml(arg, buf);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cu::pre::*;

    fn test_type(t: impl Into<TyYaml>, str_repr: &str, tyyaml_repr: &str) -> cu::Result<()> {
        let t = t.into();
        assert_eq!(t.to_string(), str_repr, "str repr mismatch for {t:?}");
        assert_eq!(t.to_tyyaml(), tyyaml_repr, "tyyaml repr mismatch for {t:?}");
        // deserialize tyyaml should become the type
        let tyyaml_parsed = yaml::parse::<TyYaml>(tyyaml_repr)?;
        assert_eq!(t, tyyaml_parsed, "tyyaml parse mismatch");
        // serialize and deserialize should yield the same
        let stringified = yaml::stringify(&t)?;
        let reparsed = yaml::parse::<TyYaml>(&stringified)?;
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
        test_type(TyYaml::ptr(Prim::Void), "void*", "[ void,'*' ]")?;
        test_type(TyYaml::ptr(Prim::Bool), "bool*", "[ bool,'*' ]")?;
        test_type(TyYaml::ptr(Prim::U8), "u8*", "[ u8,'*' ]")?;
        test_type(TyYaml::ptr(Prim::U16), "u16*", "[ u16,'*' ]")?;
        test_type(TyYaml::ptr(Prim::U32), "u32*", "[ u32,'*' ]")?;
        test_type(TyYaml::ptr(Prim::U64), "u64*", "[ u64,'*' ]")?;
        test_type(TyYaml::ptr(Prim::I8), "i8*", "[ i8,'*' ]")?;
        test_type(TyYaml::ptr(Prim::I16), "i16*", "[ i16,'*' ]")?;
        test_type(TyYaml::ptr(Prim::I32), "i32*", "[ i32,'*' ]")?;
        test_type(TyYaml::ptr(Prim::I64), "i64*", "[ i64,'*' ]")?;
        test_type(TyYaml::ptr(Prim::F32), "f32*", "[ f32,'*' ]")?;
        test_type(TyYaml::ptr(Prim::F64), "f64*", "[ f64,'*' ]")?;
        test_type(TyYaml::array(Prim::Void, 5), "void[5]", "[ void,[5] ]")?;
        test_type(TyYaml::array(Prim::Bool, 5), "bool[5]", "[ bool,[5] ]")?;
        test_type(TyYaml::array(Prim::U8, 5), "u8[5]", "[ u8,[5] ]")?;
        test_type(TyYaml::array(Prim::U16, 5), "u16[5]", "[ u16,[5] ]")?;
        test_type(TyYaml::array(Prim::U32, 5), "u32[5]", "[ u32,[5] ]")?;
        test_type(TyYaml::array(Prim::U64, 5), "u64[5]", "[ u64,[5] ]")?;
        test_type(TyYaml::array(Prim::I8, 5), "i8[5]", "[ i8,[5] ]")?;
        test_type(TyYaml::array(Prim::I16, 5), "i16[5]", "[ i16,[5] ]")?;
        test_type(TyYaml::array(Prim::I32, 5), "i32[5]", "[ i32,[5] ]")?;
        test_type(TyYaml::array(Prim::I64, 5), "i64[5]", "[ i64,[5] ]")?;
        test_type(TyYaml::array(Prim::F32, 5), "f32[5]", "[ f32,[5] ]")?;
        test_type(TyYaml::array(Prim::F64, 5), "f64[5]", "[ f64,[5] ]")?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::Void)),
            "void**",
            "[ void,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::Bool)),
            "bool**",
            "[ bool,'*','*' ]",
        )?;
        test_type(TyYaml::ptr(TyYaml::ptr(Prim::U8)), "u8**", "[ u8,'*','*' ]")?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::U16)),
            "u16**",
            "[ u16,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::U32)),
            "u32**",
            "[ u32,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::U64)),
            "u64**",
            "[ u64,'*','*' ]",
        )?;
        test_type(TyYaml::ptr(TyYaml::ptr(Prim::I8)), "i8**", "[ i8,'*','*' ]")?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::I16)),
            "i16**",
            "[ i16,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::I32)),
            "i32**",
            "[ i32,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::I64)),
            "i64**",
            "[ i64,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::F32)),
            "f32**",
            "[ f32,'*','*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(Prim::F64)),
            "f64**",
            "[ f64,'*','*' ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::Void, 5), 5),
            "void[5][5]",
            "[ void,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::Bool, 5), 5),
            "bool[5][5]",
            "[ bool,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::U8, 5), 5),
            "u8[5][5]",
            "[ u8,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::U16, 5), 5),
            "u16[5][5]",
            "[ u16,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::U32, 5), 5),
            "u32[5][5]",
            "[ u32,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::U64, 5), 5),
            "u64[5][5]",
            "[ u64,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::I8, 5), 5),
            "i8[5][5]",
            "[ i8,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::I16, 5), 5),
            "i16[5][5]",
            "[ i16,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::I32, 5), 5),
            "i32[5][5]",
            "[ i32,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::I64, 5), 5),
            "i64[5][5]",
            "[ i64,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::F32, 5), 5),
            "f32[5][5]",
            "[ f32,[5],[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::array(Prim::F64, 5), 5),
            "f64[5][5]",
            "[ f64,[5],[5] ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::Void, 5)),
            "void[5]*",
            "[ void,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::Bool, 5)),
            "bool[5]*",
            "[ bool,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::U8, 5)),
            "u8[5]*",
            "[ u8,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::U16, 5)),
            "u16[5]*",
            "[ u16,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::U32, 5)),
            "u32[5]*",
            "[ u32,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::U64, 5)),
            "u64[5]*",
            "[ u64,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::I8, 5)),
            "i8[5]*",
            "[ i8,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::I16, 5)),
            "i16[5]*",
            "[ i16,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::I32, 5)),
            "i32[5]*",
            "[ i32,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::I64, 5)),
            "i64[5]*",
            "[ i64,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::F32, 5)),
            "f32[5]*",
            "[ f32,[5],'*' ]",
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(Prim::F64, 5)),
            "f64[5]*",
            "[ f64,[5],'*' ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::Void), 5),
            "void*[5]",
            "[ void,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::Bool), 5),
            "bool*[5]",
            "[ bool,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::U8), 5),
            "u8*[5]",
            "[ u8,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::U16), 5),
            "u16*[5]",
            "[ u16,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::U32), 5),
            "u32*[5]",
            "[ u32,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::U64), 5),
            "u64*[5]",
            "[ u64,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::I8), 5),
            "i8*[5]",
            "[ i8,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::I16), 5),
            "i16*[5]",
            "[ i16,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::I32), 5),
            "i32*[5]",
            "[ i32,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::I64), 5),
            "i64*[5]",
            "[ i64,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::F32), 5),
            "f32*[5]",
            "[ f32,'*',[5] ]",
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(Prim::F64), 5),
            "f64*[5]",
            "[ f64,'*',[5] ]",
        )?;

        test_type(TyYaml::named("Foo"), "Foo", r#"[ '"Foo"' ]"#)?;
        test_type(
            TyYaml::ptr(TyYaml::named("Foo")),
            "Foo*",
            r#"[ '"Foo"','*' ]"#,
        )?;
        test_type(
            TyYaml::ptr(TyYaml::ptr(TyYaml::named("Foo"))),
            "Foo**",
            r#"[ '"Foo"','*','*' ]"#,
        )?;
        test_type(
            TyYaml::array(TyYaml::array(TyYaml::named("Foo"), 7), 8),
            "Foo[7][8]",
            r#"[ '"Foo"',[7],[8] ]"#,
        )?;
        test_type(
            TyYaml::ptr(TyYaml::array(TyYaml::named("Foo"), 7)),
            "Foo[7]*",
            r#"[ '"Foo"',[7],'*' ]"#,
        )?;
        test_type(
            TyYaml::array(TyYaml::ptr(TyYaml::named("Foo")), 8),
            "Foo*[8]",
            r#"[ '"Foo"','*',[8] ]"#,
        )?;

        Ok(())
    }

    macro_rules! test_sub_type {
        ( ( $($arg:expr),* $(,)? ) -> $retty:expr, $str_repr:literal, $str_repr_ptr:literal, $yaml_repr:literal, $yaml_repr_ptr:literal ) => {{
            let sub_type = TyYaml::Sub(vec![
                $retty.into(),
                $($arg.into()),*
            ]);
            test_type(sub_type.clone(), $str_repr, $yaml_repr)?;
            let sub_type_ptr = TyYaml::ptr(sub_type);
            test_type(sub_type_ptr, $str_repr_ptr, $yaml_repr_ptr)?;
        }}
    }

    #[test]
    fn test_type_parsing_subroutine() -> cu::Result<()> {
        test_sub_type! { () -> Prim::Void, "void()", "void (*)()", "[ void,'()',[] ]", "[ void,'()',[],'*' ]" };
        test_sub_type! { () -> TyYaml::named("Foo"), "Foo()", "Foo (*)()", r#"[ '"Foo"','()',[] ]"#, r#"[ '"Foo"','()',[],'*' ]"# };
        test_sub_type! { () -> TyYaml::ptr(Prim::Void), "void*()", "void* (*)()", "[ void,'*','()',[] ]", "[ void,'*','()',[],'*' ]" };
        test_sub_type! { (Prim::Bool) -> Prim::Void, "void(bool)", "void (*)(bool)", "[ void,'()',[[ bool ]] ]", "[ void,'()',[[ bool ]],'*' ]" };
        test_sub_type! { (Prim::Bool, TyYaml::ptr(Prim::Bool)) -> Prim::Void, "void(bool, bool*)", "void (*)(bool, bool*)", "[ void,'()',[[ bool ],[ bool,'*' ]] ]", "[ void,'()',[[ bool ],[ bool,'*' ]],'*' ]" };
        Ok(())
    }

    #[test]
    fn test_type_parsing_ptmd() -> cu::Result<()> {
        test_type(
            TyYaml::ptmd(Ty::Named("A".to_string()), Prim::U64),
            "u64 A::*",
            r#"[ u64,'"A"','::','*' ]"#,
        )?;
        test_type(
            TyYaml::ptmd(
                Ty::Named("A".to_string()),
                TyYaml::ptr(TyYaml::Sub(vec![Prim::U64.into(), Prim::Bool.into()])),
            ),
            "u64 (*)(bool) A::*",
            r#"[ u64,'()',[[ bool ]],'*','"A"','::','*' ]"#,
        )?;
        Ok(())
    }

    #[test]
    fn test_type_parsing_ptmf() -> cu::Result<()> {
        test_type(
            TyYaml::ptmf(Ty::Named("A".to_string()), vec![Prim::U64.into()]),
            "u64 (A::*)()",
            r#"[ u64,'"A"','::','()',[],'*' ]"#,
        )?;
        test_type(
            TyYaml::ptmf(
                Ty::Named("A".to_string()),
                vec![
                    TyYaml::ptr(TyYaml::Sub(vec![Prim::U64.into(), Prim::Bool.into()])),
                    Prim::U64.into(),
                ],
            ),
            "u64 (*)(bool) (A::*)(u64)",
            r#"[ u64,'()',[[ bool ]],'*','"A"','::','()',[[ u64 ]],'*' ]"#,
        )?;
        Ok(())
    }
}
