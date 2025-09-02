use cu::pre::*;

/// A Generic Type Tree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tree<Repr> {
    /// A basic type
    ///
    /// TyYAML representation is `[ TYPE_ID ]`
    Base(Repr),
    /// An array type
    ///
    /// TyYAML representation is `[ TYPE_ID,[LEN] ]`
    Array(Box<Self>, u32),
    /// A pointer type
    ///
    /// TyYAML representation is `[ TYPE_ID,'*' ]`
    Ptr(Box<Self>),
    /// A subroutine type
    ///
    /// TyYAML representation is `[ RET_TYPE_ID,'()',[ ARG_TYPE, ... ] ]`.
    /// Note that this must be wrapped
    /// in a pointer to form a pointer-to-subroutine (i.e. function pointer) type.
    Sub(Vec<Self> /*[retty, args...]*/),
    /// A pointer-to-member-data type
    ///
    /// TyYAML representation is `[ VALUE_TYPE_ID,CLASS_TYPE_ID,'::','*' ]`
    Ptmd(Repr /*base*/, Box<Self> /*pointee*/),
    /// A pointer-to-member-function type
    ///
    /// TyYAML representation is `[ VALUE_TYPE_ID,CLASS_TYPE_ID,'::','()',[ ARG_TYPE, ...],'*' ]`
    Ptmf(Repr /*base*/, Vec<Self> /*[retty, args]*/),
}

impl<Repr> Tree<Repr> {
    /// Create a pointer type
    #[inline(always)]
    pub fn ptr(pointee: impl Into<Self>) -> Self {
        Self::Ptr(Box::new(pointee.into()))
    }
    /// Create an array type
    pub fn array(pointee: impl Into<Self>, len: u32) -> Self {
        Self::Array(Box::new(pointee.into()), len)
    }
    /// Create a pointer-to-member-data type
    pub fn ptmd(base: impl Into<Repr>, pointee: impl Into<Self>) -> Self {
        Self::Ptmd(base.into(), Box::new(pointee.into()))
    }
    /// Create a pointer-to-member-function type
    pub fn ptmf(base: impl Into<Repr>, sub: Vec<Self>) -> Self {
        Self::Ptmf(base.into(), sub)
    }

    /// Get the complexity (nesting level) of the type
    pub fn complexity<F: Fn(&Repr) -> usize>(&self, f: F) -> usize {
        match self {
            Tree::Base(x) => f(x),
            Tree::Array(elem, _) => 1 + elem.complexity(f),
            Tree::Ptr(pointee) => 1 + pointee.complexity(f),
            Tree::Sub(types) => {
                let Some(first) = types.first() else {
                    return 1;
                };
                1 + first.complexity(f)
            }
            Tree::Ptmd(_, pointee) => 2 + pointee.complexity(f),
            Tree::Ptmf(_, types) => {
                let Some(first) = types.first() else {
                    return 2;
                };
                2 + first.complexity(f)
            }
        }
    }

    pub fn map<T, F: FnMut(Repr) -> T>(self, mut f: F) -> Tree<T> {
        match self {
            Tree::Base(x) => Tree::Base(f(x)),
            Tree::Array(x, len) => Tree::Array(Box::new(x.map(f)), len),
            Tree::Ptr(x) => Tree::Ptr(Box::new(x.map(f))),
            Tree::Sub(x) => {
                let mut x2 = Vec::with_capacity(x.len());
                let mut f_erased: Box<dyn FnMut(Repr) -> T> = Box::new(f);
                for t in x {
                    x2.push(t.map(&mut f_erased));
                }
                Tree::Sub(x2)
            }
            Tree::Ptmd(x, y) => {
                let x = f(x);
                let y = y.map(f);
                Tree::Ptmd(x, Box::new(y))
            }
            Tree::Ptmf(x, y) => {
                let x = f(x);
                let mut y2 = Vec::with_capacity(y.len());
                let mut f_erased: Box<dyn FnMut(Repr) -> T> = Box::new(f);
                for t in y {
                    y2.push(t.map(&mut f_erased));
                }
                Tree::Ptmf(x, y2)
            }
        }
    }
    pub fn for_each<F: FnMut(&Repr) -> cu::Result<()>>(&self, mut f: F) -> cu::Result<()> {
        match self {
            Tree::Base(x) => f(x),
            Tree::Array(x, _) => x.for_each(f),
            Tree::Ptr(x) => x.for_each(f),
            Tree::Sub(x) => {
                let mut f_erased: Box<dyn FnMut(&Repr) -> cu::Result<()>> = Box::new(f);
                for t in x {
                    t.for_each(&mut f_erased)?;
                }
                Ok(())
            }
            Tree::Ptmd(x, y) => {
                f(x)?;
                y.for_each(f)
            }
            Tree::Ptmf(x, y) => {
                f(x)?;
                let mut f_erased: Box<dyn FnMut(&Repr) -> cu::Result<()>> = Box::new(f);
                for t in y {
                    t.for_each(&mut f_erased)?;
                }
                Ok(())
            }
        }
    }
    pub fn for_each_mut<F: FnMut(&mut Repr) -> cu::Result<()>>(
        &mut self,
        mut f: F,
    ) -> cu::Result<()> {
        match self {
            Tree::Base(x) => f(x),
            Tree::Array(x, _) => x.for_each_mut(f),
            Tree::Ptr(x) => x.for_each_mut(f),
            Tree::Sub(x) => {
                let mut f_erased: Box<dyn FnMut(&mut Repr) -> cu::Result<()>> = Box::new(f);
                for t in x {
                    t.for_each_mut(&mut f_erased)?;
                }
                Ok(())
            }
            Tree::Ptmd(x, y) => {
                f(x)?;
                y.for_each_mut(f)
            }
            Tree::Ptmf(x, y) => {
                f(x)?;
                let mut f_erased: Box<dyn FnMut(&mut Repr) -> cu::Result<()>> = Box::new(f);
                for t in y {
                    t.for_each_mut(&mut f_erased)?;
                }
                Ok(())
            }
        }
    }
}

pub trait TreeRepr: Sized {
    fn void() -> Self;
    fn deserialize_spec(spec: &str) -> Option<Self>;
}


impl<Repr: std::fmt::Display> std::fmt::Display for Tree<Repr> {
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
        fn write_tyyaml_args<
            'a,
            Repr: std::fmt::Display + 'a,
            I: Iterator<Item = &'a Tree<Repr>>,
        >(
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
impl<T: Serialize> Serialize for Tree<T> {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq as _;
        let mut seq = ser.serialize_seq(None)?;
        self.serialize_internal(&mut seq)?;
        seq.end()
    }
}
#[doc(hidden)]
impl<T: Serialize> Tree<T> {
    fn serialize_internal<S: serde::ser::SerializeSeq>(&self, seq: &mut S) -> Result<(), S::Error> {
        match self {
            Tree::Base(ty) => seq.serialize_element(ty)?,
            Tree::Array(ty, len) => {
                ty.serialize_internal(seq)?;
                seq.serialize_element(&[len])?;
            }
            Tree::Ptr(ty) => {
                ty.serialize_internal(seq)?;
                seq.serialize_element("*")?;
            }
            Tree::Sub(args) => {
                let retty = args.get(0).expect("missing return type in subroutine type");
                retty.serialize_internal(seq)?;

                seq.serialize_element("()")?;
                seq.serialize_element(&args[1..])?;
            }
            Tree::Ptmd(base, pointee) => {
                pointee.serialize_internal(seq)?;
                seq.serialize_element(base)?;
                seq.serialize_element("::")?;
                seq.serialize_element("*")?;
            }
            Tree::Ptmf(base, args) => {
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

impl<'de, T: Deserialize<'de> + TreeRepr> Deserialize<'de> for Tree<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        return deserializer.deserialize_seq(Visitor(std::marker::PhantomData));
        struct Visitor<T>(std::marker::PhantomData<T>);
        impl<'de, T: Deserialize<'de> + TreeRepr> serde::de::Visitor<'de> for Visitor<T> {
            type Value = Tree<T>;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a TyYAML TYPE")
            }
            fn visit_seq<A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
            ) -> Result<Self::Value, A::Error> {
                let Some(base) = seq.next_element::<T>()? else {
                    return Err(serde::de::Error::custom("missing base type in TyYAML TYPE"));
                };
                self.continue_visit(seq, Tree::Base(base))
            }
        }
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Spec<'a> {
            Str(&'a str),
            Len([u32; 1]),
        }
        impl<'de, T: Deserialize<'de> + TreeRepr> Visitor<T> {
            fn continue_visit<A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
                mut base: Tree<T>,
            ) -> Result<Tree<T>, A::Error> {
                'visit_loop: loop {
                    let Some(spec) = seq.next_element::<Spec<'_>>()? else {
                        return Ok(base);
                    };
                    let spec = match spec {
                        Spec::Str(x) => x,
                        Spec::Len(len) => {
                            // array
                            base = Tree::Array(Box::new(base), len[0]);
                            continue 'visit_loop;
                        }
                    };
                    // pointer
                    if spec == "*" {
                        base = Tree::Ptr(Box::new(base));
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

                        base = Tree::Sub(args);
                        continue 'visit_loop;
                    }
                    let Some(m) = T::deserialize_spec(spec) else {
                        return Err(serde::de::Error::custom(
                            "malformated TreeRepr in TyYAML TYPE",
                        ));
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
                        base = Tree::Ptmd(m, Box::new(base));
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

                        base = Tree::Ptmf(m, args);
                        continue 'visit_loop;
                    }
                    return Err(serde::de::Error::custom("malformed TyYAML TYPE"));
                }
            }
        }
        struct SubroutineVec<T>(Vec<Tree<T>>);
        impl<'de, T: Deserialize<'de> + TreeRepr> Deserialize<'de> for SubroutineVec<T> {
            fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                deserializer.deserialize_seq(SubroutineVecVisitor(std::marker::PhantomData))
            }
        }
        struct SubroutineVecVisitor<T>(std::marker::PhantomData<T>);
        impl<'de, T: Deserialize<'de> + TreeRepr> serde::de::Visitor<'de> for SubroutineVecVisitor<T> {
            type Value = SubroutineVec<T>;
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
                v.push(Tree::Base(T::void()));
                while let Some(base) = seq.next_element::<Tree<T>>()? {
                    v.push(base);
                }
                Ok(SubroutineVec(v))
            }
        }
    }
}
