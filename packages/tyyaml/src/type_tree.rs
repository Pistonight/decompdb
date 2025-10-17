use cu::pre::*;

/// A Generic Type Tree
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    /// Replace the base representation of the tree using a mapping function
    #[inline(always)]
    pub fn map<T, F: FnMut(Repr) -> T>(self, f: F) -> Tree<T> {
        let mut f: Box<dyn FnMut(Repr) -> T> = Box::new(f);
        self.map_impl(&mut f)
    }
    fn map_impl<'a, T>(self, f: &mut Box<dyn FnMut(Repr) -> T + 'a>) -> Tree<T> {
        match self {
            Tree::Base(x) => Tree::Base(f(x)),
            Tree::Array(x, len) => Tree::Array(Box::new(x.map_impl(f)), len),
            Tree::Ptr(x) => Tree::Ptr(Box::new(x.map_impl(f))),
            Tree::Sub(x) => {
                let mut x2 = Vec::with_capacity(x.len());
                let mut f_erased: Box<dyn FnMut(Repr) -> T> = Box::new(f);
                for t in x {
                    x2.push(t.map_impl(&mut f_erased));
                }
                Tree::Sub(x2)
            }
            Tree::Ptmd(x, y) => {
                let x = f(x);
                let y = y.map_impl(f);
                Tree::Ptmd(x, Box::new(y))
            }
            Tree::Ptmf(x, y) => {
                let x = f(x);
                let mut y2 = Vec::with_capacity(y.len());
                for t in y {
                    y2.push(t.map_impl(f));
                }
                Tree::Ptmf(x, y2)
            }
        }
    }

    /// Execute a function on each node of the type tree
    #[inline(always)]
    pub fn for_each<F: FnMut(&Repr) -> cu::Result<()>>(&self, f: F) -> cu::Result<()> {
        let mut f: Box<dyn FnMut(&Repr) -> cu::Result<()>> = Box::new(f);
        self.for_each_impl(&mut f)
    }
    fn for_each_impl<'a>(&self, f: 
        &mut Box<dyn FnMut(&Repr) -> cu::Result<()> + 'a>
    ) -> cu::Result<()> {
        match self {
            Tree::Base(x) => f(x),
            Tree::Array(x, _) => x.for_each_impl(f),
            Tree::Ptr(x) => x.for_each_impl(f),
            Tree::Sub(x) => {
                for t in x {
                    t.for_each_impl(f)?;
                }
                Ok(())
            }
            Tree::Ptmd(x, y) => {
                f(x)?;
                y.for_each_impl(f)
            }
            Tree::Ptmf(x, y) => {
                f(x)?;
                for t in y {
                    t.for_each_impl(f)?;
                }
                Ok(())
            }
        }
    }

    /// Execute a function on every base representation that is a base type of a PTMD or PTMF.
    /// The function might be called multiple times on the same base repr
    #[inline(always)]
    pub fn for_each_ptm_base<F: FnMut(&Repr)>(&self, f: F) {
        let mut f: Box<dyn FnMut(&Repr)> = Box::new(f);
        self.for_each_ptm_base_impl(&mut f)
    }
    fn for_each_ptm_base_impl<'a>(&self, f: 
        &mut Box<dyn FnMut(&Repr)+'a>
    ) {
        match self {
            Tree::Base(_) => {}
            Tree::Array(x, _) => x.for_each_ptm_base_impl(f),
            Tree::Ptr(x) => x.for_each_ptm_base_impl(f),
            Tree::Sub(x) => {
                for t in x {
                    t.for_each_ptm_base_impl(f);
                }
            }
            Tree::Ptmd(x, y) => {
                f(x);
                y.for_each_ptm_base_impl(f)
            }
            Tree::Ptmf(x, y) => {
                f(x);
                for t in y {
                    t.for_each_ptm_base_impl(f);
                }
            }
        }
    }

    /// Execute a function on each mutable node of the type tree
    #[inline(always)]
    pub fn for_each_mut<F: FnMut(&mut Repr) -> cu::Result<()>>(
        &mut self,
        f: F,
    ) -> cu::Result<()> {
        let mut f: Box<dyn FnMut(&mut Repr) -> cu::Result<()>> = Box::new(f);
        self.for_each_mut_impl(&mut f)
    }
    fn for_each_mut_impl<'a>(
        &mut self,
        f: &mut Box<dyn FnMut(&mut Repr) -> cu::Result<()> + 'a>,
    ) -> cu::Result<()> {
        match self {
            Tree::Base(x) => f(x),
            Tree::Array(x, _) => x.for_each_mut_impl(f),
            Tree::Ptr(x) => x.for_each_mut_impl(f),
            Tree::Sub(x) => {
                for t in x {
                    t.for_each_mut_impl(f)?;
                }
                Ok(())
            }
            Tree::Ptmd(x, y) => {
                f(x)?;
                y.for_each_mut_impl(f)
            }
            Tree::Ptmf(x, y) => {
                f(x)?;
                for t in y {
                    t.for_each_mut_impl(f)?;
                }
                Ok(())
            }
        }
    }
    
    /// Replace nodes with subtree using a replacer function. Returns None if no replacement
    /// are made.
    ///
    /// The replacer function must return None or Some(Tree::Base) for nodes that appear as base type for PTMD or PTMF.
    /// Otherwise, an error will be returned.
    #[inline(always)]
    pub fn to_replaced<F: FnMut(&Repr) -> Option<Self>>(
        &mut self,
        f: F,
    ) -> cu::Result<Option<Self>> 
    where Repr: Clone
    {
        let mut f: Box<dyn FnMut(&Repr) -> Option<Self>> = Box::new(f);
        self.to_replaced_impl(&mut f)
    }
    fn to_replaced_impl<'a>(&self, f: &mut Box<dyn FnMut(&Repr) -> Option<Self> + 'a>)
        -> cu::Result<Option<Self>>
    where Repr: Clone
    {
        match self {
            Tree::Base(x) => Ok(f(x)),
            Tree::Array(x, len) => {
                Ok(x.to_replaced_impl(f)?.map(|elem| Self::array(elem, *len)))
            }
            Tree::Ptr(x) => {
                Ok(x.to_replaced_impl(f)?.map(Self::ptr))
            }
            Tree::Sub(x) => {
                Ok(Self::to_replaced_impl_vec(x, f)?.map(Tree::Sub))
            }
            Tree::Ptmd(base, x) => {
                todo!() // Tree::Base case
                if f(base).is_some() {
                    cu::bail!("ptmd base type cannot be replaced with tree! check if the type is replacable before calling to_replaced");
                }
                Ok(x.to_replaced_impl(f)?.map(|new_x| Self::ptmd(base.clone(), new_x)))
            }
            Tree::Ptmf(base, x) => {
                if f(base).is_some() {
                    cu::bail!("ptmf base type cannot be replaced with tree! check if the type is replacable before calling to_replaced");
                }
                Ok(Self::to_replaced_impl_vec(x, f)?.map(|new_x| Self::ptmf(base.clone(), new_x)))
            }
        }
    }
    fn to_replaced_impl_vec<'a>(v: &[Self], f: &mut Box<dyn FnMut(&Repr) -> Option<Self> + 'a>)
        -> cu::Result<Option<Vec<Self>>>
    where Repr: Clone
    {
        let mut out: Vec<Self> = vec![];
        for (i, t) in v.iter().enumerate() {
            match t.to_replaced_impl(f)? {
                None => {
                    if !out.is_empty() {
                        out.push(t.clone())
                    }
                }
                Some(new_t) => {
                    if out.is_empty() {
                        out.reserve_exact(v.len());
                        for t in v.iter().take(i) {
                            out.push(t.clone());
                        }
                    }
                    out.push(new_t)
                }
            }
        }
        if out.is_empty() {
            return Ok(None)
        }
        Ok(Some(out))
    }
}

pub trait TreeRepr: Sized + std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash {
    /// Serialize the type into a spec string for TyYAML
    fn serialize_spec(&self) -> cu::Result<String>;
    /// Deserialize void type
    fn deserialize_void() -> Self;
    /// Deserialize type from spec string
    fn deserialize_spec(spec: &str) -> cu::Result<Self>;
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
impl<T: TreeRepr> Serialize for Tree<T> {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq as _;
        let mut seq = ser.serialize_seq(None)?;
        self.serialize_internal(&mut seq)?;
        seq.end()
    }
}
#[doc(hidden)]
impl<T: TreeRepr> Tree<T> {
    fn serialize_internal<S: serde::ser::SerializeSeq>(&self, seq: &mut S) -> Result<(), S::Error> {
        use serde::ser::Error;
        match self {
            Tree::Base(ty) => {
                match ty.serialize_spec() {
                    Ok(x) => seq.serialize_element(&x)?,
                    Err(e) => {
                        return Err(Error::custom(format!(
                            "failed to serialize TreeRepr to spec: {e:?}"
                        )));
                    }
                };
            }
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
                match base.serialize_spec() {
                    Ok(x) => seq.serialize_element(&x)?,
                    Err(e) => {
                        return Err(Error::custom(format!(
                            "failed to serialize TreeRepr to spec: {e:?}"
                        )));
                    }
                };
                seq.serialize_element("::")?;
                seq.serialize_element("*")?;
            }
            Tree::Ptmf(base, args) => {
                let retty = args
                    .get(0)
                    .expect("missing return type in pointer-to-member-function type");
                retty.serialize_internal(seq)?;

                match base.serialize_spec() {
                    Ok(x) => seq.serialize_element(&x)?,
                    Err(e) => {
                        return Err(Error::custom(format!(
                            "failed to serialize TreeRepr to spec: {e:?}"
                        )));
                    }
                };
                seq.serialize_element("::")?;
                seq.serialize_element("()")?;
                seq.serialize_element(&args[1..])?;
                seq.serialize_element("*")?;
            }
        }
        Ok(())
    }
}

impl<'de, T: TreeRepr> Deserialize<'de> for Tree<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        return deserializer.deserialize_seq(Visitor(std::marker::PhantomData));
        struct Visitor<T>(std::marker::PhantomData<T>);
        impl<'de, T: TreeRepr> serde::de::Visitor<'de> for Visitor<T> {
            type Value = Tree<T>;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a TyYAML TYPE")
            }
            fn visit_seq<A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
            ) -> Result<Self::Value, A::Error> {
                let Some(base_spec) = seq.next_element::<String>()? else {
                    return Err(serde::de::Error::custom("missing base type in TyYAML TYPE"));
                };
                let base = match T::deserialize_spec(&base_spec) {
                    Ok(x) => x,
                    Err(e) => {
                        return Err(serde::de::Error::custom(format!(
                            "failed to deserialize TreeRepr from spec: {e:?}"
                        )));
                    }
                };
                self.continue_visit(seq, Tree::Base(base))
            }
        }
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Spec {
            Str(String),
            Len([u32; 1]),
        }
        impl<'de, T: TreeRepr> Visitor<T> {
            fn continue_visit<A: serde::de::SeqAccess<'de>>(
                self,
                mut seq: A,
                mut base: Tree<T>,
            ) -> Result<Tree<T>, A::Error> {
                'visit_loop: loop {
                    let Some(spec) = seq.next_element::<Spec>()? else {
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
                    let m = match T::deserialize_spec(&spec) {
                        Ok(x) => x,
                        Err(e) => {
                            return Err(serde::de::Error::custom(format!(
                                "failed to deserialize TreeRepr from spec: {e:?}"
                            )));
                        }
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
        impl<'de, T: TreeRepr> Deserialize<'de> for SubroutineVec<T> {
            fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                deserializer.deserialize_seq(SubroutineVecVisitor(std::marker::PhantomData))
            }
        }
        struct SubroutineVecVisitor<T>(std::marker::PhantomData<T>);
        impl<'de, T: TreeRepr> serde::de::Visitor<'de> for SubroutineVecVisitor<T> {
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
                v.push(Tree::Base(T::deserialize_void()));
                while let Some(base) = seq.next_element::<Tree<T>>()? {
                    v.push(base);
                }
                Ok(SubroutineVec(v))
            }
        }
    }
}
