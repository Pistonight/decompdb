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
