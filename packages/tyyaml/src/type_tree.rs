/// A Generic Type Tree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyTree<Repr> {
    /// A basic type
    ///
    /// TyYAML representation is `[ TYPE_ID ]`
    Base(Repr),
    /// An array type
    ///
    /// TyYAML representation is `[ TYPE_ID,[LEN] ]`
    Array(Box<Self>, usize),
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

impl<Repr> TyTree<Repr> {
    /// Create a pointer type
    #[inline(always)]
    pub fn ptr(pointee: impl Into<Self>) -> Self {
        Self::Ptr(Box::new(pointee.into()))
    }
    /// Create an array type
    pub fn array(pointee: impl Into<Self>, len: usize) -> Self {
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
}
