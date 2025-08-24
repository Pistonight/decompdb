use derive_more::{Deref, DerefMut};

/// Trait for validating the config
pub trait Validate<S> {
    /// Validate the config object, as the `key` property in the parent object
    fn validate_property(&mut self, ctx: &mut ValidateCtx<S>, key: &str) -> cu::Result<()> {
        ctx.push(key);
        let x = self.validate(ctx);
        ctx.pop();
        x
    }

    /// Validate the config object, as the `index` index property in the parent object
    fn validate_index(&mut self, ctx: &mut ValidateCtx<S>, index: impl std::fmt::Display) -> cu::Result<()> {
        ctx.push_index(index);
        let x = self.validate(ctx);
        ctx.pop();
        x
    }

    /// Validate the config object as root. Implementers should not override this.
    fn validate_root_state(&mut self, state: S) -> cu::Result<()> {
        let mut ctx = ValidateCtx::new(state);
        self.validate(&mut ctx)
    }

    /// Validate the config object with context
    fn validate(&mut self, ctx: &mut ValidateCtx<S>) -> cu::Result<()>;
}

/// Context for config validation
#[derive(Default, Deref, DerefMut)]
pub struct ValidateCtx<S=()> {
    key: String,
    len_stack: Vec<usize>,
    #[deref]
    #[deref_mut]
    state: S
}
impl<S> ValidateCtx<S> {
    fn new(state: S) -> Self {
        Self {
            key: Default::default(),
            len_stack: Default::default(),
            state,
        }
    }
    /// Pop the last key path
    fn pop(&mut self) {
        match self.len_stack.pop() {
            None => self.key.clear(),
            Some(i) => self.key.truncate(i),
        }
    }

    /// Push a new key segment using `.` notation to the path.
    fn push(&mut self, key: &str) {
        self.len_stack.push(self.key.len());
        if self.key.is_empty() {
            self.key.push_str(key)
        } else {
            use std::fmt::Write;
            write!(self.key, ".{key}").expect("write config key failed");
        }
    }

    /// Push a new key segment using index notation `[i]` to the path.
    fn push_index(&mut self, key: impl std::fmt::Display) {
        use std::fmt::Write;
        self.len_stack.push(self.key.len());
        write!(self.key, "[{key}]").expect("write config index key failed");
    }

    /// Get the current key path being validated
    pub fn key(&self) -> &str {
        &self.key
    }

    /// Generate an FYI bail message for the current config key
    pub fn bail(&self) -> cu::Result<()> {
        cu::bailfyi!("error found at config key: {}", self.key);
    }
}

