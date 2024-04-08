//! Cell type and token for traversing AST.
//! Just thin wrappers around `GhostCell` and `GhostToken`.
//!
//! Instead of using `GhostToken`'s method of creating tokens `GhostToken::new`, which then
//! means all code has to be within a closure, we use an unsafe API `GToken::new_unchecked`.
//! It is the caller's responsibility to ensure no more than 1 token is "in play" at any time.
//!
//! To block access to `GhostToken::new`, we have to wrap both `GhostToken` and `GhostCell`
//! in newtype wrappers which just forward calls to the underlying `GhostCell`.
//!
//! Without this wrapper, it would be possible to violate the constraint that there must only
//! ever be one token accessible to code at any time, with safe function calls, as follows:
//! ```
//! struct MyTransformer(GhostToken);
//! GhostToken::new(|token|) {
//!   let transformer = MyTransformer(token);
//!   // `transform` creates a token internally and passes it to `Traverse` methods.
//!   // so `transformer`'s methods now have access to 2 tokens, and can violate aliasing rules.
//!   transform(transformer, &mut program);
//! })
//! ```

use ghost_cell::{GhostCell, GhostToken};

/// Access token for traversing AST.
#[repr(transparent)]
pub struct Token<'t>(GhostToken<'t>);

impl<'t> Token<'t> {
    /// Create new access token for traversing AST.
    ///
    /// It is imperative that any code operating on a single AST does not have access to more
    /// than 1 token. `GCell` uses this guarantee to make it impossible to obtain a `&mut`
    /// reference to any AST node while another reference exists. If more than 1 token is "in play",
    /// this guarantee can be broken, and may lead to undefined behavior.
    ///
    /// This function is used internally by `transform`, but probably should not be used elsewhere.
    ///
    /// It is permissable to create multiple tokens which are never used together on the same AST.
    /// In practice, this means it is possible to transform multiple ASTs on different threads
    /// simultaneously.
    ///
    /// If operating on multiple ASTs together (e.g. concatenating 2 files), then a single token
    /// must be used to access all the ASTs involved in the operation NOT 1 token per AST.
    ///
    /// # SAFETY
    /// Caller must ensure only a single token is used with any AST at one time.
    #[inline]
    pub unsafe fn new_unchecked() -> Self {
        // Token is a ZST
        std::mem::transmute(())
    }
}

/// A cell type providing interior mutability, with aliasing rules enforced at compile time.
///
/// This type is just a thin wrapper around `GhostCell`.
#[repr(transparent)]
pub struct GCell<'t, T: ?Sized>(GhostCell<'t, T>);

#[allow(dead_code)]
impl<'t, T> GCell<'t, T> {
    #[inline]
    pub const fn new(value: T) -> Self {
        Self(GhostCell::new(value))
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }
}

#[allow(dead_code)]
impl<'t, T: ?Sized> GCell<'t, T> {
    #[inline]
    pub fn borrow<'a>(&'a self, tk: &'a Token<'t>) -> &'a T {
        self.0.borrow(&tk.0)
    }

    #[inline]
    pub fn borrow_mut<'a>(&'a self, tk: &'a mut Token<'t>) -> &'a mut T {
        self.0.borrow_mut(&mut tk.0)
    }

    #[inline]
    pub const fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    #[inline]
    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    #[inline]
    pub fn from_mut(t: &mut T) -> &mut Self {
        // SAFETY: As this wrapper type is `#[repr(transparent)]`, it's safe to transmute
        let inner = GhostCell::from_mut(t);
        unsafe { std::mem::transmute(inner) }
    }
}

#[allow(dead_code)]
impl<'t, T> GCell<'t, [T]> {
    #[inline]
    pub fn as_slice_of_cells(&self) -> &[GCell<'t, T>] {
        unsafe { &*(self as *const GCell<'t, [T]> as *const [GCell<'t, T>]) }
    }
}

#[allow(dead_code)]
impl<'t, T> GCell<'t, T> {
    #[inline]
    pub fn replace(&self, value: T, tk: &mut Token<'t>) -> T {
        self.0.replace(value, &mut tk.0)
    }

    #[inline]
    pub fn take(&self, tk: &mut Token<'t>) -> T
    where
        T: Default,
    {
        self.0.take(&mut tk.0)
    }
}

impl<'t, T: Default> Default for GCell<'t, T> {
    #[inline]
    fn default() -> Self {
        Self(GhostCell::default())
    }
}

impl<'t, T: ?Sized> AsMut<T> for GCell<'t, T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        self.0.as_mut()
    }
}

impl<'t, T> From<T> for GCell<'t, T> {
    #[inline]
    fn from(t: T) -> Self {
        Self(GhostCell::from(t))
    }
}

// SAFETY: `GhostCell` is `Send` + `Sync`, so this wrapper can be too
unsafe impl<'t, T: ?Sized + Send> Send for GCell<'t, T> {}
unsafe impl<'t, T: ?Sized + Send + Sync> Sync for GCell<'t, T> {}

/// Type alias for a shared ref to a `GCell`.
/// This is the interior-mutable equivalent to `oxc_allocator::Box`.
pub type SharedBox<'a, 't, T> = &'a GCell<'t, T>;

/// Macro to reduce boilerplate of defining `SharedBox` types.
/// `shared_box!(ExpressionStatement<'a, 't>)` -> `SharedBox<'a, 't, ExpressionStatement<'a, 't>>`
/// (which is equivalent to `&'a GCell<'t, ExpressionStatement<'a, 't>>`)
macro_rules! shared_box {
    ($ty:ident<$arena:lifetime, $token:lifetime>) => {
        $crate::cell::SharedBox<$arena, $token, $ty<$arena, $token>>
    };
}
pub(crate) use shared_box;

/// Macro to reduce boilerplate of `GCell` references.
/// `gcell!(ExpressionStatement<'a, 't>)` -> `GCell<'t, ExpressionStatement<'a, 't>>`
macro_rules! gcell {
    ($ty:ident<$arena:lifetime, $token:lifetime>) => {
        $crate::cell::GCell<$token, $ty<$arena, $token>>
    };
}
pub(crate) use gcell;

/// Type alias for a shared Vec
pub type SharedVec<'a, 't, T> = oxc_allocator::Vec<'a, GCell<'t, T>>;

/// Macro to reduce boilerplate of defining `SharedVec` types.
/// `shared_vec!(Statement<'a, 't>)` -> `SharedVec<'a, 't, Statement<'a, 't>>`
/// (which is equivalent to `Vec<'a, GCell<'t, Statement<'a, 't>>>`)
macro_rules! shared_vec {
    ($ty:ident<$arena:lifetime, $token:lifetime>) => {
        $crate::cell::SharedVec<$arena, $token, $ty<$arena, $token>>
    };
}
pub(crate) use shared_vec;
