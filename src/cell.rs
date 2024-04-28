//! Cell type and context object for traversing AST.
//!
//! Based on `GhostCell`.
//! All method implementations copied verbatim from original version by paper's authors
//! https://gitlab.mpi-sws.org/FP/ghostcell/-/blob/master/ghostcell/src/lib.rs
//! and `ghost_cell` crate https://docs.rs/ghost-cell .
//!
//! `TransformCtx` plays the same role as `GhostToken` does for `GhostCell`.
//!
//! Only difference is that instead of using a lifetime to constrain the life of access tokens,
//! here we provide only an unsafe method `TransformCtx::new_unchecked` and the user must maintain
//! the invariant that only one context object may be "in play" at same time
//! (see below for exactly what "in play" means).
//!
//! This alteration removes a lifetime, and avoids the unergonomic pattern of all the code that
//! works with a structure containing `GCell`s needing to be within a single closure.

use std::cell::UnsafeCell;

/// Context object for traversing AST.
///
/// This object has 2 purposes:
/// 1. Acts as a token to guarantee user cannot obtain aliasing pointers to AST nodes.
/// 2. Contains a counter for number of dummy AST nodes in the AST.
///
/// When transform is complete, there must be 0 dummy nodes in the AST.
/// This allows the AST to be used as a "standard" AST again. The standard AST does not contain `Dummy`
/// enum variants, so it would be UB to allow such variants to exist. They can only exist temporarily
/// in the traversable AST.
/// `transform` checks the dummy count at end of traverse, and panics if it's non-zero.
#[repr(transparent)]
pub struct TraverseCtx {
    dummy_count: usize,
}

impl TraverseCtx {
    /// Create new context object for traversing AST.
    ///
    /// It is imperative that any code operating on a single AST does not have access to more
    /// than 1 context object. `GCell` uses this guarantee to make it impossible to obtain a `&mut`
    /// reference to any AST node while another reference exists. If more than 1 context object is
    /// "in play", this guarantee can be broken, and may lead to undefined behavior.
    ///
    /// This function is used internally by `transform`, but probably should not be used elsewhere.
    ///
    /// It is permissable to create multiple contexts which are never used together on the same AST.
    /// In practice, this means it is possible to transform multiple ASTs on different threads
    /// simultaneously.
    ///
    /// If operating on multiple ASTs together (e.g. concatenating 2 files), then a single context object
    /// must be used to access all the ASTs involved in the operation NOT 1 context object per AST.
    ///
    /// # SAFETY
    /// Caller must ensure only a single context object is used with any AST at one time.
    #[inline]
    pub unsafe fn new_unchecked() -> Self {
        Self { dummy_count: 0 }
    }

    pub fn dummy_count(&self) -> usize {
        self.dummy_count
    }

    #[inline]
    pub fn increment_dummy_count(&mut self) {
        self.dummy_count += 1;
    }

    // SAFETY: Caller must ensure that a dummy AST node is actually being removed from AST.
    // This is essential for the soundness of treating the AST as a "standard" AST again after traversal.
    #[inline]
    pub unsafe fn decrement_dummy_count(&mut self) {
        self.dummy_count -= 1;
    }
}

/// A cell type providing interior mutability, with aliasing rules enforced at compile time.
#[repr(transparent)]
pub struct GCell<T: ?Sized> {
    value: UnsafeCell<T>,
}

#[allow(dead_code)]
impl<T> GCell<T> {
    pub const fn new(value: T) -> Self {
        GCell {
            value: UnsafeCell::new(value),
        }
    }

    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }
}

#[allow(dead_code, unused_variables)]
impl<T: ?Sized> GCell<T> {
    #[inline]
    pub fn borrow<'a>(&'a self, ctx: &'a TraverseCtx) -> &'a T {
        unsafe { &*self.value.get() }
    }

    #[inline]
    pub fn borrow_mut<'a>(&'a self, ctx: &'a mut TraverseCtx) -> &'a mut T {
        unsafe { &mut *self.value.get() }
    }

    #[inline]
    pub const fn as_ptr(&self) -> *mut T {
        self.value.get()
    }

    #[inline]
    pub fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *self.value.get() }
    }

    #[inline]
    pub fn from_mut(t: &mut T) -> &mut Self {
        unsafe { &mut *(t as *mut T as *mut Self) }
    }
}

#[allow(dead_code)]
impl<T> GCell<[T]> {
    #[inline]
    pub fn as_slice_of_cells(&self) -> &[GCell<T>] {
        unsafe { &*(self as *const GCell<[T]> as *const [GCell<T>]) }
    }
}

#[allow(dead_code)]
impl<T> GCell<T> {
    #[inline]
    pub fn replace(&self, value: T, ctx: &mut TraverseCtx) -> T {
        std::mem::replace(self.borrow_mut(ctx), value)
    }

    #[inline]
    pub fn take(&self, ctx: &mut TraverseCtx) -> T
    where
        T: Default,
    {
        self.replace(T::default(), ctx)
    }
}

#[allow(dead_code)]
impl<T: Clone> GCell<T> {
    #[inline]
    pub fn clone(&self, ctx: &TraverseCtx) -> Self {
        GCell::new(self.borrow(ctx).clone())
    }
}

impl<T: Default> Default for GCell<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: ?Sized> AsMut<T> for GCell<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}

impl<T> From<T> for GCell<T> {
    fn from(t: T) -> Self {
        GCell::new(t)
    }
}

// SAFETY: `GhostCell` is `Send` + `Sync`, so `GCell` can be too
unsafe impl<T: ?Sized + Send> Send for GCell<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GCell<T> {}

/// Type alias for a shared ref to a `GCell`.
/// This is the interior-mutable equivalent to `oxc_allocator::Box`.
pub type SharedBox<'a, T> = &'a GCell<T>;

/// Type alias for a shared Vec
pub type SharedVec<'a, T> = oxc_allocator::Vec<'a, GCell<T>>;
