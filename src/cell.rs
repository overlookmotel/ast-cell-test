//! Cell type and token for traversing AST.
//!
//! Based on `GhostCell`.
//! See original paper: https://plv.mpi-sws.org/rustbelt/ghostcell/
//!
//! All method implementations copied verbatim from original version by paper's authors
//! https://gitlab.mpi-sws.org/FP/ghostcell/-/blob/master/ghostcell/src/lib.rs
//! and `ghost_cell` crate https://docs.rs/ghost-cell .
//!
//! Only difference is that instead of using a lifetime to constrain the life of access tokens,
//! here we provide only an unsafe method `Token::new_unchecked` and the user must maintain
//! the invariant that only one token may be "in play" at same time
//! (see below for exactly what "in play" means).
//!
//! This alteration removes a lifetime, and avoids the unergonomic pattern of all the code that
//! works with a structure containing `GCell`s needing to be within a single closure.

use std::cell::UnsafeCell;

/// Access token for traversing AST.
#[repr(transparent)]
pub struct Token(());

impl Token {
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
        Self(())
    }
}

/// A cell type providing interior mutability, with aliasing rules enforced at compile time.
#[repr(transparent)]
pub struct GCell<T: ?Sized> {
    value: UnsafeCell<T>,
}

// ----------------------------------------------------------------------------
// Methods copied verbatim from
// https://gitlab.mpi-sws.org/FP/ghostcell/-/blob/master/ghostcell/src/lib.rs
// Only changes made are removing the lifetime from `GCell` vs `GhostCell`,
// as we're guaranteeing the uniqueness of the `Token` without use of lifetimes
// (`Token::new_unchecked`).
// ----------------------------------------------------------------------------

impl<T> GCell<T> {
    /// Create new `GCell` containing `value`.
    #[inline]
    pub const fn new(value: T) -> Self {
        GCell {
            value: UnsafeCell::new(value),
        }
    }

    /// Unwrap contained value from an owned `GCell`.
    #[allow(dead_code)]
    #[inline]
    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }
}

impl<T: ?Sized> GCell<T> {
    /// Get an immutable reference to the cell's value that lives for as long as the
    /// token is immutably borrowed (the lifetime `'t`).
    #[inline]
    #[allow(unsafe_code)]
    #[allow(unused_variables)] // For `tk`
    pub fn borrow<'t>(&'t self, tk: &'t Token) -> &'t T {
        // SAFETY:
        // We know the token is borrowed at `'t`, and the token is borrowed immutably.
        // Therefore, nobody has a mutable reference to this token.
        // Therefore, any items in the set that are currently aliased would have been legal to
        // alias at `&'t T` as well, so we can take out an immutable reference to any of them,
        // as long as we make sure that nobody else can take a mutable reference to any item
        // in the set until we're done.
        unsafe { &*self.value.get() }
    }

    /// Get a mutable reference to the cell's value that lives for as long as the owning
    /// token is mutably borrowed (the lifetime `'t`).
    #[inline]
    #[allow(unsafe_code)]
    #[allow(unused_variables)] // For `tk`
    pub fn borrow_mut<'t>(&'t self, tk: &'t mut Token) -> &'t mut T {
        // SAFETY:
        // We know the token is borrowed at `'t`, and the token is borrowed mutably.
        // Therefore, nobody else has a mutable reference to this token.
        // As a result, all items in the set are currently unaliased, so we can take out a
        // mutable reference to any one of them, as long as we make sure that nobody else
        // take a mutable reference to any other item in the set until the current borrow
        // is done.
        unsafe { &mut *self.value.get() }
    }

    /// Returns a raw pointer to the underlying value in this cell.
    #[allow(dead_code)]
    #[inline]
    pub const fn as_ptr(&self) -> *mut T {
        self.value.get()
    }

    /// Returns a mutable reference to the underlying value.
    ///
    /// This call borrows `GCell` mutably (at compile-time) which guarantees
    /// that we possess the only reference.
    #[inline]
    pub fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *self.value.get() }
    }

    /// Returns a `&mut GCell<T>` from a `&mut T`.
    #[inline]
    pub fn from_mut(t: &mut T) -> &mut Self {
        unsafe { &mut *(t as *mut T as *mut Self) }
    }
}

impl<T> GCell<[T]> {
    /// Returns a `&[GCell<T>]` from a `&GCell<[T]>`.
    #[allow(dead_code)]
    #[inline]
    pub fn as_slice_of_cells(&self) -> &[GCell<T>] {
        unsafe { &*(self as *const GCell<[T]> as *const [GCell<T>]) }
    }
}

impl<T: Clone> GCell<T> {
    /// Convenience method to clone the `GCell` when `T` is `Clone`,
    /// as long as the token is available.
    #[allow(dead_code)]
    #[inline]
    pub fn clone(&self, tk: &Token) -> Self {
        GCell::new(self.borrow(tk).clone())
    }
}

impl<T: ?Sized> AsMut<T> for GCell<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}

// ----------------------------------------------------------------------------
// Methods copied verbatim from `ghost_cell` crate https://docs.rs/ghost-cell
// Only changes made are removing the lifetime from `GCell` vs `GhostCell`,
// as we're guaranteeing the uniqueness of the `Token` without use of lifetimes
// (`Token::new_unchecked`).
// ----------------------------------------------------------------------------

impl<T> GCell<T> {
    /// Returns the cell's value, replacing it with the supplied one.
    #[allow(dead_code)]
    #[inline]
    pub fn replace(&self, value: T, tk: &mut Token) -> T {
        std::mem::replace(self.borrow_mut(tk), value)
    }

    /// Returns the cell's value, replacing it with the default value.
    #[allow(dead_code)]
    #[inline]
    pub fn take(&self, tk: &mut Token) -> T
    where
        T: Default,
    {
        self.replace(T::default(), tk)
    }

    // `ghost_cell` crate's `swap` method omitted here as is marked as experimental
}

impl<T: Default> Default for GCell<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> GCell<[T]> {
    /// Returns a cell containing a slice from slice of cells.
    #[allow(dead_code)]
    #[allow(unsafe_code)]
    #[inline]
    pub fn from_slice_of_cells(slice: &[GCell<T>]) -> &Self {
        // SAFETY:
        // * Same lifetime.
        // * `GCell<T>` has the same in-memory representation as `T` (`repr(transparent)`).
        unsafe { &*(slice as *const _ as *const GCell<[T]>) }
    }
}

impl<T, const N: usize> GCell<[T; N]> {
    /// Returns a reference to an array of cells from a cell containing an array.
    #[allow(dead_code)]
    #[allow(unsafe_code)]
    #[inline]
    pub fn as_array_of_cells(&self) -> &[GCell<T>; N] {
        // SAFETY:
        // * Same lifetime.
        // * `GCell<T>` has the same in-memory representation as `T` (`repr(transparent)`).
        unsafe { &*(self.as_ptr() as *mut [GCell<T>; N]) }
    }

    /// Returns a cell containing an array from an array of cells.
    #[allow(dead_code)]
    #[allow(unsafe_code)]
    #[inline]
    pub fn from_array_of_cells(array: &[GCell<T>; N]) -> &Self {
        // SAFETY:
        // * Same lifetime.
        // * `GCell<T>` has the same in-memory representation as `T` (`repr(transparent)`).
        unsafe { &*(array as *const [GCell<T>; N] as *const Self) }
    }
}

impl<T> From<T> for GCell<T> {
    #[inline]
    fn from(t: T) -> Self {
        GCell::new(t)
    }
}

// `ghost_cell` crate's `as_tuple_of_cells` and `from_tuple_of_cells` omitted here as not useful

// ----------------------------------------------------------------------------
// New code
// ----------------------------------------------------------------------------

impl<T> GCell<T> {
    /// Extend lifetime of a `SharedBox` to `'a`.
    /// SAFETY:
    /// Caller must ensure the data pointer to does live this long.
    pub unsafe fn extend_lifetime<'a>(&self) -> &'a GCell<T> {
        std::mem::transmute(self)
    }
}

impl<T: Sized> GCell<T> {
    /// Swap the contents of two `SharedBox`s.
    #[inline]
    #[allow(dead_code)]
    #[allow(unused_variables)] // For `tk`
    pub fn swap(&self, other: &Self, tk: &mut Token) {
        // Here we break out of `GCell`'s usual constraints and mutate the contents of 2 cells
        // at the same time.
        // This is only safe to do because we do a runtime check that the pointers don't alias.
        let self_ptr = self.as_ptr();
        let other_ptr = other.as_ptr();

        // Exit if the pointers are the same - swapping is a no-op
        if self_ptr == other_ptr {
            return;
        }

        // It could be better to convert the 2 pointers to `&mut` refs and use `std::mem::swap`.
        // However, I'm not sure of the aliasing semantics. We can check that the pointers don't
        // point to same place, but still one could be the parent/ancestor of the other.
        // That might be a violation of the aliasing rules, as usually having a `&mut` ref
        // to the parent would preclude also getting a `&mut` ref to its child.
        // So I've played it safe and used raw pointers, to avoid this potential danger.
        //
        // https://doc.rust-lang.org/std/ptr/fn.swap_nonoverlapping.html says:
        // "The operation is “untyped” in the sense that data may be uninitialized
        // or otherwise violate the requirements of `T`."
        // That would imply that aliasing rules don't apply, beyond the safety invariant that
        // the two items can't overlap in memory.
        //
        // TODO: Figure out if can use `&mut`s instead, and whether that's advantageous.

        // SAFETY:
        // We have checked that `self` and `other` don't point to same location.
        // The `Sized` bound on `T` ensures we're not dealing with slices, which could overlap
        // even if pointers to start of the 2 slices aren't the same.
        // This function takes a `&mut Token`, which guarantees no other references exist to
        // any data in the set. So there can can be no references to either `self` or `other`.
        // Both pointers are created from references to `GCell<T>`s, and are therefore valid for
        // reads and writes of a `T`. For the same reason, they are properly aligned.
        unsafe {
            std::ptr::swap_nonoverlapping(self_ptr, other_ptr, 1);
        };
    }
}

// SAFETY: `GhostCell` is `Send` + `Sync`, so `GCell` can be too
unsafe impl<T: ?Sized + Send> Send for GCell<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GCell<T> {}

/// Type alias for a shared ref to a `GCell`.
/// This is the interior-mutable equivalent to `oxc_allocator::Box`.
pub type SharedBox<'a, T> = &'a GCell<T>;

/// Type alias for a shared Vec
#[allow(dead_code)]
pub type SharedVec<'a, T> = oxc_allocator::Vec<'a, GCell<T>>;
