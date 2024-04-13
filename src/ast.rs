#![allow(dead_code, clippy::enum_variant_names)]

//! This file defines 2 different versions of the AST.
//!
//! # AST versions
//!
//! 1. Standard version - using `Box<'a, T>` for references between types.
//! 2. Traversable version - identical, except references between types are `SharedBox<'a, T>`.
//!
//! The difference between the two is that the traversable version features interior mutability
//! (via `GCell`). So the traversable AST can be mutated with just an immutable `&` reference.
//! It can also be traversed in any direction (up or down).
//!
//! To avoid an expensive conversion process between the two AST versions, they are laid out in memory
//! exactly the same, and one can be transmuted to the other at zero cost.
//!
//! # Links between nodes in traversable AST
//!
//! `SharedBox<'a, T>` is an alias for `&'a GCell<T>`.
//! `GCell` is a cell type allowing interior mutability.
//! The traverable AST uses this type to allow traversing up or down the tree.
//!
//! It maintains the no-aliasing invariant that you cannot obtain a `&mut` ref to an AST node while
//! also holding any other references to that node by widening the constraints to:
//!
//! * You can hold as many immut `&` refs to AST nodes as you like simultaneously.
//! * To obtain a `&mut` ref to *any* node in the AST, you cannot simultaneously hold *any* refs
//!   to *any* node in the entire AST.
//!
//! These constraints are enforced via the borrow-checker, and have zero runtime cost.
//!
//! All AST nodes are doubly-linked - links down to their children, and a link up to their parent.
//! This allows limitless travel around the AST in any direction.
//!
//! NB: Holding a `SharedBox<T>` is not the same as holding the node itself. You can hold as many
//! `SharedBox<T>`s as you like at any time, even while holding a `&mut` ref to an AST node.
//! You just cannot "open" a box to obtain a mutable ref to the node it contains with
//! `SharedBox<T>::borrow_mut` while any other box is "open".
//!
//! # Cloning in traversable AST
//!
//! `GCell` is not `Clone` or `Copy` but references to it (`SharedBox<T>` aka `&GCell<T>`) are.
//! Therefore `SharedBox<T>`s can be passed around easily, copied, or stored in context.
//!
//! Cloning a `SharedBox<T>` only clones the *reference* and does not clone the *contents* of the box.
//! i.e. `let expr: SharedBox<Expression> = get_expr(); let expr2 = expr.clone();` does not create a 2nd
//! AST node, it only creates another *reference* to the *same* node.
//!
//! `enum` AST node types (e.g. `Expression`) are `Copy` and `Clone`. As they only contain
//! a `SharedBox<T>` and the enum discriminant.
//!
//! # Preventing illegal ASTs
//!
//! Without specific handling to prevent it, it is possible to generate illegal ASTs with the same
//! node connected to the AST in 2 or more places.
//! This would be legal (but weird) in the the traversable AST, but is not legal in the standard AST,
//! which uses `Box` for "links" between AST nodes. `Box` owns its contents, so you cannot do
//! e.g. `let x = get_node(); let b1 = Box(x); let b2 = Box(x);`
//!
//! We want to be able to transmute the traversable AST back to the standard AST, so we have to prevent
//! this in the traversable AST too. If we didn't, it would result in UB when using the standard AST
//! if it contains such illegal double-references.
//!
//! The method to prevent this is the `Orphan<T>` wrapper type.
//! All `take_*` methods, which remove nodes from the AST, return the node wrapped in an `Orphan<T>`.
//! All `set_*` methods, and other methods which attach nodes to the AST, only accept an `Orphan<T>`.
//! This prevents a node from being attached to the AST in more than 1 place, by insisting it must be
//! removed from it's current position in AST first.
//!
//! To maintain these invariants, it is essential that access to `.parent` and other fields containing
//! nodes are not public outside this file. Alteration of these fields must only be allowed via
//! methods (e.g. `take_*` and `set_*`), which enforce the no-duplicates rule.
//!
//! This implies that struct AST node types must *not* be `Clone`. If they were cloned, the `parent`
//! of the clone would be incorrect. Enum AST node types can be `Copy` and `Clone` as they don't have
//! a `parent` field. Each of their variants contains a `SharedBox<T>` ref to the specific node type,
//! and *those* contain the `parent`.
//!
//! # Cycles of nodes
//!
//! The above does not prevent circular references between nodes, or even a node whose parent is itself.
//! However, this is fine from a safety perspective. Such a circular set of nodes by definition cannot
//! be connected to the tree which extends down from `Program`, so it's "floating in space" unconnected
//! to the AST.
//!
//! Such a circular structure is illegal in the standard AST, and it would likely be UB to attempt to
//! traverse it in "standard land". But as all that's returned to "standard land" is the `Program`,
//! the illegal loop cannot be reached from there, and this unsound situation cannot arise.
//!
//! In the traversable AST, if you're in the middle of traversing a node and it's turned into part of
//! a cycle, this will lead to an infinite loop - a traversal which keeps going around in circles.
//! Not good, but not UB. It would not be possible to prevent this statically, and even checking for
//! circularity at runtime would be fairly expensive, so we leave it as a situation that consumer of
//! the AST must ensure they avoid themselves when modifying the AST.
//!
//! # SAFETY
//!
//! * The size, alignment, and layout of all AST node types and their "traversable" counterparts
//!   must be identical, so that transmuting `Statement` to `TraversableStatement` is sound.
//! * All types must be `#[repr(C)]` to ensure predictable type layouts.
//! * All enums must be `#[repr(C, u8)]` with explicit discriminants to ensure discriminants
//!   match between the "standard" and "traversable" types.
//! * The invariant that a node cannot be attached to the tree in 2 places must be upheld,
//!   and all access to AST nodes' fields which contain other nodes must be intermediated
//!   via a method which upholds this invariant.

// TODO: Create the "Traversable" types with a macro to ensure they cannot be out of sync,
// and apply `#[repr(C)]` (for structs) / `#[repr(C, u8)]` (for enums) programmatically,
// so can't get forgotten. Generate accessor methods (`take_*` etc) with a macro/codegen too.

// TODO: Place `parent` field in all types in same position to remove branches when setting/getting
// parent for an `Expression` or `Statement`.

// TODO: Re-order fields in AST types so they are efficiently packed with padding only at the end.
// When types were `#[repr(rust)]`, compiler re-ordered the fields itself, but since they're now
// `#[repr(C)]`, we need to do it ourselves. We could write a macro which produces code to inspect
// type layouts at compile time and error if there's padding in middle of a struct.
// This isn't mission-critical for safety, just a perf optimization, so the macro could be behind
// a feature, disabled by default, and we just compile with it enabled from time to time.

// TODO: Create an AST builder which returns nodes wrapped in `Orphan<T>`, ready to be attached
// to the AST.

use std::mem;

use oxc_allocator::{Box, Vec};

use crate::cell::{GCell, SharedBox, SharedVec, Token};

/// Module namespace for traversable AST node types
pub mod traversable {
    use super::*;

    pub type Program<'a> = traversable_program::TraversableProgram<'a>;
    pub type Statement<'a> = traversable_statement::TraversableStatement<'a>;
    pub type ExpressionStatement<'a> =
        traversable_expression_statement::TraversableExpressionStatement<'a>;
    pub type Expression<'a> = traversable_expression::TraversableExpression<'a>;
    pub type IdentifierReference<'a> =
        traversable_identifier_reference::TraversableIdentifierReference<'a>;
    pub type StringLiteral<'a> = traversable_string_literal::TraversableStringLiteral<'a>;
    pub type BinaryExpression<'a> = traversable_binary_expression::TraversableBinaryExpression<'a>;
    pub type UnaryExpression<'a> = traversable_unary_expression::TraversableUnaryExpression<'a>;
    pub type Parent<'a> = traversable_parent::TraversableParent<'a>;

    #[allow(unused_imports)]
    pub use super::Orphan;
}

/// Module namespace for traversable AST traits which allow mutating traversable AST.
/// Consumers of traversable AST will likely want to import this as a prelude:
/// `use ast::traversable_traits::*;`
#[allow(unused_imports)]
#[rustfmt::skip] // To keep `use` statements in same order as types are defined in below
pub mod traversable_traits {
    use super::*;

    pub use traversable_program::SharedProgram;
    pub use traversable_expression_statement::SharedExpressionStatement;
    pub use traversable_identifier_reference::SharedIdentifierReference;
    pub use traversable_string_literal::SharedStringLiteral;
    pub use traversable_binary_expression::SharedBinaryExpression;
    pub use traversable_unary_expression::SharedUnaryExpression;

    pub use super::Copyable;
}

/// Macro to assert equivalence in size and alignment between standard and traversable types
macro_rules! link_types {
    ($standard:ident, $traversable:ident) => {
        const _: () = {
            use std::mem::{align_of, size_of};
            assert!(size_of::<$standard>() == size_of::<$traversable>());
            assert!(align_of::<$standard>() == align_of::<$traversable>());
            assert!(size_of::<Box<$standard>>() == size_of::<&crate::cell::GCell<$traversable>>());
            assert!(
                align_of::<Box<$standard>>() == align_of::<&crate::cell::GCell<$traversable>>()
            );
        };
    };
}

mod orphan {
    use std::ops::Deref;

    /// Wrapper for AST nodes which have been disconnected from the AST.
    ///
    /// This type is central to preventing a node from being attached to the AST in multiple places.
    ///
    /// `Orphan` cannot be `Copy` or `Clone`, or it would allow creating a duplicate ref to the
    /// contained node. The original `Orphan` could be attached to the AST, and then the copy
    /// could also be attached to the AST elsewhere.
    #[repr(transparent)]
    pub struct Orphan<T>(T);

    impl<T> Orphan<T> {
        /// Wrap node to indicate it's disconnected from AST.
        /// SAFETY: Caller must ensure that `node` is not attached to the AST.
        #[inline]
        pub unsafe fn new(node: T) -> Self {
            Self(node)
        }

        /// Unwrap node from `Orphan<T>`.
        /// This should only be done before inserting it into the AST.
        /// Not unsafe as there is nothing bad you can do with an un-orphaned AST node.
        /// No APIs are provided to attach nodes to the AST, unless they're wrapped in `Orphan<T>`.
        #[inline]
        pub fn inner(self) -> T {
            self.0
        }
    }

    impl<T> Deref for Orphan<T> {
        type Target = T;

        #[inline]
        fn deref(&self) -> &T {
            &self.0
        }
    }
}
pub use orphan::Orphan;

// --------------------------------------------------------------------------------
// AST node types
// --------------------------------------------------------------------------------

#[derive(Debug)]
#[repr(C)]
pub struct Program<'a> {
    pub body: Vec<'a, Statement<'a>>,
}

// NB: Traversable types are defined in modules to avoid exporting them outside of `traversable`
// namespace, and because this is likely how we'd want a macro to spit it out.
mod traversable_program {
    use super::*;

    #[repr(C)]
    pub struct TraversableProgram<'a> {
        pub(super) body: SharedVec<'a, traversable::Statement<'a>>,
    }

    link_types!(Program, TraversableProgram);

    impl<'a> traversable::Program<'a> {
        pub fn body_len(&self) -> usize {
            self.body.len()
        }

        // NB: We could name these methods `body_stmt` / `set_body_stmt` etc, but this would make
        // macro which we'll use to generate these impls more complicated.
        // TODO: We could probably abstract much of this into methods on a `SharedVec` type.
        // TODO: Implement more `Vec` methods.
        pub fn body_item(&self, index: usize) -> SharedBox<traversable::Statement<'a>> {
            // TODO: Extend lifetme to `'a`?
            &self.body[index]
        }
    }

    pub trait SharedProgram<'a> {
        fn body_len(self, tk: &Token) -> usize;
        fn body_item(self, index: usize, tk: &Token) -> SharedBox<traversable::Statement<'a>>;
        fn set_body_item(
            self,
            index: usize,
            stmt: Orphan<traversable::Statement<'a>>,
            tk: &mut Token,
        );
        fn take_body_item(self, index: usize, tk: &mut Token)
            -> Orphan<traversable::Statement<'a>>;
        fn push_body_item(self, stmt: Orphan<traversable::Statement<'a>>, tk: &mut Token);
    }

    impl<'a> SharedProgram<'a> for SharedBox<'a, traversable::Program<'a>> {
        /// Convenience method for getting `body.len()` from a ref.
        fn body_len(self, tk: &Token) -> usize {
            self.borrow(tk).body.len()
        }

        /// Convenience method for getting a body statement from a ref.
        fn body_item(self, index: usize, tk: &Token) -> SharedBox<traversable::Statement<'a>> {
            // TODO: Extend lifetme to `'a`?
            &self.borrow(tk).body[index]
        }

        fn set_body_item(
            self,
            index: usize,
            stmt: Orphan<traversable::Statement<'a>>,
            tk: &mut Token,
        ) {
            stmt.set_parent(traversable::Parent::Program(self), tk);

            // Unsafe code here is a workaround for `oxc_allocator::Vec` not implementing `IndexMut`.
            // `bumpalo::collections::Vec` implements `IndexMut`, so `oxc_allocator::Vec` could too.
            // TODO: Do that instead and remove this `unsafe`.
            assert!(index < self.borrow(tk).body.len());
            // SAFETY: We checked `index` is in bounds.
            let item = unsafe { &mut *self.borrow_mut(tk).body.as_mut_ptr().add(index) };
            item.replace(stmt.inner(), tk);
        }

        fn take_body_item(
            self,
            index: usize,
            tk: &mut Token,
        ) -> Orphan<traversable::Statement<'a>> {
            // Unsafe code here is a workaround for `oxc_allocator::Vec` not implementing `IndexMut`.
            // `bumpalo::collections::Vec` implements `IndexMut`, so `oxc_allocator::Vec` could too.
            // TODO: Do that instead and remove this `unsafe`.
            assert!(index < self.borrow(tk).body.len());
            // SAFETY: We checked `index` is in bounds.
            let item = unsafe { &mut *self.borrow_mut(tk).body.as_mut_ptr().add(index) };
            let stmt = item.replace(traversable::Statement::Dummy, tk);
            stmt.remove_parent(tk);
            // SAFETY: We have removed `stmt` from the AST
            unsafe { Orphan::new(stmt) }
        }

        fn push_body_item(self, stmt: Orphan<traversable::Statement<'a>>, tk: &mut Token) {
            stmt.set_parent(traversable::Parent::Program(self), tk);
            self.borrow_mut(tk).body.push(GCell::new(stmt.inner()));
        }
    }
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Statement<'a> {
    Dummy = 0,
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>) = 1,
}

// `Dummy` variant is a temporary placeholder indicating that a node has been removed from the AST.
// A valid AST should not contain any dummy nodes when transformation is complete, but it's required
// to have this placeholder so you can remove a node from the AST in order to insert it somewhere else
// instead. It's expected you'll replace the placeholder again with some other node.
// Failure to do so will not lead to memory unsafety or UB, but it's not a valid AST and other tools
// should probably panic if they find one.
//
// I cannot see any way to statically prevent this. `take_*` methods could return a marker type which
// panics in it's `Drop` impl. When inserting a node back into same place in the AST again, this marker
// type would be consumed without dropping it. If nothing is reinserted, the marker type will get dropped
// at end of the function where it was created, and will panic.
// But that is a runtime panic, not const time, so while it's probably better to trigger the panic early,
// during the transform, as you'll be able to see where the mistake is, it's still not ideal.
// Rust should not include the pannicking `drop` function in output if it can prove it won't be called.
// But in any function which can panic, it will have to include it as `drop` gets called during unwinding.
// This is probably not solveable.
mod traversable_statement {
    use super::*;

    // NB: `Copy` because it's only 16 bytes
    #[derive(Clone, Copy)]
    #[repr(C, u8)]
    pub enum TraversableStatement<'a> {
        Dummy = 0,
        ExpressionStatement(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 1,
    }

    link_types!(Statement, TraversableStatement);

    impl<'a> traversable::Statement<'a> {
        pub(super) fn set_parent(&self, parent: traversable::Parent<'a>, tk: &mut Token) {
            use TraversableStatement::*;
            match self {
                ExpressionStatement(expr_stmt) => {
                    expr_stmt.borrow_mut(tk).parent = parent;
                }
                Dummy => {}
            }
        }

        #[inline]
        pub(super) fn remove_parent(&self, tk: &mut Token) {
            self.set_parent(traversable::Parent::None, tk);
        }
    }

    impl<'a> Copyable for traversable::Statement<'a> {}
}

#[derive(Debug)]
#[repr(C)]
pub struct ExpressionStatement<'a> {
    pub expression: Expression<'a>,
    pub parent: Parent<'a>,
}

mod traversable_expression_statement {
    use super::*;

    #[repr(C)]
    pub struct TraversableExpressionStatement<'a> {
        pub(super) expression: traversable::Expression<'a>,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(ExpressionStatement, TraversableExpressionStatement);

    impl<'a> traversable::ExpressionStatement<'a> {
        pub fn expression(&self) -> traversable::Expression<'a> {
            self.expression
        }

        pub fn parent(&self) -> traversable::Parent<'a> {
            self.parent
        }

        // SAFETY: Caller must ensure parent is set correctly to track whether node is
        // currently attached to AST or not. See doc comment at top of file.
        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
        }
    }

    pub trait SharedExpressionStatement<'a> {
        fn parent(self, tk: &Token) -> traversable::Parent<'a>;
        fn expression(self, tk: &Token) -> traversable::Expression<'a>;
        fn set_expression(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token);
        fn take_expression(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>>;
    }

    impl<'a> SharedExpressionStatement<'a> for SharedBox<'a, traversable::ExpressionStatement<'a>> {
        /// Convenience method for getting `parent` from a ref.
        fn parent(self, tk: &Token) -> traversable::Parent<'a> {
            self.borrow(tk).parent
        }

        /// Convenience method for getting `expression` from a ref.
        fn expression(self, tk: &Token) -> traversable::Expression<'a> {
            self.borrow(tk).expression
        }

        fn set_expression(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token) {
            expr.set_parent(traversable::Parent::ExpressionStatement(self), tk);
            self.borrow_mut(tk).expression = expr.inner();
        }

        fn take_expression(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>> {
            let expr = mem::replace(
                &mut self.borrow_mut(tk).expression,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            // SAFETY: We have removed `expr` from the AST
            unsafe { Orphan::new(expr) }
        }
    }
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Expression<'a> {
    Dummy = 0,
    StringLiteral(Box<'a, StringLiteral<'a>>) = 1,
    Identifier(Box<'a, IdentifierReference<'a>>) = 2,
    BinaryExpression(Box<'a, BinaryExpression<'a>>) = 3,
    UnaryExpression(Box<'a, UnaryExpression<'a>>) = 4,
}

mod traversable_expression {
    use super::*;

    // NB: `Copy` because it's only 16 bytes
    #[derive(Clone, Copy)]
    #[repr(C, u8)]
    pub enum TraversableExpression<'a> {
        Dummy = 0,
        StringLiteral(SharedBox<'a, traversable::StringLiteral<'a>>) = 1,
        Identifier(SharedBox<'a, traversable::IdentifierReference<'a>>) = 2,
        BinaryExpression(SharedBox<'a, traversable::BinaryExpression<'a>>) = 3,
        UnaryExpression(SharedBox<'a, traversable::UnaryExpression<'a>>) = 4,
    }

    link_types!(Expression, TraversableExpression);

    impl<'a> traversable::Expression<'a> {
        pub(super) fn set_parent(&self, parent: traversable::Parent<'a>, tk: &mut Token) {
            use TraversableExpression::*;
            match self {
                StringLiteral(expr) => {
                    expr.borrow_mut(tk).parent = parent;
                }
                Identifier(expr) => {
                    expr.borrow_mut(tk).parent = parent;
                }
                BinaryExpression(expr) => {
                    expr.borrow_mut(tk).parent = parent;
                }
                UnaryExpression(expr) => {
                    expr.borrow_mut(tk).parent = parent;
                }
                Dummy => {}
            }
        }

        pub(super) fn remove_parent(&self, tk: &mut Token) {
            self.set_parent(traversable::Parent::None, tk);
        }
    }

    impl<'a> Copyable for traversable::Expression<'a> {}
}

#[derive(Debug)]
#[repr(C)]
pub struct IdentifierReference<'a> {
    pub name: &'a str,
    pub parent: Parent<'a>,
}

mod traversable_identifier_reference {
    use super::*;

    #[repr(C)]
    pub struct TraversableIdentifierReference<'a> {
        pub name: &'a str,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(IdentifierReference, TraversableIdentifierReference);

    impl<'a> traversable::IdentifierReference<'a> {
        pub fn parent(&self) -> traversable::Parent<'a> {
            self.parent
        }

        // SAFETY: Caller must ensure parent is set correctly to track whether node is
        // currently attached to AST or not. See doc comment at top of file.
        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
        }
    }

    pub trait SharedIdentifierReference<'a> {
        fn parent(self, tk: &Token) -> traversable::Parent<'a>;
    }

    impl<'a> SharedIdentifierReference<'a> for SharedBox<'a, traversable::IdentifierReference<'a>> {
        /// Convenience method for getting `parent` from a ref.
        fn parent(self, tk: &Token) -> traversable::Parent<'a> {
            self.borrow(tk).parent
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StringLiteral<'a> {
    pub value: &'a str,
    pub parent: Parent<'a>,
}

mod traversable_string_literal {
    use super::*;

    #[repr(C)]
    pub struct TraversableStringLiteral<'a> {
        pub value: &'a str,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(StringLiteral, TraversableStringLiteral);

    impl<'a> traversable::StringLiteral<'a> {
        pub fn parent(&self) -> traversable::Parent<'a> {
            self.parent
        }

        // SAFETY: Caller must ensure parent is set correctly to track whether node is
        // currently attached to AST or not. See doc comment at top of file.
        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
        }
    }

    pub trait SharedStringLiteral<'a> {
        fn parent(self, tk: &Token) -> traversable::Parent<'a>;
    }

    impl<'a> SharedStringLiteral<'a> for SharedBox<'a, traversable::StringLiteral<'a>> {
        /// Convenience method for getting `parent` from a ref.
        fn parent(self, tk: &Token) -> traversable::Parent<'a> {
            self.borrow(tk).parent
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct BinaryExpression<'a> {
    pub left: Expression<'a>,
    pub operator: BinaryOperator,
    pub right: Expression<'a>,
    pub parent: Parent<'a>,
}

mod traversable_binary_expression {
    use super::*;

    #[repr(C)]
    pub struct TraversableBinaryExpression<'a> {
        pub(super) left: traversable::Expression<'a>,
        pub operator: BinaryOperator,
        pub(super) right: traversable::Expression<'a>,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(BinaryExpression, TraversableBinaryExpression);

    impl<'a> traversable::BinaryExpression<'a> {
        pub fn left(&self) -> traversable::Expression<'a> {
            self.left
        }

        pub fn right(&self) -> traversable::Expression<'a> {
            self.right
        }

        pub fn parent(self) -> traversable::Parent<'a> {
            self.parent
        }

        // SAFETY: Caller must ensure parent is set correctly to track whether node is
        // currently attached to AST or not. See doc comment at top of file.
        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
        }
    }

    pub trait SharedBinaryExpression<'a> {
        fn parent(self, tk: &Token) -> traversable::Parent<'a>;
        fn left(self, tk: &mut Token) -> traversable::Expression<'a>;
        fn right(self, tk: &mut Token) -> traversable::Expression<'a>;
        fn operator(self, tk: &mut Token) -> BinaryOperator;
        fn set_left(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token);
        fn set_right(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token);
        fn take_left(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>>;
        fn take_right(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>>;
    }

    impl<'a> SharedBinaryExpression<'a> for SharedBox<'a, traversable::BinaryExpression<'a>> {
        /// Convenience method for getting `parent` from a ref.
        fn parent(self, tk: &Token) -> traversable::Parent<'a> {
            self.borrow(tk).parent
        }

        /// Convenience method for getting `left` from a ref.
        fn left(self, tk: &mut Token) -> traversable::Expression<'a> {
            self.borrow(tk).left
        }

        /// Convenience method for getting `right` from a ref.
        fn right(self, tk: &mut Token) -> traversable::Expression<'a> {
            self.borrow(tk).right
        }

        /// Convenience method for getting `operator` from a ref.
        fn operator(self, tk: &mut Token) -> BinaryOperator {
            self.borrow(tk).operator
        }

        fn set_left(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token) {
            expr.set_parent(traversable::Parent::BinaryExpressionLeft(self), tk);
            self.borrow_mut(tk).left = expr.inner();
        }

        fn set_right(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token) {
            expr.set_parent(traversable::Parent::BinaryExpressionRight(self), tk);
            self.borrow_mut(tk).right = expr.inner();
        }

        fn take_left(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>> {
            let expr = mem::replace(
                &mut self.borrow_mut(tk).left,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            // SAFETY: We have removed `expr` from the AST
            unsafe { Orphan::new(expr) }
        }

        fn take_right(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>> {
            let expr = mem::replace(
                &mut self.borrow_mut(tk).right,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            // SAFETY: We have removed `expr` from the AST
            unsafe { Orphan::new(expr) }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
pub enum BinaryOperator {
    Equality = 0,
    StrictEquality = 1,
}

#[derive(Debug)]
#[repr(C)]
pub struct UnaryExpression<'a> {
    pub operator: UnaryOperator,
    pub argument: Expression<'a>,
    pub parent: Parent<'a>,
}

mod traversable_unary_expression {
    use super::*;

    #[repr(C)]
    pub struct TraversableUnaryExpression<'a> {
        pub operator: UnaryOperator,
        pub(super) argument: traversable::Expression<'a>,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(UnaryExpression, TraversableUnaryExpression);

    impl<'a> traversable::UnaryExpression<'a> {
        pub fn argument(&self) -> traversable::Expression<'a> {
            self.argument
        }

        pub fn parent(&self) -> traversable::Parent<'a> {
            self.parent
        }

        // SAFETY: Caller must ensure parent is set correctly to track whether node is
        // currently attached to AST or not. See doc comment at top of file.
        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
        }
    }

    pub trait SharedUnaryExpression<'a> {
        fn parent(self, tk: &Token) -> traversable::Parent<'a>;
        fn argument(self, tk: &mut Token) -> traversable::Expression<'a>;
        fn operator(self, tk: &mut Token) -> UnaryOperator;
        fn set_argument(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token);
        fn take_argument(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>>;
    }

    impl<'a> SharedUnaryExpression<'a> for SharedBox<'a, traversable::UnaryExpression<'a>> {
        /// Convenience method for getting `parent` from a ref.
        fn parent(self, tk: &Token) -> traversable::Parent<'a> {
            self.borrow(tk).parent
        }

        /// Convenience method for getting `argument` from a ref.
        fn argument(self, tk: &mut Token) -> traversable::Expression<'a> {
            self.borrow(tk).argument
        }

        /// Convenience method for getting `operator` from a ref.
        fn operator(self, tk: &mut Token) -> UnaryOperator {
            self.borrow(tk).operator
        }

        fn set_argument(self, expr: Orphan<traversable::Expression<'a>>, tk: &mut Token) {
            expr.set_parent(traversable::Parent::UnaryExpression(self), tk);
            self.borrow_mut(tk).argument = expr.inner();
        }

        fn take_argument(self, tk: &mut Token) -> Orphan<traversable::Expression<'a>> {
            let expr = mem::replace(
                &mut self.borrow_mut(tk).argument,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            // SAFETY: We have removed `expr` from the AST
            unsafe { Orphan::new(expr) }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
pub enum UnaryOperator {
    UnaryNegation = 0,
    UnaryPlus = 1,
    LogicalNot = 2,
    BitwiseNot = 3,
    Typeof = 4,
    Void = 5,
    Delete = 6,
}

/// Parent type used in standard AST.
///
/// It is not so useful in the standard AST though, as the actual parent is not accessible via this type
/// (such circular references would be UB in the standard AST).
///
/// This type is only present in the standard AST to maintain space for `TraversableParent`
/// in the traversable AST. In the traversable AST, it is possible (and necessary) to alter `parent`.
/// When creating a standard AST, all `parent` fields should be set to `Parent::None`.
/// `semantic` works on the traversable AST and will set `parent` on each node correctly
/// before the AST goes to the transformer.
///
/// If an AST has been through `semantic`, it can be used in the standard AST to get the *type*
/// of the parent e.g. `let is_top_level = matches!(node.parent, Parent::Program);`.
/// But if the AST has been modified in "standard AST land", this info may be inaccurate.
/// This will not be UB, just wrong.
#[derive(Debug)]
#[repr(C, u8)]
pub enum Parent<'a> {
    None = 0,
    Program(ParentPointer<Program<'a>>) = 1,
    ExpressionStatement(ParentPointer<ExpressionStatement<'a>>) = 2,
    BinaryExpressionLeft(ParentPointer<BinaryExpression<'a>>) = 3,
    BinaryExpressionRight(ParentPointer<BinaryExpression<'a>>) = 4,
    UnaryExpression(ParentPointer<UnaryExpression<'a>>) = 5,
}

/// Wrapper around pointer to parent.
/// Type is opaque to external consumer. Pointer it contains cannot be read or written,
/// and no API is provided to create one outside of this module.
/// Purpose is to be the substitute for `&GCell` which this is transmuted to in the traversable AST.
#[derive(Debug)]
#[repr(transparent)]
pub struct ParentPointer<T>(*const T);

/// Parent link type for traversable AST.
/// This type is `Copy`, to make them easy to pass around.
/// Currently 16 bytes, which is unfortunate as it appears in every AST node.
/// We could likely squeeze it down to 8 bytes using pointer tagging.
mod traversable_parent {
    use super::*;

    // NB: `Copy` because it's only 16 bytes
    #[derive(Clone, Copy)]
    #[repr(C, u8)]
    pub enum TraversableParent<'a> {
        None = 0,
        Program(SharedBox<'a, traversable::Program<'a>>) = 1,
        ExpressionStatement(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 2,
        BinaryExpressionLeft(SharedBox<'a, traversable::BinaryExpression<'a>>) = 3,
        BinaryExpressionRight(SharedBox<'a, traversable::BinaryExpression<'a>>) = 4,
        UnaryExpression(SharedBox<'a, traversable::UnaryExpression<'a>>) = 5,
    }

    link_types!(Parent, TraversableParent);

    impl<'a> TraversableParent<'a> {
        pub(super) fn is_none(&self) -> bool {
            matches!(self, Self::None)
        }

        pub(super) fn assert_none(&self) {
            assert!(self.is_none());
        }
    }
}

/// Convenience trait to copy `Copy` types without a temp var, where borrow-checker
/// complains otherwise. Equivalent to `.clone()`, but clippy doesn't flag it.
/// It's also useful to use `.copy()` instead of `.clone()` to indicate it's not
/// an expensive operation.
pub trait Copyable {
    #[inline]
    fn copy(self) -> Self
    where
        Self: Copy,
    {
        let me = self;
        #[allow(clippy::let_and_return)]
        me
    }
}
