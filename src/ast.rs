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
//!
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
//! All `replace_*` methods, and other methods which attach nodes to the AST, only accept an `Orphan<T>`.
//! This prevents a node from being attached to the AST in more than 1 place, by insisting it must be
//! removed from it's current position in AST first.
//!
//! To maintain these invariants, it is essential that access to `.parent` and other fields containing
//! nodes are not public outside this file. Alteration of these fields must only be allowed via
//! methods (e.g. `take_*` and `replace_*`), which enforce the no-duplicates rule.
//!
//! There must be **no** public API to obtain an owned struct AST node, otherwise it would be possible
//! to circumvent the invariant via `borrow_mut`.
//!
//! e.g.:
//! ```
//! // This is not possible
//! let unary_expr_mut = unary_expr_ref.borrow_mut(ctx);
//! *unary_expr_mut = get_owned_unary_expr_somehow();
//! // This is also not possible
//! unary_expr_ref.replace(get_owned_unary_expr_somehow(), ctx);
//! ```
//!
//! This implies that struct AST node types must **not** be `Clone`.
//!
//! Enum AST node types can be `Copy` and `Clone` as they don't have a `parent` field. Each of their
//! variants contains a `SharedBox<T>` ref to the specific node type, and *those* contain the `parent`.
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
//!   must be identical, so that transmuting `Program` to `TraversableProgram` is sound.
//! * All types must be `#[repr(C)]` to ensure predictable type layouts.
//! * All enums must be `#[repr(C, u8)]` with explicit discriminants to ensure discriminants
//!   match between the "standard" and "traversable" types.
//! * The invariant that a node cannot be attached to the tree in 2 places must be upheld,
//!   and all access to AST nodes' fields which contain other nodes must be intermediated
//!   via a method which upholds this invariant.

// TODO: Create the "Traversable" types with a macro to ensure they cannot be out of sync,
// and apply `#[repr(C)]` (for structs) / `#[repr(C, u8)]` (for enums) programmatically,
// so can't get forgotten. Generate accessor methods (`take_*` etc) with a macro/codegen too.

// TODO: Re-order fields in AST types so they are efficiently packed with padding only at the end.
// When types were `#[repr(rust)]`, compiler re-ordered the fields itself, but since they're now
// `#[repr(C)]`, we need to do it ourselves. We could write a macro which produces code to inspect
// type layouts at compile time and error if there's padding in middle of a struct.
// This isn't mission-critical for safety, just a perf optimization, so the macro could be behind
// a feature, disabled by default, and we just compile with it enabled from time to time,
// or in a separate "type sizes" job on CI.

// TODO: We can actually relax GhostCell's rules a little, enabled by the fact that the AST is a tree
// not a graph, and we've made it impossible for a node to be in the AST twice.
// Therefore, we can mutably borrow all of the properties of a node simultaneously, and can be sure
// they can't alias. e.g. can mutably borrow `left` and `right` of `BinaryExpression` simultaneously.
// NB: *Cannot* mut borrow `parent` at same time, as cycles in AST are possible, so e.g. `left`
// and `parent` could point to the same node.

// TODO: There is a problem with the dummy counter.
// It's sound, but some operations are made impossible.
// e.g. If you replace `typeof x` with `x`, you `take_argument` to get `x` which increments dummy count,
// then `replace_*` the location `typeof x` was in with `x`, which doesn't affect dummy count,
// and the old `UnaryExpression` `typeof <dummy>` is discarded.
// So we now have a valid AST containing no dummies, but dummy count is 1, which will cause a panic
// at end of transform.
// One solution would be to add a method `Orphan::discard` which traverses the node being discarded,
// and decrements the dummy count by the number of dummies it contains. But this would be expensive
// if the discarded tree is deep.
// Or a function to consume an `Orphan<T>` and get all its child nodes as `Orphan`s. If any of children
// are dummies, that would decrement dummy count.

// TODO: Enforce that no dummy AST nodes left in AST via static means.
// Only way I can see to do this is to turn `TransformCtx` into a little state machine.
// * `TransformCtx` becomes `TransformCtx<const DUMMY_COUNT: usize>`.
// * `visit_*` functions receive an owned `TransformCtx<const DUMMY_COUNT = 0>`.
// * `visit_*` functions have to return a `TransformCtx<const DUMMY_COUNT = 0>`.
// * `take_*` consumes `ctx` and returns new `TransformCtx` with count incremented.
//   `fn take_foo(&self, ctx: TraverseCtx<N>) -> (Orphan<Foo>, TraverseCtx<N+1>)`
// * `replace_*` consumes `ctx` returns new `TransformCtx` with count decremented.
//   `fn replace_foo(&self, foo: Orphan<Foo>, ctx: TraverseCtx<N>) -> (Orphan<Foo>, TraverseCtx<N-1>)`
// Therefore, each `visit_*` function cannot leave any dummies in AST when it exits, or when it calls `walk_*`.
// This would require preventing dummy nodes being taken from AST with `take_*` or inserted with `replace_*`,
// so that updated `TransformCtx` returned by these functions has static dummy count at type level.
// It would be tricky to implement `Orphan::discard` (see above) with this scheme.

// TODO: If provided `swap_*` methods, could we get rid of dummies entirely?
// `swap_expression` would take 2 `SharedBox<Expression>`s and swap their contents.
// Then there are never any dummies in the AST, and AST is valid at all times.

use std::mem;

use oxc_allocator::{Allocator, Box, Vec};

use crate::{
    cell::{GCell, SharedBox, SharedVec},
    traverse::TraverseCtx,
};

/// Module namespace for traversable AST node types
#[allow(unused_imports)]
pub mod traversable {
    pub use super::Orphan;
    pub use super::TraversableAstBuilder as AstBuilder;
    pub use super::TraversableBinaryExpression as BinaryExpression;
    pub use super::TraversableExpression as Expression;
    pub use super::TraversableExpressionStatement as ExpressionStatement;
    pub use super::TraversableIdentifierReference as IdentifierReference;
    pub use super::TraversableParent as Parent;
    pub use super::TraversableProgram as Program;
    pub use super::TraversableStatement as Statement;
    pub use super::TraversableStringLiteral as StringLiteral;
    pub use super::TraversableUnaryExpression as UnaryExpression;
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

/// Trait to sugar `GCell::from_mut(allocator.alloc(t))` to `allocator.galloc(t)`.
trait GCellAlloc {
    #[allow(clippy::mut_from_ref)]
    fn galloc<T>(&self, value: T) -> &mut GCell<T>;
}

impl GCellAlloc for Allocator {
    /// Allocate `T` into arena and return a `&mut GCell` to it
    #[inline]
    fn galloc<T>(&self, value: T) -> &mut GCell<T> {
        GCell::from_mut(self.alloc(value))
    }
}

/// AST builder for creating AST nodes for traversable AST
pub struct TraversableAstBuilder<'a> {
    pub allocator: &'a Allocator,
}

impl<'a> TraversableAstBuilder<'a> {
    pub fn new(allocator: &'a Allocator) -> Self {
        Self { allocator }
    }
}

// --------------------------------------------------------------------------------
// AST node types
// --------------------------------------------------------------------------------

#[derive(Debug)]
#[repr(C)]
pub struct Program<'a> {
    pub body: Vec<'a, Statement<'a>>,
}

#[repr(C)]
pub struct TraversableProgram<'a> {
    body: SharedVec<'a, traversable::Statement<'a>>,
}

link_types!(Program, TraversableProgram);

impl<'a> traversable::Program<'a> {
    pub fn body_len(&self) -> usize {
        self.body.len()
    }

    pub fn body_stmt(&self, index: usize, ctx: &TraverseCtx) -> traversable::Statement<'a> {
        *self.body[index].borrow(ctx)
    }
}

// TODO: We could probably abstract much of this into methods on a `SharedVec` type.
// TODO: Implement more `Vec` methods.
impl<'a> GCell<traversable::Program<'a>> {
    /// Convenience method for getting `body.len()` from a ref.
    pub fn body_len(&'a self, ctx: &TraverseCtx) -> usize {
        self.borrow(ctx).body.len()
    }

    /// Convenience method for getting a body statement from a ref.
    #[inline]
    pub fn body_stmt(&'a self, index: usize, ctx: &TraverseCtx) -> traversable::Statement<'a> {
        *self.borrow(ctx).body[index].borrow(ctx)
    }

    /// Replace statement at `index` of `Program` body, and return previous value.
    pub fn replace_body_stmt(
        &'a self,
        index: usize,
        stmt: Orphan<traversable::Statement<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Statement<'a>> {
        let stmt = stmt.inner();
        match stmt {
            traversable::Statement::Dummy => ctx.increment_dummy_count(),
            _ => stmt.set_parent(traversable::Parent::ProgramBody(self), ctx),
        }

        // Unsafe code here is a workaround for `oxc_allocator::Vec` not implementing `IndexMut`.
        // `bumpalo::collections::Vec` implements `IndexMut`, so `oxc_allocator::Vec` could too.
        // Currently code here is sound for `Program`, as `Program` cannot be in `Vec` of statements.
        // But for other types, it would not be sound.
        // e.g. `BlockStatement::body` contains statements, and we are unable to prevent circularity,
        // so a `BlockStatement`'s body `Vec` could contain itself.
        // As this stands, in such a case we would obtain an immut ref and a mut ref to same node
        // simultaneously, which violates the aliasing rules.
        // If `Vec` was `IndexMut`, I think can do this instead:
        // ```
        // let old_stmt = std::mem::replace(&mut self.borrow_mut(ctx).body[index], GCell::new(stmt.inner()))
        // let old_stmt = *old_stmt.borrow(ctx);
        // match old_stmt {
        //   // SAFETY: Dummy count is only decremented if node being removed from AST is a dummy
        //   traversable::Statement::Dummy => unsafe { ctx.decrement_dummy_count() },
        //   _ => old_stmt.remove_parent(ctx),
        // }
        // return unsafe { Orphan::new(old_stmt) };
        // ```
        // i.e. we replace the cell itself, rather than the *contents* of the cell.
        // TODO: Make `Vec` impl `IndexMut` and replace this dodgy unsafe code with the above.
        assert!(index < self.borrow(ctx).body.len());
        // SAFETY: We checked `index` is in bounds.
        let item = unsafe { &*self.borrow(ctx).body.as_ptr().add(index) };
        let old_stmt = item.replace(stmt, ctx);
        match old_stmt {
            // SAFETY: Dummy count is only decremented if node being removed from AST is a dummy
            traversable::Statement::Dummy => unsafe { ctx.decrement_dummy_count() },
            _ => old_stmt.remove_parent(ctx),
        }
        // SAFETY: We have removed `old_stmt` from the AST
        unsafe { Orphan::new(old_stmt) }
    }

    /// Extract statement at `index` of `Program` body, and replace with a dummy statement.
    pub fn take_body_stmt(
        &'a self,
        index: usize,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Statement<'a>> {
        // Unsafe code here is a workaround for `oxc_allocator::Vec` not implementing `IndexMut`.
        // `bumpalo::collections::Vec` implements `IndexMut`, so `oxc_allocator::Vec` could too.
        // See comment in `replace_body_stmt` above for why this is not sound at present.
        // If `Vec` was `IndexMut`, I think can do this instead:
        // ```
        // let stmt = std::mem::replace(
        //   &mut self.borrow_mut(ctx).body[index],
        //   GCell::new(traversable::Statement::Dummy)
        // );
        // let stmt = *stmt.borrow(ctx);
        // if !matches!(stmt, traversable::Statement::Dummy) {
        //   stmt.remove_parent(ctx);
        //   ctx.increment_dummy_count();
        // }
        // return unsafe { Orphan::new(old_stmt) };
        // ```
        // TODO: Make `Vec` impl `IndexMut` and replace this dodgy unsafe code with the above.
        assert!(index < self.borrow(ctx).body.len());
        // SAFETY: We checked `index` is in bounds.
        let item = unsafe { &*self.borrow(ctx).body.as_ptr().add(index) };
        let stmt = item.replace(traversable::Statement::Dummy, ctx);
        if !matches!(stmt, traversable::Statement::Dummy) {
            stmt.remove_parent(ctx);
            ctx.increment_dummy_count();
        }
        // SAFETY: We have removed `stmt` from the AST
        unsafe { Orphan::new(stmt) }
    }

    pub fn push_body_stmt(
        &'a self,
        stmt: Orphan<traversable::Statement<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        let stmt = stmt.inner();
        match stmt {
            traversable::Statement::Dummy => ctx.increment_dummy_count(),
            _ => stmt.set_parent(traversable::Parent::ProgramBody(self), ctx),
        }
        self.borrow_mut(ctx).body.push(GCell::new(stmt));
    }
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Statement<'a> {
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>) = 0,
}

// `Dummy` variant is a temporary placeholder indicating that a node has been removed from the AST.
// A valid AST should not contain any dummy nodes when transformation is complete, but it's required
// to have this placeholder so you can remove a node from the AST in order to insert it somewhere else
// instead. It's expected you'll replace the placeholder again with some other node.
//
// Failure to do so will lead to a panic at end of the transform process, to prevent the AST being
// used as a "standard" AST again, which would be UB as the `Dummy` variants don't exist in standard AST.

// NB: `Copy` because it's only 16 bytes
#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TraversableStatement<'a> {
    ExpressionStatement(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 0,
    Dummy = 128,
}

link_types!(Statement, TraversableStatement);

impl<'a> traversable::Statement<'a> {
    fn set_parent(&self, parent: traversable::Parent<'a>, ctx: &mut TraverseCtx) {
        match self {
            Self::ExpressionStatement(expr_stmt) => {
                expr_stmt.borrow_mut(ctx).parent = parent;
            }
            Self::Dummy => {}
        }
    }

    #[inline]
    fn remove_parent(&self, ctx: &mut TraverseCtx) {
        self.set_parent(traversable::Parent::None, ctx);
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct ExpressionStatement<'a> {
    pub parent: Parent<'a>,
    pub expression: Expression<'a>,
}

#[repr(C)]
pub struct TraversableExpressionStatement<'a> {
    parent: traversable::Parent<'a>,
    expression: traversable::Expression<'a>,
}

link_types!(ExpressionStatement, TraversableExpressionStatement);

impl<'a> traversable::ExpressionStatement<'a> {
    pub fn new_stmt_in(
        expression: Orphan<traversable::Expression<'a>>,
        alloc: &'a Allocator,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Statement<'a>> {
        let expression = expression.inner();
        // We can't allow a dummy to sneak into AST this way without incrementing dummy count
        assert!(!matches!(expression, traversable::Expression::Dummy));
        let stmt = alloc.galloc(Self {
            expression,
            parent: traversable::Parent::None,
        });
        expression.set_parent(
            traversable::Parent::ExpressionStatementExpression(stmt),
            ctx,
        );
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Statement::ExpressionStatement(stmt)) }
    }

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

impl<'a> GCell<traversable::ExpressionStatement<'a>> {
    /// Convenience method for getting `parent` from a ref.
    pub fn parent(&'a self, ctx: &TraverseCtx) -> traversable::Parent<'a> {
        self.borrow(ctx).parent
    }

    /// Convenience method for getting `expression` from a ref.
    #[inline]
    pub fn expression(&'a self, ctx: &TraverseCtx) -> traversable::Expression<'a> {
        self.borrow(ctx).expression
    }

    /// Replace value of `expression` field, and return previous value.
    pub fn replace_expression(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_expression = self.expression(ctx);
        match old_expression {
            // SAFETY: Dummy count is only decremented if node being removed from AST is a dummy
            traversable::Expression::Dummy => unsafe { ctx.decrement_dummy_count() },
            _ => old_expression.remove_parent(ctx),
        }
        let expr = expr.inner();
        match expr {
            traversable::Expression::Dummy => ctx.increment_dummy_count(),
            _ => expr.set_parent(
                traversable::Parent::ExpressionStatementExpression(self),
                ctx,
            ),
        }
        self.borrow_mut(ctx).expression = expr;
        // SAFETY: We have removed `old_expression` from the AST
        unsafe { Orphan::new(old_expression) }
    }

    /// Extract value of `expression` field, and replace with a dummy expression.
    pub fn take_expression(&'a self, ctx: &mut TraverseCtx) -> Orphan<traversable::Expression<'a>> {
        let expr = mem::replace(
            &mut self.borrow_mut(ctx).expression,
            traversable::Expression::Dummy,
        );
        if !matches!(expr, traversable::Expression::Dummy) {
            expr.remove_parent(ctx);
            ctx.increment_dummy_count();
        }
        // SAFETY: We have removed `expr` from the AST
        unsafe { Orphan::new(expr) }
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn expression_statement(
        &self,
        expression: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Statement<'a>> {
        traversable::ExpressionStatement::new_stmt_in(expression, self.allocator, ctx)
    }
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Expression<'a> {
    StringLiteral(Box<'a, StringLiteral<'a>>) = 0,
    Identifier(Box<'a, IdentifierReference<'a>>) = 1,
    BinaryExpression(Box<'a, BinaryExpression<'a>>) = 2,
    UnaryExpression(Box<'a, UnaryExpression<'a>>) = 3,
}

// NB: `Copy` because it's only 16 bytes
#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TraversableExpression<'a> {
    StringLiteral(SharedBox<'a, traversable::StringLiteral<'a>>) = 0,
    Identifier(SharedBox<'a, traversable::IdentifierReference<'a>>) = 1,
    BinaryExpression(SharedBox<'a, traversable::BinaryExpression<'a>>) = 2,
    UnaryExpression(SharedBox<'a, traversable::UnaryExpression<'a>>) = 3,
    Dummy = 128,
}

link_types!(Expression, TraversableExpression);

impl<'a> traversable::Expression<'a> {
    fn set_parent(&self, parent: traversable::Parent<'a>, ctx: &mut TraverseCtx) {
        match self {
            Self::StringLiteral(expr) => {
                expr.borrow_mut(ctx).parent = parent;
            }
            Self::Identifier(expr) => {
                expr.borrow_mut(ctx).parent = parent;
            }
            Self::BinaryExpression(expr) => {
                expr.borrow_mut(ctx).parent = parent;
            }
            Self::UnaryExpression(expr) => {
                expr.borrow_mut(ctx).parent = parent;
            }
            Self::Dummy => {}
        }
    }

    fn remove_parent(&self, ctx: &mut TraverseCtx) {
        self.set_parent(traversable::Parent::None, ctx);
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct IdentifierReference<'a> {
    pub parent: Parent<'a>,
    pub name: &'a str,
}

#[repr(C)]
pub struct TraversableIdentifierReference<'a> {
    parent: traversable::Parent<'a>,
    pub name: &'a str,
}

link_types!(IdentifierReference, TraversableIdentifierReference);

impl<'a> traversable::IdentifierReference<'a> {
    pub fn new_expr_in(name: &'a str, alloc: &'a Allocator) -> Orphan<traversable::Expression<'a>> {
        let expr = alloc.galloc(Self {
            name,
            parent: traversable::Parent::None,
        });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::Identifier(expr)) }
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

impl<'a> GCell<traversable::IdentifierReference<'a>> {
    /// Convenience method for getting `parent` from a ref.
    pub fn parent(&'a self, ctx: &TraverseCtx) -> traversable::Parent<'a> {
        self.borrow(ctx).parent
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn identifier_reference(&self, name: &'a str) -> Orphan<traversable::Expression<'a>> {
        traversable::IdentifierReference::new_expr_in(name, self.allocator)
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StringLiteral<'a> {
    pub parent: Parent<'a>,
    pub value: &'a str,
}

#[repr(C)]
pub struct TraversableStringLiteral<'a> {
    parent: traversable::Parent<'a>,
    pub value: &'a str,
}

link_types!(StringLiteral, TraversableStringLiteral);

impl<'a> traversable::StringLiteral<'a> {
    pub fn new_expr_in(
        value: &'a str,
        alloc: &'a Allocator,
    ) -> Orphan<traversable::Expression<'a>> {
        let expr = alloc.galloc(Self {
            value,
            parent: traversable::Parent::None,
        });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::StringLiteral(expr)) }
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

impl<'a> GCell<traversable::StringLiteral<'a>> {
    /// Convenience method for getting `parent` from a ref.
    pub fn parent(&'a self, ctx: &TraverseCtx) -> traversable::Parent<'a> {
        self.borrow(ctx).parent
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn string_literal(&self, value: &'a str) -> Orphan<traversable::Expression<'a>> {
        traversable::StringLiteral::new_expr_in(value, self.allocator)
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct BinaryExpression<'a> {
    pub parent: Parent<'a>,
    pub left: Expression<'a>,
    pub operator: BinaryOperator,
    pub right: Expression<'a>,
}

#[repr(C)]
pub struct TraversableBinaryExpression<'a> {
    parent: traversable::Parent<'a>,
    left: traversable::Expression<'a>,
    pub operator: BinaryOperator,
    right: traversable::Expression<'a>,
}

link_types!(BinaryExpression, TraversableBinaryExpression);

impl<'a> traversable::BinaryExpression<'a> {
    pub fn new_expr_in(
        left: Orphan<traversable::Expression<'a>>,
        operator: BinaryOperator,
        right: Orphan<traversable::Expression<'a>>,
        alloc: &'a Allocator,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        let left = left.inner();
        let right = right.inner();
        // We can't allow a dummy to sneak into AST this way without incrementing dummy count
        assert!(!matches!(left, traversable::Expression::Dummy));
        assert!(!matches!(right, traversable::Expression::Dummy));
        let expr = alloc.galloc(Self {
            left,
            operator,
            right,
            parent: traversable::Parent::None,
        });
        left.set_parent(traversable::Parent::BinaryExpressionLeft(expr), ctx);
        right.set_parent(traversable::Parent::BinaryExpressionRight(expr), ctx);
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::BinaryExpression(expr)) }
    }

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

impl<'a> GCell<traversable::BinaryExpression<'a>> {
    /// Convenience method for getting `parent` from a ref.
    pub fn parent(&'a self, ctx: &TraverseCtx) -> traversable::Parent<'a> {
        self.borrow(ctx).parent
    }

    /// Convenience method for getting `left` from a ref.
    #[inline]
    pub fn left(&'a self, ctx: &TraverseCtx) -> traversable::Expression<'a> {
        self.borrow(ctx).left
    }

    /// Convenience method for getting `right` from a ref.
    #[inline]
    pub fn right(&'a self, ctx: &TraverseCtx) -> traversable::Expression<'a> {
        self.borrow(ctx).right
    }

    /// Convenience method for getting `operator` from a ref.
    pub fn operator(&'a self, ctx: &TraverseCtx) -> BinaryOperator {
        self.borrow(ctx).operator
    }

    /// Convenience method for setting `operator` from a ref.
    pub fn set_operator(&'a self, operator: BinaryOperator, ctx: &mut TraverseCtx) {
        self.borrow_mut(ctx).operator = operator;
    }

    /// Replace value of `left` field, and return previous value.
    pub fn replace_left(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_left = self.left(ctx);
        match old_left {
            // SAFETY: Dummy count is only decremented if node being removed from AST is a dummy
            traversable::Expression::Dummy => unsafe { ctx.decrement_dummy_count() },
            _ => old_left.remove_parent(ctx),
        }
        let expr = expr.inner();
        match expr {
            traversable::Expression::Dummy => ctx.increment_dummy_count(),
            _ => expr.set_parent(traversable::Parent::BinaryExpressionLeft(self), ctx),
        }
        self.borrow_mut(ctx).left = expr;
        // SAFETY: We have removed `old_left` from the AST
        unsafe { Orphan::new(old_left) }
    }

    /// Replace value of `right` field, and return previous value.
    pub fn replace_right(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_right = self.right(ctx);
        match old_right {
            // SAFETY: Dummy count is only decremented if node being removed from AST is a dummy
            traversable::Expression::Dummy => unsafe { ctx.decrement_dummy_count() },
            _ => old_right.remove_parent(ctx),
        }
        let expr = expr.inner();
        match expr {
            traversable::Expression::Dummy => ctx.increment_dummy_count(),
            _ => expr.set_parent(traversable::Parent::BinaryExpressionRight(self), ctx),
        }
        self.borrow_mut(ctx).right = expr;
        // SAFETY: We have removed `old_right` from the AST
        unsafe { Orphan::new(old_right) }
    }

    /// Extract value of `left` field, and replace with a dummy expression.
    pub fn take_left(&'a self, ctx: &mut TraverseCtx) -> Orphan<traversable::Expression<'a>> {
        let expr = mem::replace(
            &mut self.borrow_mut(ctx).left,
            traversable::Expression::Dummy,
        );
        if !matches!(expr, traversable::Expression::Dummy) {
            expr.remove_parent(ctx);
            ctx.increment_dummy_count();
        }
        // SAFETY: We have removed `expr` from the AST
        unsafe { Orphan::new(expr) }
    }

    /// Extract value of `right` field, and replace with a dummy expression.
    pub fn take_right(&'a self, ctx: &mut TraverseCtx) -> Orphan<traversable::Expression<'a>> {
        let expr = mem::replace(
            &mut self.borrow_mut(ctx).right,
            traversable::Expression::Dummy,
        );
        if !matches!(expr, traversable::Expression::Dummy) {
            expr.remove_parent(ctx);
            ctx.increment_dummy_count();
        }
        // SAFETY: We have removed `expr` from the AST
        unsafe { Orphan::new(expr) }
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn binary_expression(
        &self,
        left: Orphan<traversable::Expression<'a>>,
        operator: BinaryOperator,
        right: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        traversable::BinaryExpression::new_expr_in(left, operator, right, self.allocator, ctx)
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
    pub parent: Parent<'a>,
    pub operator: UnaryOperator,
    pub argument: Expression<'a>,
}

#[repr(C)]
pub struct TraversableUnaryExpression<'a> {
    parent: traversable::Parent<'a>,
    pub operator: UnaryOperator,
    argument: traversable::Expression<'a>,
}

link_types!(UnaryExpression, TraversableUnaryExpression);

impl<'a> traversable::UnaryExpression<'a> {
    pub fn new_expr_in(
        operator: UnaryOperator,
        argument: Orphan<traversable::Expression<'a>>,
        alloc: &'a Allocator,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        let argument = argument.inner();
        // We can't allow a dummy to sneak into AST this way without incrementing dummy count
        assert!(!matches!(argument, traversable::Expression::Dummy));
        let expr = alloc.galloc(Self {
            operator,
            argument,
            parent: traversable::Parent::None,
        });
        argument.set_parent(traversable::Parent::UnaryExpressionArgument(expr), ctx);
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::UnaryExpression(expr)) }
    }

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

impl<'a> GCell<traversable::UnaryExpression<'a>> {
    /// Convenience method for getting `parent` from a ref.
    pub fn parent(&'a self, ctx: &TraverseCtx) -> traversable::Parent<'a> {
        self.borrow(ctx).parent
    }

    /// Convenience method for getting `argument` from a ref.
    #[inline]
    pub fn argument(&'a self, ctx: &TraverseCtx) -> traversable::Expression<'a> {
        self.borrow(ctx).argument
    }

    /// Convenience method for getting `operator` from a ref.
    pub fn operator(&'a self, ctx: &TraverseCtx) -> UnaryOperator {
        self.borrow(ctx).operator
    }

    /// Convenience method for setting `operator` from a ref.
    pub fn set_operator(&'a self, operator: UnaryOperator, ctx: &mut TraverseCtx) {
        self.borrow_mut(ctx).operator = operator;
    }

    /// Replace value of `argument` field, and return previous value.
    pub fn replace_argument(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_argument = self.argument(ctx);
        match old_argument {
            // SAFETY: Dummy count is only decremented if node being removed from AST is a dummy
            traversable::Expression::Dummy => unsafe { ctx.decrement_dummy_count() },
            _ => old_argument.remove_parent(ctx),
        }
        let expr = expr.inner();
        match expr {
            traversable::Expression::Dummy => ctx.increment_dummy_count(),
            _ => expr.set_parent(traversable::Parent::UnaryExpressionArgument(self), ctx),
        }
        self.borrow_mut(ctx).argument = expr;
        // SAFETY: We have removed `old_right` from the AST
        unsafe { Orphan::new(old_argument) }
    }

    /// Extract value of `argument` field, and replace with a dummy expression.
    pub fn take_argument(&'a self, ctx: &mut TraverseCtx) -> Orphan<traversable::Expression<'a>> {
        let expr = mem::replace(
            &mut self.borrow_mut(ctx).argument,
            traversable::Expression::Dummy,
        );
        if !matches!(expr, traversable::Expression::Dummy) {
            expr.remove_parent(ctx);
            ctx.increment_dummy_count();
        }
        // SAFETY: We have removed `expr` from the AST
        unsafe { Orphan::new(expr) }
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Orphan<traversable::Expression<'a>>,
        ctx: &mut TraverseCtx,
    ) -> Orphan<traversable::Expression<'a>> {
        traversable::UnaryExpression::new_expr_in(operator, argument, self.allocator, ctx)
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
/// Encodes both the type of the parent, and child's location in the parent.
/// i.e. variants for `BinaryExpressionLeft` and `BinaryExpressionRight`, not just `BinaryExpression`.
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
    ProgramBody(ParentPointer<Program<'a>>) = 1,
    ExpressionStatementExpression(ParentPointer<ExpressionStatement<'a>>) = 2,
    BinaryExpressionLeft(ParentPointer<BinaryExpression<'a>>) = 3,
    BinaryExpressionRight(ParentPointer<BinaryExpression<'a>>) = 4,
    UnaryExpressionArgument(ParentPointer<UnaryExpression<'a>>) = 5,
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
// NB: `Copy` because it's only 16 bytes
#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TraversableParent<'a> {
    None = 0,
    ProgramBody(SharedBox<'a, traversable::Program<'a>>) = 1,
    ExpressionStatementExpression(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 2,
    BinaryExpressionLeft(SharedBox<'a, traversable::BinaryExpression<'a>>) = 3,
    BinaryExpressionRight(SharedBox<'a, traversable::BinaryExpression<'a>>) = 4,
    UnaryExpressionArgument(SharedBox<'a, traversable::UnaryExpression<'a>>) = 5,
}

link_types!(Parent, TraversableParent);
