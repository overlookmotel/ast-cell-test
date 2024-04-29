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
//! All `replace_*` methods only accept an `Orphan<T>`.
//! This prevents a node from being attached to the AST in more than 1 place, by insisting it must be
//! removed from it's current position in AST first.
//!
//! To maintain these invariants, it is essential that access to fields containing nodes are not
//! public outside this file. Alteration of these fields must only be allowed via methods
//! (e.g. `replace_*`, `swap_with`), which enforce the no-duplicates rule.
//!
//! There must be **no** public API to obtain an owned struct AST node, otherwise it would be possible
//! to circumvent the invariant via `borrow_mut`.
//!
//! e.g.:
//! ```
//! // This is not possible
//! let unary_expr_mut = unary_expr_ref.borrow_mut(tk);
//! *unary_expr_mut = get_owned_unary_expr_somehow();
//! // This is also not possible
//! unary_expr_ref.replace(get_owned_unary_expr_somehow(), tk);
//! ```
//!
//! This implies that struct AST node types must **not** be `Clone`.
//!
//! Enum AST node types can be `Copy` and `Clone`. Each of their variants contains a `SharedBox<T>`.
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
// so can't get forgotten. Generate accessor methods (`replace_*` etc) with a macro/codegen too.

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
// NB: *Cannot* mut borrow parent at same time, as cycles in AST are possible, so e.g. `left`
// and parent could point to the same node.

use oxc_allocator::{Allocator, Box, Vec};

use crate::cell::{GCell, SharedBox, SharedVec, Token};

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

    #[inline]
    pub fn alloc<T>(&self, node: T) -> SharedBox<'a, T> {
        self.allocator.galloc(node)
    }
}

/// Trait for traversable struct fields
pub trait TraversableField<'a, T>: Sized {
    fn get(&self, tk: &Token) -> T;
    fn set(&self, left: T, tk: &mut Token);

    fn swap_with<O: TraversableField<'a, T>>(self, other: O, tk: &mut Token) {
        let self_enum = self.get(tk);
        let other_enum = other.get(tk);
        self.set(other_enum, tk);
        other.set(self_enum, tk);
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

    pub fn body_stmt(&self, index: usize, tk: &Token) -> traversable::Statement<'a> {
        *self.body[index].borrow(tk)
    }
}

// TODO: We could probably abstract much of this into methods on a `SharedVec` type.
// TODO: Implement more `Vec` methods.
impl<'a> GCell<traversable::Program<'a>> {
    /// Convenience method for getting `body.len()` from a ref.
    pub fn body_len(&'a self, tk: &Token) -> usize {
        self.borrow(tk).body.len()
    }

    /// Convenience method for getting a body statement from a ref.
    #[inline]
    pub fn body_stmt(&'a self, index: usize, tk: &Token) -> traversable::Statement<'a> {
        *self.borrow(tk).body[index].borrow(tk)
    }

    /// Replace statement at `index` of `Program` body, and return previous value.
    pub fn replace_body_stmt(
        &'a self,
        index: usize,
        stmt: Orphan<traversable::Statement<'a>>,
        tk: &mut Token,
    ) -> Orphan<traversable::Statement<'a>> {
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
        // let old_stmt = std::mem::replace(&mut self.borrow_mut(tk).body[index], GCell::new(stmt.inner()))
        // let old_stmt = *old_stmt.borrow(tk);
        // return unsafe { Orphan::new(old_stmt) };
        // ```
        // i.e. we replace the cell itself, rather than the *contents* of the cell.
        // TODO: Make `Vec` impl `IndexMut` and replace this dodgy unsafe code with the above.
        assert!(index < self.borrow(tk).body.len());
        // SAFETY: We checked `index` is in bounds.
        let item = unsafe { &*self.borrow(tk).body.as_ptr().add(index) };
        let old_stmt = item.replace(stmt.inner(), tk);
        // SAFETY: We have removed `old_stmt` from the AST
        unsafe { Orphan::new(old_stmt) }
    }

    #[inline]
    pub fn body_stmt_ref(&'a self, index: usize) -> TraversableProgramBodyStmt<'a> {
        TraversableProgramBodyStmt {
            program: self,
            index,
        }
    }

    pub fn push_body_stmt(&'a self, stmt: Orphan<traversable::Statement<'a>>, tk: &mut Token) {
        self.borrow_mut(tk).body.push(GCell::new(stmt.inner()));
    }
}

pub struct TraversableProgramBodyStmt<'a> {
    program: SharedBox<'a, TraversableProgram<'a>>,
    index: usize,
}

impl<'a> TraversableField<'a, traversable::Statement<'a>> for TraversableProgramBodyStmt<'a> {
    #[inline]
    fn get(&self, tk: &Token) -> traversable::Statement<'a> {
        assert!(self.index < self.program.borrow(tk).body.len());
        let item = unsafe { &*self.program.borrow(tk).body.as_ptr().add(self.index) };
        *item.borrow(tk)
    }

    #[inline]
    fn set(&self, stmt: traversable::Statement<'a>, tk: &mut Token) {
        assert!(self.index < self.program.borrow(tk).body.len());
        let item = unsafe { &*self.program.borrow(tk).body.as_ptr().add(self.index) };
        *item.borrow_mut(tk) = stmt;
    }
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Statement<'a> {
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>) = 0,
}

// NB: `Copy` because it's only 16 bytes
#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TraversableStatement<'a> {
    ExpressionStatement(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 0,
}

link_types!(Statement, TraversableStatement);

#[derive(Debug)]
#[repr(C)]
pub struct ExpressionStatement<'a> {
    pub expression: Expression<'a>,
}

#[repr(C)]
pub struct TraversableExpressionStatement<'a> {
    expression: traversable::Expression<'a>,
}

link_types!(ExpressionStatement, TraversableExpressionStatement);

impl<'a> traversable::ExpressionStatement<'a> {
    pub fn new_stmt_in(
        expression: Orphan<traversable::Expression<'a>>,
        alloc: &'a Allocator,
    ) -> Orphan<traversable::Statement<'a>> {
        let stmt = alloc.galloc(Self {
            expression: expression.inner(),
        });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Statement::ExpressionStatement(stmt)) }
    }

    pub fn expression(&self) -> traversable::Expression<'a> {
        self.expression
    }
}

impl<'a> GCell<traversable::ExpressionStatement<'a>> {
    /// Convenience method for getting `expression` from a ref.
    #[inline]
    pub fn expression(&'a self, tk: &Token) -> traversable::Expression<'a> {
        self.borrow(tk).expression
    }

    /// Replace value of `expression` field, and return previous value.
    pub fn replace_expression(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        tk: &mut Token,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_expression = self.expression(tk);
        self.borrow_mut(tk).expression = expr.inner();
        // SAFETY: We have removed `old_expression` from the AST
        unsafe { Orphan::new(old_expression) }
    }

    #[inline]
    pub fn expression_ref(&'a self) -> TraversableExpressionStatementExpression<'a> {
        TraversableExpressionStatementExpression(self)
    }
}

pub struct TraversableExpressionStatementExpression<'a>(
    SharedBox<'a, TraversableExpressionStatement<'a>>,
);

impl<'a> TraversableField<'a, traversable::Expression<'a>>
    for TraversableExpressionStatementExpression<'a>
{
    #[inline]
    fn get(&self, tk: &Token) -> traversable::Expression<'a> {
        self.0.borrow(tk).expression
    }

    #[inline]
    fn set(&self, expression: traversable::Expression<'a>, tk: &mut Token) {
        self.0.borrow_mut(tk).expression = expression;
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn expression_statement(
        &self,
        expression: Orphan<traversable::Expression<'a>>,
    ) -> Orphan<traversable::Statement<'a>> {
        traversable::ExpressionStatement::new_stmt_in(expression, self.allocator)
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
}

link_types!(Expression, TraversableExpression);

#[derive(Debug)]
#[repr(C)]
pub struct IdentifierReference<'a> {
    pub name: &'a str,
}

#[repr(C)]
pub struct TraversableIdentifierReference<'a> {
    pub name: &'a str,
}

link_types!(IdentifierReference, TraversableIdentifierReference);

impl<'a> traversable::IdentifierReference<'a> {
    pub fn new_expr_in(name: &'a str, alloc: &'a Allocator) -> Orphan<traversable::Expression<'a>> {
        let expr = alloc.galloc(Self { name });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::Identifier(expr)) }
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
    pub value: &'a str,
}

#[repr(C)]
pub struct TraversableStringLiteral<'a> {
    pub value: &'a str,
}

link_types!(StringLiteral, TraversableStringLiteral);

impl<'a> traversable::StringLiteral<'a> {
    pub fn new_expr_in(
        value: &'a str,
        alloc: &'a Allocator,
    ) -> Orphan<traversable::Expression<'a>> {
        let expr = alloc.galloc(Self { value });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::StringLiteral(expr)) }
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
    pub left: Expression<'a>,
    pub operator: BinaryOperator,
    pub right: Expression<'a>,
}

#[repr(C)]
pub struct TraversableBinaryExpression<'a> {
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
    ) -> Orphan<traversable::Expression<'a>> {
        let expr = alloc.galloc(Self {
            left: left.inner(),
            operator,
            right: right.inner(),
        });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::BinaryExpression(expr)) }
    }

    pub fn left(&self) -> traversable::Expression<'a> {
        self.left
    }

    pub fn right(&self) -> traversable::Expression<'a> {
        self.right
    }
}

impl<'a> GCell<traversable::BinaryExpression<'a>> {
    /// Convenience method for getting `left` from a ref.
    #[inline]
    pub fn left(&'a self, tk: &Token) -> traversable::Expression<'a> {
        self.borrow(tk).left
    }

    /// Convenience method for getting `right` from a ref.
    #[inline]
    pub fn right(&'a self, tk: &Token) -> traversable::Expression<'a> {
        self.borrow(tk).right
    }

    /// Convenience method for getting `operator` from a ref.
    pub fn operator(&'a self, tk: &Token) -> BinaryOperator {
        self.borrow(tk).operator
    }

    /// Convenience method for setting `operator` from a ref.
    pub fn set_operator(&'a self, operator: BinaryOperator, tk: &mut Token) {
        self.borrow_mut(tk).operator = operator;
    }

    /// Replace value of `left` field, and return previous value.
    pub fn replace_left(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        tk: &mut Token,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_left = self.left(tk);
        self.borrow_mut(tk).left = expr.inner();
        // SAFETY: We have removed `old_left` from the AST
        unsafe { Orphan::new(old_left) }
    }

    /// Replace value of `right` field, and return previous value.
    pub fn replace_right(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        tk: &mut Token,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_right = self.right(tk);
        self.borrow_mut(tk).right = expr.inner();
        // SAFETY: We have removed `old_right` from the AST
        unsafe { Orphan::new(old_right) }
    }

    #[inline]
    pub fn left_ref(&'a self) -> TraversableBinaryExpressionLeft<'a> {
        TraversableBinaryExpressionLeft(self)
    }

    #[inline]
    pub fn right_ref(&'a self) -> TraversableBinaryExpressionRight<'a> {
        TraversableBinaryExpressionRight(self)
    }
}

pub struct TraversableBinaryExpressionLeft<'a>(SharedBox<'a, TraversableBinaryExpression<'a>>);

impl<'a> TraversableField<'a, traversable::Expression<'a>> for TraversableBinaryExpressionLeft<'a> {
    #[inline]
    fn get(&self, tk: &Token) -> traversable::Expression<'a> {
        self.0.borrow(tk).left
    }

    #[inline]
    fn set(&self, left: traversable::Expression<'a>, tk: &mut Token) {
        self.0.borrow_mut(tk).left = left;
    }
}

pub struct TraversableBinaryExpressionRight<'a>(SharedBox<'a, TraversableBinaryExpression<'a>>);

impl<'a> TraversableField<'a, traversable::Expression<'a>>
    for TraversableBinaryExpressionRight<'a>
{
    #[inline]
    fn get(&self, tk: &Token) -> traversable::Expression<'a> {
        self.0.borrow(tk).right
    }

    #[inline]
    fn set(&self, right: traversable::Expression<'a>, tk: &mut Token) {
        self.0.borrow_mut(tk).right = right;
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn binary_expression(
        &self,
        left: Orphan<traversable::Expression<'a>>,
        operator: BinaryOperator,
        right: Orphan<traversable::Expression<'a>>,
    ) -> Orphan<traversable::Expression<'a>> {
        traversable::BinaryExpression::new_expr_in(left, operator, right, self.allocator)
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
}

#[repr(C)]
pub struct TraversableUnaryExpression<'a> {
    pub operator: UnaryOperator,
    argument: traversable::Expression<'a>,
}

link_types!(UnaryExpression, TraversableUnaryExpression);

impl<'a> traversable::UnaryExpression<'a> {
    pub fn new_expr_in(
        operator: UnaryOperator,
        argument: Orphan<traversable::Expression<'a>>,
        alloc: &'a Allocator,
    ) -> Orphan<traversable::Expression<'a>> {
        let expr = alloc.galloc(Self {
            operator,
            argument: argument.inner(),
        });
        // SAFETY: Node is newly created so by definition is not yet attached to AST
        unsafe { Orphan::new(traversable::Expression::UnaryExpression(expr)) }
    }

    pub fn argument(&self) -> traversable::Expression<'a> {
        self.argument
    }
}

impl<'a> GCell<traversable::UnaryExpression<'a>> {
    /// Convenience method for getting `argument` from a ref.
    #[inline]
    pub fn argument(&'a self, tk: &Token) -> traversable::Expression<'a> {
        self.borrow(tk).argument
    }

    /// Convenience method for getting `operator` from a ref.
    pub fn operator(&'a self, tk: &Token) -> UnaryOperator {
        self.borrow(tk).operator
    }

    /// Convenience method for setting `operator` from a ref.
    pub fn set_operator(&'a self, operator: UnaryOperator, tk: &mut Token) {
        self.borrow_mut(tk).operator = operator;
    }

    /// Replace value of `argument` field, and return previous value.
    pub fn replace_argument(
        &'a self,
        expr: Orphan<traversable::Expression<'a>>,
        tk: &mut Token,
    ) -> Orphan<traversable::Expression<'a>> {
        let old_argument = self.argument(tk);
        self.borrow_mut(tk).argument = expr.inner();
        // SAFETY: We have removed `old_right` from the AST
        unsafe { Orphan::new(old_argument) }
    }

    #[inline]
    pub fn argument_ref(&'a self) -> TraversableUnaryExpressionArgument<'a> {
        TraversableUnaryExpressionArgument(self)
    }
}

pub struct TraversableUnaryExpressionArgument<'a>(SharedBox<'a, TraversableUnaryExpression<'a>>);

impl<'a> TraversableField<'a, traversable::Expression<'a>>
    for TraversableUnaryExpressionArgument<'a>
{
    #[inline]
    fn get(&self, tk: &Token) -> traversable::Expression<'a> {
        self.0.borrow(tk).argument
    }

    #[inline]
    fn set(&self, argument: traversable::Expression<'a>, tk: &mut Token) {
        self.0.borrow_mut(tk).argument = argument;
    }
}

impl<'a> TraversableAstBuilder<'a> {
    #[inline]
    pub fn unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Orphan<traversable::Expression<'a>>,
    ) -> Orphan<traversable::Expression<'a>> {
        traversable::UnaryExpression::new_expr_in(operator, argument, self.allocator)
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

/// Parent type used in traversable AST.
///
/// Encodes both the type of the parent, and child's location in the parent.
/// i.e. variants for `BinaryExpressionLeft` and `BinaryExpressionRight`, not just `BinaryExpression`.
#[derive(Clone, Copy)]
pub enum TraversableParent<'a> {
    ProgramBody(SharedBox<'a, traversable::Program<'a>>),
    ExpressionStatementExpression(SharedBox<'a, traversable::ExpressionStatement<'a>>),
    BinaryExpressionLeft(SharedBox<'a, traversable::BinaryExpression<'a>>),
    BinaryExpressionRight(SharedBox<'a, traversable::BinaryExpression<'a>>),
    UnaryExpressionArgument(SharedBox<'a, traversable::UnaryExpression<'a>>),
}
