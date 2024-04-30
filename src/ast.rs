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

use crate::{
    cell::{GCell, SharedBox, SharedVec, Token},
    traverse::{Traverse, TraverseCtx},
};

/// Module namespace for traversable AST node types
#[allow(unused_imports)]
pub mod traversable {
    pub use super::traversable_binary_expression::BinaryExpression;
    pub use super::traversable_directive::Directive;
    pub use super::traversable_expression::Expression;
    pub use super::traversable_expression_statement::ExpressionStatement;
    pub use super::traversable_identifier_reference::IdentifierReference;
    pub use super::traversable_program::Program;
    pub use super::traversable_statement::Statement;
    pub use super::traversable_string_literal::StringLiteral;
    pub use super::traversable_unary_expression::UnaryExpression;
    pub use super::Ancestor;
    pub use super::Orphan;
    pub use super::TraversableAstBuilder as AstBuilder;
}

/// Macro to assert equivalence in size and alignment between standard and traversable types
macro_rules! link_types {
    ($standard:ty, $traversable:ty) => {
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

/// Trait for traversable struct fields which are `Copy`
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

/// Traversal entry point.
pub fn traverse<'a, T: Traverse<'a>>(
    traverser: &mut T,
    program: SharedBox<'a, traversable::Program<'a>>,
    ctx: &mut TraverseCtx<'a>,
    tk: &mut Token,
) {
    walk_program(traverser, program, ctx, tk);
}

// --------------------------------------------------------------------------------
// AST node types
// --------------------------------------------------------------------------------

#[derive(Debug)]
#[repr(C)]
pub struct Program<'a> {
    pub directives: Vec<'a, Directive<'a>>,
    pub body: Vec<'a, Statement<'a>>,
}

mod traversable_program {
    use super::*;

    #[repr(C)]
    pub struct Program<'a> {
        // `Directive` is a struct, so we use `SharedVec`
        directives: SharedVec<'a, traversable::Directive<'a>>,
        // `Statement` is an enum so we can just use `Vec` instead of `SharedVec`.
        // Note the different implementations for the methods relating to the 2 below.
        body: Vec<'a, traversable::Statement<'a>>,
    }

    link_types!(super::Program, Program);

    impl<'a> Program<'a> {
        /// Get length of directives.
        pub fn directives_len(&self) -> usize {
            self.directives.len()
        }

        /// Get directives item.
        /// # Panic
        /// Panics if `index` is out of bounds.
        pub fn directives_item(&self, index: usize) -> SharedBox<'a, traversable::Directive<'a>> {
            let item_ref = &self.directives[index];
            // Extend lifetime to 'a.
            // SAFETY: Item is stored in arena. Even if vec reallocates, the arena never reclaims or
            // overwrites memory, so ref will remain a valid pointer to a `Directive` for 'a.
            // TODO @overlookmotel: While I believe this is sound, probably could avoid the need for
            // unsafe code here by changing the lifetimes on `Traverse` trait methods and `walk_*` fns.
            unsafe { item_ref.extend_lifetime() }
        }

        /// Get directives item.
        /// Returns `None` if `index` is out of bounds.
        pub fn directives_item_get(
            &self,
            index: usize,
        ) -> Option<SharedBox<'a, traversable::Directive<'a>>> {
            let item_ref = self.directives.get(index);
            // Extend lifetime to 'a.
            // SAFETY: Item is stored in arena. Even if vec reallocates, the arena never reclaims or
            // overwrites memory, so ref will remain a valid pointer to a `Directive` for 'a.
            // TODO @overlookmotel: While I believe this is sound, probably could avoid the need for
            // unsafe code here by changing the lifetimes on `Traverse` trait methods and `walk_*` fns.
            item_ref.map(|item_ref| unsafe { item_ref.extend_lifetime() })
        }

        /// Get length of body.
        pub fn body_len(&self) -> usize {
            self.body.len()
        }

        /// Get body item.
        /// # Panic
        /// Panics if `index` is out of bounds.
        pub fn body_item(&self, index: usize) -> traversable::Statement<'a> {
            self.body[index]
        }

        /// Get body item.
        /// Returns `None` if `index` is out of bounds.
        pub fn body_item_get(&self, index: usize) -> Option<traversable::Statement<'a>> {
            self.body.get(index).copied()
        }
    }

    // TODO: We could probably abstract much of this into methods on a `SharedVec` type.
    // TODO: Implement more `Vec` methods.
    impl<'a> GCell<Program<'a>> {
        /// Convenience method for getting directives of body from a ref.
        pub fn directives_len(&self, tk: &Token) -> usize {
            self.borrow(tk).directives_len()
        }

        /// Convenience method for getting directives item from a ref.
        /// # Panic
        /// Panics if `index` is out of bounds.
        #[inline]
        pub fn directives_item(
            &self,
            index: usize,
            tk: &Token,
        ) -> SharedBox<'a, traversable::Directive<'a>> {
            self.borrow(tk).directives_item(index)
        }

        /// Convenience method for getting directives item from a ref.
        /// Returns `None` if `index` is out of bounds.
        #[inline]
        pub fn directives_item_get(
            &self,
            index: usize,
            tk: &Token,
        ) -> Option<SharedBox<'a, traversable::Directive<'a>>> {
            self.borrow(tk).directives_item_get(index)
        }

        /// Replace value at `index` of `Program` body, and return previous value.
        /// # Panic
        /// Panics if `index` is out of bounds.
        pub fn replace_directives_item(
            &self,
            index: usize,
            node: Orphan<traversable::Directive<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::Directive<'a>> {
            let item = self
                .borrow_mut(tk)
                .directives
                .get_mut(index)
                .expect("Out of bounds vec access");
            let old = std::mem::replace(item, GCell::new(node.inner()));
            // SAFETY: We have removed `old` from the AST
            unsafe { Orphan::new(old.into_inner()) }
        }

        /// Push an item to end of directives.
        pub fn push_directives(&self, item: Orphan<traversable::Directive<'a>>, tk: &mut Token) {
            let item = GCell::from(item.inner());
            self.borrow_mut(tk).directives.push(item);
        }

        /// Convenience method for getting length of body from a ref.
        pub fn body_len(&self, tk: &Token) -> usize {
            self.borrow(tk).body_len()
        }

        /// Convenience method for getting body item from a ref.
        /// # Panic
        /// Panics if `index` is out of bounds.
        #[inline]
        pub fn body_item(&self, index: usize, tk: &Token) -> traversable::Statement<'a> {
            self.borrow(tk).body_item(index)
        }

        /// Convenience method for getting body item from a ref.
        /// Returns `None` if `index` is out of bounds.
        #[inline]
        pub fn body_item_get(
            &self,
            index: usize,
            tk: &Token,
        ) -> Option<traversable::Statement<'a>> {
            self.borrow(tk).body_item_get(index)
        }

        /// Replace body item, and return previous value.
        /// # Panic
        /// Panics if `index` is out of bounds.
        pub fn replace_body_item(
            &self,
            index: usize,
            node: Orphan<traversable::Statement<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::Statement<'a>> {
            let item = self
                .borrow_mut(tk)
                .body
                .get_mut(index)
                .expect("Out of bounds vec access");
            let old = std::mem::replace(item, node.inner());
            // SAFETY: We have removed `old` from the AST
            unsafe { Orphan::new(old) }
        }

        /// Get swappable reference to a body item.
        #[inline]
        pub fn body_item_ref(&'a self, index: usize) -> ProgramBodyItem<'a> {
            ProgramBodyItem {
                program: self,
                index,
            }
        }

        /// Push an item to end of body.
        pub fn push_body(&self, item: Orphan<traversable::Statement<'a>>, tk: &mut Token) {
            self.borrow_mut(tk).body.push(item.inner());
        }
    }

    pub struct ProgramBodyItem<'a> {
        program: SharedBox<'a, Program<'a>>,
        index: usize,
    }

    impl<'a> TraversableField<'a, traversable::Statement<'a>> for ProgramBodyItem<'a> {
        #[inline]
        fn get(&self, tk: &Token) -> traversable::Statement<'a> {
            self.program.borrow(tk).body[self.index]
        }

        #[inline]
        fn set(&self, node: traversable::Statement<'a>, tk: &mut Token) {
            let item = self
                .program
                .borrow_mut(tk)
                .body
                .get_mut(self.index)
                .expect("Out of bounds vec access");
            *item = node;
        }
    }

    pub fn walk_program<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: SharedBox<'a, Program<'a>>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_program(node, ctx, tk);

        ctx.push_stack(Ancestor::ProgramDirectives(node));
        // Need to check bounds on each turn of the loop, as `walk_directive` (or a child of it)
        // could add more nodes to the `Vec`, or remove them
        let mut index = 0;
        loop {
            let item = node.directives_item_get(index, tk);
            let Some(item) = item else {
                break;
            };
            walk_directive(traverser, item, ctx, tk);
            index += 1;
        }

        ctx.replace_stack(Ancestor::ProgramBody(node));
        // Need to check bounds on each turn of the loop, as `walk_statement` (or a child of it)
        // could add more nodes to the `Vec`, or remove them
        let mut index = 0;
        loop {
            let item = node.body_item_get(index, tk);
            let Some(item) = item else {
                break;
            };
            walk_statement(traverser, item, ctx, tk);
            index += 1;
        }
        ctx.pop_stack();

        traverser.exit_program(node, ctx, tk);
    }
}
use traversable_program::walk_program;

#[derive(Debug)]
#[repr(C)]
pub struct Directive<'a> {
    pub expression: Box<'a, StringLiteral<'a>>,
}

mod traversable_directive {
    use super::*;

    #[repr(C)]
    pub struct Directive<'a> {
        expression: SharedBox<'a, traversable::StringLiteral<'a>>,
    }

    link_types!(super::Directive, Directive);

    // NB: These methods are different from `ExpressionStatement::expression` because here the content
    // of the box is a struct not an enum.
    impl<'a> Directive<'a> {
        pub fn new_in(
            expression: Orphan<traversable::StringLiteral<'a>>,
            alloc: &'a Allocator,
        ) -> Orphan<Directive<'a>> {
            let node = Self {
                expression: alloc.galloc(expression.inner()),
            };
            // SAFETY: Node is newly created so by definition is not yet attached to AST
            unsafe { Orphan::new(node) }
        }

        pub fn expression(&self) -> SharedBox<'a, traversable::StringLiteral<'a>> {
            self.expression
        }
    }

    impl<'a> GCell<Directive<'a>> {
        /// Convenience method for getting `expression` from a ref.
        #[inline]
        pub fn expression(&self, tk: &Token) -> SharedBox<'a, traversable::StringLiteral<'a>> {
            self.borrow(tk).expression
        }

        /// Replace value of `expression` field, and return previous value.
        pub fn replace_expression(
            &self,
            node: Orphan<traversable::StringLiteral<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::StringLiteral<'a>> {
            let old_ref = self.borrow(tk).expression;
            let old = old_ref.replace(node.inner(), tk);
            unsafe { Orphan::new(old) }
        }
    }

    impl<'a> TraversableAstBuilder<'a> {
        #[inline]
        pub fn directive(
            &self,
            expression: Orphan<traversable::StringLiteral<'a>>,
        ) -> Orphan<Directive<'a>> {
            Directive::new_in(expression, self.allocator)
        }
    }

    pub fn walk_directive<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: SharedBox<'a, Directive<'a>>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_directive(node, ctx, tk);

        ctx.push_stack(Ancestor::DirectiveExpression(node));
        traverser.visit_string_literal(node.borrow(tk).expression, ctx, tk);
        ctx.pop_stack();

        traverser.exit_directive(node, ctx, tk);
    }
}
use traversable_directive::walk_directive;

#[derive(Debug)]
#[repr(C, u8)]
pub enum Statement<'a> {
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>) = 0,
}

mod traversable_statement {
    use super::*;

    // NB: `Copy` because it's only 16 bytes
    #[derive(Clone, Copy)]
    #[repr(C, u8)]
    pub enum Statement<'a> {
        ExpressionStatement(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 0,
    }

    link_types!(super::Statement, Statement);

    pub fn walk_statement<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: Statement<'a>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_statement(node, ctx, tk);
        match node {
            traversable::Statement::ExpressionStatement(node) => {
                walk_expression_statement(traverser, node, ctx, tk);
            }
        }
        traverser.exit_statement(node, ctx, tk);
    }
}
use traversable_statement::walk_statement;

#[derive(Debug)]
#[repr(C)]
pub struct ExpressionStatement<'a> {
    pub expression: Expression<'a>,
}

mod traversable_expression_statement {
    use super::*;

    #[repr(C)]
    pub struct ExpressionStatement<'a> {
        expression: traversable::Expression<'a>,
    }

    link_types!(super::ExpressionStatement, ExpressionStatement);

    impl<'a> ExpressionStatement<'a> {
        pub fn new_statement_in(
            expression: Orphan<traversable::Expression<'a>>,
            alloc: &'a Allocator,
        ) -> Orphan<traversable::Statement<'a>> {
            let node = alloc.galloc(Self {
                expression: expression.inner(),
            });
            // SAFETY: Node is newly created so by definition is not yet attached to AST
            unsafe { Orphan::new(traversable::Statement::ExpressionStatement(node)) }
        }

        pub fn expression(&self) -> traversable::Expression<'a> {
            self.expression
        }
    }

    impl<'a> GCell<ExpressionStatement<'a>> {
        /// Convenience method for getting `expression` from a ref.
        #[inline]
        pub fn expression(&self, tk: &Token) -> traversable::Expression<'a> {
            self.borrow(tk).expression
        }

        /// Replace value of `expression` field, and return previous value.
        pub fn replace_expression(
            &self,
            node: Orphan<traversable::Expression<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::Expression<'a>> {
            let old = std::mem::replace(&mut self.borrow_mut(tk).expression, node.inner());
            // SAFETY: We have removed `old` from the AST
            unsafe { Orphan::new(old) }
        }

        #[inline]
        pub fn expression_ref(&'a self) -> ExpressionStatementExpression<'a> {
            ExpressionStatementExpression(self)
        }
    }

    pub struct ExpressionStatementExpression<'a>(SharedBox<'a, ExpressionStatement<'a>>);

    impl<'a> TraversableField<'a, traversable::Expression<'a>> for ExpressionStatementExpression<'a> {
        #[inline]
        fn get(&self, tk: &Token) -> traversable::Expression<'a> {
            self.0.borrow(tk).expression
        }

        #[inline]
        fn set(&self, node: traversable::Expression<'a>, tk: &mut Token) {
            self.0.borrow_mut(tk).expression = node;
        }
    }

    impl<'a> TraversableAstBuilder<'a> {
        #[inline]
        pub fn expression_statement(
            &self,
            expression: Orphan<traversable::Expression<'a>>,
        ) -> Orphan<traversable::Statement<'a>> {
            ExpressionStatement::new_statement_in(expression, self.allocator)
        }
    }

    pub fn walk_expression_statement<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: SharedBox<'a, ExpressionStatement<'a>>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_expression_statement(node, ctx, tk);

        ctx.push_stack(Ancestor::ExpressionStatementExpression(node));
        walk_expression(traverser, node.expression(tk), ctx, tk);
        ctx.pop_stack();

        traverser.exit_expression_statement(node, ctx, tk);
    }
}
use traversable_expression_statement::walk_expression_statement;

#[derive(Debug)]
#[repr(C, u8)]
pub enum Expression<'a> {
    StringLiteral(Box<'a, StringLiteral<'a>>) = 0,
    Identifier(Box<'a, IdentifierReference<'a>>) = 1,
    BinaryExpression(Box<'a, BinaryExpression<'a>>) = 2,
    UnaryExpression(Box<'a, UnaryExpression<'a>>) = 3,
}

mod traversable_expression {
    use super::*;

    // NB: `Copy` because it's only 16 bytes
    #[derive(Clone, Copy)]
    #[repr(C, u8)]
    pub enum Expression<'a> {
        StringLiteral(SharedBox<'a, traversable::StringLiteral<'a>>) = 0,
        Identifier(SharedBox<'a, traversable::IdentifierReference<'a>>) = 1,
        BinaryExpression(SharedBox<'a, traversable::BinaryExpression<'a>>) = 2,
        UnaryExpression(SharedBox<'a, traversable::UnaryExpression<'a>>) = 3,
    }

    link_types!(super::Expression, Expression);

    pub fn walk_expression<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_expression(node, ctx, tk);
        match node {
            Expression::Identifier(node) => {
                traverser.visit_identifier_reference(node, ctx, tk);
            }
            Expression::StringLiteral(node) => {
                traverser.visit_string_literal(node, ctx, tk);
            }
            Expression::BinaryExpression(node) => {
                walk_binary_expression(traverser, node, ctx, tk);
            }
            Expression::UnaryExpression(node) => {
                walk_unary_expression(traverser, node, ctx, tk);
            }
        }
        traverser.exit_expression(node, ctx, tk);
    }
}
use traversable_expression::walk_expression;

#[derive(Debug)]
#[repr(C)]
pub struct IdentifierReference<'a> {
    pub name: &'a str,
}

mod traversable_identifier_reference {
    use super::*;

    #[repr(C)]
    pub struct IdentifierReference<'a> {
        pub name: &'a str,
    }

    link_types!(super::IdentifierReference, IdentifierReference);

    impl<'a> IdentifierReference<'a> {
        pub fn new_expression_in(
            name: &'a str,
            alloc: &'a Allocator,
        ) -> Orphan<traversable::Expression<'a>> {
            let node = alloc.galloc(Self { name });
            // SAFETY: Node is newly created so by definition is not yet attached to AST
            unsafe { Orphan::new(traversable::Expression::Identifier(node)) }
        }
    }

    impl<'a> TraversableAstBuilder<'a> {
        #[inline]
        pub fn identifier_reference(&self, name: &'a str) -> Orphan<traversable::Expression<'a>> {
            IdentifierReference::new_expression_in(name, self.allocator)
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StringLiteral<'a> {
    pub value: &'a str,
}

mod traversable_string_literal {
    use super::*;

    #[repr(C)]
    pub struct StringLiteral<'a> {
        pub value: &'a str,
    }

    link_types!(super::StringLiteral, StringLiteral);

    impl<'a> StringLiteral<'a> {
        pub fn new_expression_in(
            value: &'a str,
            alloc: &'a Allocator,
        ) -> Orphan<traversable::Expression<'a>> {
            let node = alloc.galloc(Self { value });
            // SAFETY: Node is newly created so by definition is not yet attached to AST
            unsafe { Orphan::new(traversable::Expression::StringLiteral(node)) }
        }
    }

    impl<'a> TraversableAstBuilder<'a> {
        #[inline]
        pub fn string_literal(&self, value: &'a str) -> Orphan<traversable::Expression<'a>> {
            StringLiteral::new_expression_in(value, self.allocator)
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct BinaryExpression<'a> {
    pub left: Expression<'a>,
    pub operator: BinaryOperator,
    pub right: Expression<'a>,
}

mod traversable_binary_expression {
    use super::*;

    #[repr(C)]
    pub struct BinaryExpression<'a> {
        left: traversable::Expression<'a>,
        pub operator: BinaryOperator,
        right: traversable::Expression<'a>,
    }

    link_types!(super::BinaryExpression, BinaryExpression);

    impl<'a> BinaryExpression<'a> {
        pub fn new_expression_in(
            left: Orphan<traversable::Expression<'a>>,
            operator: BinaryOperator,
            right: Orphan<traversable::Expression<'a>>,
            alloc: &'a Allocator,
        ) -> Orphan<traversable::Expression<'a>> {
            let node = alloc.galloc(Self {
                left: left.inner(),
                operator,
                right: right.inner(),
            });
            // SAFETY: Node is newly created so by definition is not yet attached to AST
            unsafe { Orphan::new(traversable::Expression::BinaryExpression(node)) }
        }

        pub fn left(&self) -> traversable::Expression<'a> {
            self.left
        }

        pub fn right(&self) -> traversable::Expression<'a> {
            self.right
        }
    }

    impl<'a> GCell<BinaryExpression<'a>> {
        /// Convenience method for getting `left` from a ref.
        #[inline]
        pub fn left(&self, tk: &Token) -> traversable::Expression<'a> {
            self.borrow(tk).left
        }

        /// Convenience method for getting `right` from a ref.
        #[inline]
        pub fn right(&self, tk: &Token) -> traversable::Expression<'a> {
            self.borrow(tk).right
        }

        /// Convenience method for getting `operator` from a ref.
        pub fn operator(&self, tk: &Token) -> BinaryOperator {
            self.borrow(tk).operator
        }

        /// Convenience method for setting `operator` from a ref.
        pub fn set_operator(&self, value: BinaryOperator, tk: &mut Token) {
            self.borrow_mut(tk).operator = value;
        }

        /// Replace value of `left` field, and return previous value.
        pub fn replace_left(
            &self,
            node: Orphan<traversable::Expression<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::Expression<'a>> {
            let old = std::mem::replace(&mut self.borrow_mut(tk).left, node.inner());
            // SAFETY: We have removed `old` from the AST
            unsafe { Orphan::new(old) }
        }

        /// Replace value of `right` field, and return previous value.
        pub fn replace_right(
            &self,
            node: Orphan<traversable::Expression<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::Expression<'a>> {
            let old = std::mem::replace(&mut self.borrow_mut(tk).right, node.inner());
            // SAFETY: We have removed `old` from the AST
            unsafe { Orphan::new(old) }
        }

        #[inline]
        pub fn left_ref(&'a self) -> BinaryExpressionLeft<'a> {
            BinaryExpressionLeft(self)
        }

        #[inline]
        pub fn right_ref(&'a self) -> BinaryExpressionRight<'a> {
            BinaryExpressionRight(self)
        }
    }

    pub struct BinaryExpressionLeft<'a>(SharedBox<'a, BinaryExpression<'a>>);

    impl<'a> TraversableField<'a, traversable::Expression<'a>> for BinaryExpressionLeft<'a> {
        #[inline]
        fn get(&self, tk: &Token) -> traversable::Expression<'a> {
            self.0.borrow(tk).left
        }

        #[inline]
        fn set(&self, node: traversable::Expression<'a>, tk: &mut Token) {
            self.0.borrow_mut(tk).left = node;
        }
    }

    pub struct BinaryExpressionRight<'a>(SharedBox<'a, BinaryExpression<'a>>);

    impl<'a> TraversableField<'a, traversable::Expression<'a>> for BinaryExpressionRight<'a> {
        #[inline]
        fn get(&self, tk: &Token) -> traversable::Expression<'a> {
            self.0.borrow(tk).right
        }

        #[inline]
        fn set(&self, node: traversable::Expression<'a>, tk: &mut Token) {
            self.0.borrow_mut(tk).right = node;
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
            BinaryExpression::new_expression_in(left, operator, right, self.allocator)
        }
    }

    pub fn walk_binary_expression<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: SharedBox<'a, traversable::BinaryExpression<'a>>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_binary_expression(node, ctx, tk);

        ctx.push_stack(Ancestor::BinaryExpressionLeft(node));
        walk_expression(traverser, node.left(tk), ctx, tk);
        ctx.replace_stack(Ancestor::BinaryExpressionRight(node));
        walk_expression(traverser, node.right(tk), ctx, tk);
        ctx.pop_stack();

        traverser.exit_binary_expression(node, ctx, tk);
    }
}
use traversable_binary_expression::walk_binary_expression;

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

mod traversable_unary_expression {
    use super::*;

    #[repr(C)]
    pub struct UnaryExpression<'a> {
        pub operator: UnaryOperator,
        argument: traversable::Expression<'a>,
    }

    link_types!(super::UnaryExpression, UnaryExpression);

    impl<'a> UnaryExpression<'a> {
        pub fn new_expression_in(
            operator: UnaryOperator,
            argument: Orphan<traversable::Expression<'a>>,
            alloc: &'a Allocator,
        ) -> Orphan<traversable::Expression<'a>> {
            let node = alloc.galloc(Self {
                operator,
                argument: argument.inner(),
            });
            // SAFETY: Node is newly created so by definition is not yet attached to AST
            unsafe { Orphan::new(traversable::Expression::UnaryExpression(node)) }
        }

        pub fn argument(&self) -> traversable::Expression<'a> {
            self.argument
        }
    }

    impl<'a> GCell<UnaryExpression<'a>> {
        /// Convenience method for getting `argument` from a ref.
        #[inline]
        pub fn argument(&self, tk: &Token) -> traversable::Expression<'a> {
            self.borrow(tk).argument
        }

        /// Convenience method for getting `operator` from a ref.
        pub fn operator(&self, tk: &Token) -> UnaryOperator {
            self.borrow(tk).operator
        }

        /// Convenience method for setting `operator` from a ref.
        pub fn set_operator(&self, value: UnaryOperator, tk: &mut Token) {
            self.borrow_mut(tk).operator = value;
        }

        /// Replace value of `argument` field, and return previous value.
        pub fn replace_argument(
            &self,
            node: Orphan<traversable::Expression<'a>>,
            tk: &mut Token,
        ) -> Orphan<traversable::Expression<'a>> {
            let old = std::mem::replace(&mut self.borrow_mut(tk).argument, node.inner());
            // SAFETY: We have removed `old` from the AST
            unsafe { Orphan::new(old) }
        }

        #[inline]
        pub fn argument_ref(&'a self) -> UnaryExpressionArgument<'a> {
            UnaryExpressionArgument(self)
        }
    }

    pub struct UnaryExpressionArgument<'a>(SharedBox<'a, UnaryExpression<'a>>);

    impl<'a> TraversableField<'a, traversable::Expression<'a>> for UnaryExpressionArgument<'a> {
        #[inline]
        fn get(&self, tk: &Token) -> traversable::Expression<'a> {
            self.0.borrow(tk).argument
        }

        #[inline]
        fn set(&self, node: traversable::Expression<'a>, tk: &mut Token) {
            self.0.borrow_mut(tk).argument = node;
        }
    }

    impl<'a> TraversableAstBuilder<'a> {
        #[inline]
        pub fn unary_expression(
            &self,
            operator: UnaryOperator,
            argument: Orphan<traversable::Expression<'a>>,
        ) -> Orphan<traversable::Expression<'a>> {
            UnaryExpression::new_expression_in(operator, argument, self.allocator)
        }
    }

    pub fn walk_unary_expression<'a, T: Traverse<'a>>(
        traverser: &mut T,
        node: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &mut TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        traverser.enter_unary_expression(node, ctx, tk);

        ctx.push_stack(Ancestor::UnaryExpressionArgument(node));
        walk_expression(traverser, node.argument(tk), ctx, tk);
        ctx.pop_stack();

        traverser.exit_unary_expression(node, ctx, tk);
    }
}
use traversable_unary_expression::walk_unary_expression;

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

/// Ancestor type used in traversable AST.
///
/// Encodes both the type of the parent, and child's location in the parent.
/// i.e. variants for `BinaryExpressionLeft` and `BinaryExpressionRight`, not just `BinaryExpression`.
#[derive(Clone, Copy)]
pub enum Ancestor<'a> {
    ProgramDirectives(SharedBox<'a, traversable::Program<'a>>),
    ProgramBody(SharedBox<'a, traversable::Program<'a>>),
    DirectiveExpression(SharedBox<'a, traversable::Directive<'a>>),
    ExpressionStatementExpression(SharedBox<'a, traversable::ExpressionStatement<'a>>),
    BinaryExpressionLeft(SharedBox<'a, traversable::BinaryExpression<'a>>),
    BinaryExpressionRight(SharedBox<'a, traversable::BinaryExpression<'a>>),
    UnaryExpressionArgument(SharedBox<'a, traversable::UnaryExpression<'a>>),
}
