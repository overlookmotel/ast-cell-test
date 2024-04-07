#![allow(dead_code, clippy::enum_variant_names)]

//! This file defines 2 different versions of the AST:
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
//! # SAFETY
//! The size, alignment, and layout of all AST node types and their "traversable" counterparts
//! must be identical, so that transmuting `Statement` to `TraversableStatement` is sound.
//! All types must be `#[repr(C)]` to ensure predictable type layouts.
//! All enums must be `#[repr(C, u8)]` with explicit discriminants to ensure discriminants
//! match between the "standard" and "traversable" types.

// TODO: Create the "Traversable" types with a macro to ensure they cannot be out of sync,
// and apply `#[repr(C)]` (for structs) / `#[repr(C, u8)]` (for enums) programmatically,
// so can't get forgotten.

use oxc_allocator::{Box, Vec};

use crate::cell::{SharedBox, SharedVec};

/// Macro to assert equivalence in size and alignment between standard and traversable types
macro_rules! assert_size_align_match {
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

#[derive(Debug)]
#[repr(C)]
pub struct Program<'a> {
    pub body: Vec<'a, Statement<'a>>,
}

#[repr(C)]
pub struct TraversableProgram<'a, 't> {
    pub body: shared_vec!(TraversableStatement<'a, 't>),
}

assert_size_align_match!(Program, TraversableProgram);

#[derive(Debug)]
#[repr(C, u8)]
pub enum Statement<'a> {
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>) = 0,
}

#[derive(Clone)]
#[repr(C, u8)]
pub enum TraversableStatement<'a> {
    ExpressionStatement(SharedBox<'a, TraversableExpressionStatement<'a>>) = 0,
}

assert_size_align_match!(Statement, TraversableStatement);

#[derive(Clone, Copy, Debug)]
#[repr(C, u8)]
pub enum StatementParent<'a> {
    None = 0,
    Program(*const Program<'a>) = 1,
}

#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TraversableStatementParent<'a, 't> {
    None = 0,
    Program(shared_box!(TraversableProgram<'a, 't>)) = 1,
}

assert_size_align_match!(StatementParent, TraversableStatementParent);

#[derive(Debug)]
#[repr(C)]
pub struct ExpressionStatement<'a> {
    pub expression: Expression<'a>,
    pub parent: StatementParent<'a>,
}

#[derive(Clone)]
#[repr(C)]
pub struct TraversableExpressionStatement<'a> {
    pub expression: TraversableExpression<'a>,
    pub parent: TraversableStatementParent<'a>,
}

assert_size_align_match!(ExpressionStatement, TraversableExpressionStatement);

#[derive(Debug)]
#[repr(C, u8)]
pub enum Expression<'a> {
    StringLiteral(Box<'a, StringLiteral<'a>>) = 0,
    Identifier(Box<'a, IdentifierReference<'a>>) = 1,
    BinaryExpression(Box<'a, BinaryExpression<'a>>) = 2,
    UnaryExpression(Box<'a, UnaryExpression<'a>>) = 3,
}

#[derive(Clone)]
#[repr(C, u8)]
pub enum TraversableExpression<'a> {
    StringLiteral(SharedBox<'a, TraversableStringLiteral<'a>>) = 0,
    Identifier(SharedBox<'a, TraversableIdentifierReference<'a>>) = 1,
    BinaryExpression(SharedBox<'a, TraversableBinaryExpression<'a>>) = 2,
    UnaryExpression(SharedBox<'a, TraversableUnaryExpression<'a>>) = 3,
}

assert_size_align_match!(Expression, TraversableExpression);

#[derive(Clone, Copy, Debug)]
#[repr(C, u8)]
pub enum ExpressionParent<'a> {
    None = 0,
    ExpressionStatement(*const ExpressionStatement<'a>) = 1,
    BinaryExpressionLeft(*const BinaryExpression<'a>) = 2,
    BinaryExpressionRight(*const BinaryExpression<'a>) = 3,
    UnaryExpression(*const UnaryExpression<'a>) = 4,
}

#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TraversableExpressionParent<'a> {
    None = 0,
    ExpressionStatement(SharedBox<'a, TraversableExpressionStatement<'a>>) = 1,
    BinaryExpressionLeft(SharedBox<'a, TraversableBinaryExpression<'a>>) = 2,
    BinaryExpressionRight(SharedBox<'a, TraversableBinaryExpression<'a>>) = 3,
    UnaryExpression(SharedBox<'a, TraversableUnaryExpression<'a>>) = 4,
}

assert_size_align_match!(ExpressionParent, TraversableExpressionParent);

#[derive(Debug)]
#[repr(C)]
pub struct IdentifierReference<'a> {
    pub name: &'a str,
    pub parent: ExpressionParent<'a>,
}

#[derive(Clone)]
#[repr(C)]
pub struct TraversableIdentifierReference<'a> {
    pub name: &'a str,
    pub parent: TraversableExpressionParent<'a>,
}

assert_size_align_match!(IdentifierReference, TraversableIdentifierReference);

#[derive(Debug)]
#[repr(C)]
pub struct StringLiteral<'a> {
    pub value: &'a str,
    pub parent: ExpressionParent<'a>,
}

#[derive(Clone)]
#[repr(C)]
pub struct TraversableStringLiteral<'a> {
    pub value: &'a str,
    pub parent: TraversableExpressionParent<'a>,
}

assert_size_align_match!(StringLiteral, TraversableStringLiteral);

#[derive(Debug)]
#[repr(C)]
pub struct BinaryExpression<'a> {
    pub left: Expression<'a>,
    pub operator: BinaryOperator,
    pub right: Expression<'a>,
    pub parent: ExpressionParent<'a>,
}

#[derive(Clone)]
#[repr(C)]
pub struct TraversableBinaryExpression<'a> {
    pub left: TraversableExpression<'a>,
    pub operator: BinaryOperator,
    pub right: TraversableExpression<'a>,
    pub parent: TraversableExpressionParent<'a>,
}

assert_size_align_match!(BinaryExpression, TraversableBinaryExpression);

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
    pub parent: ExpressionParent<'a>,
}

#[derive(Clone)]
#[repr(C)]
pub struct TraversableUnaryExpression<'a> {
    pub operator: UnaryOperator,
    pub argument: TraversableExpression<'a>,
    pub parent: TraversableExpressionParent<'a>,
}

assert_size_align_match!(UnaryExpression, TraversableUnaryExpression);

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

pub mod traversable {
    pub type Program<'a> = super::TraversableProgram<'a>;
    pub type Statement<'a> = super::TraversableStatement<'a>;
    pub type ExpressionStatement<'a> = super::TraversableExpressionStatement<'a>;
    pub type Expression<'a> = super::TraversableExpression<'a>;
    pub type ExpressionParent<'a> = super::TraversableExpressionParent<'a>;
    pub type IdentifierReference<'a> = super::TraversableIdentifierReference<'a>;
    pub type StringLiteral<'a> = super::TraversableStringLiteral<'a>;
    pub type BinaryExpression<'a> = super::TraversableBinaryExpression<'a>;
    pub type UnaryExpression<'a> = super::TraversableUnaryExpression<'a>;
}
