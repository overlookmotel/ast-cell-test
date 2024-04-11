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

        impl<'a> AsTraversable for $standard<'a> {
            type Traversable = $traversable<'a>;
        }
    };
}

pub trait AsTraversable {
    type Traversable;
}

#[derive(Debug)]
#[repr(C)]
pub struct Program<'a> {
    pub body: Vec<'a, Statement<'a>>,
}

mod traversable_program {
    use super::*;

    #[repr(C)]
    pub struct TraversableProgram<'a> {
        pub body: SharedVec<'a, traversable::Statement<'a>>,
    }

    link_types!(Program, TraversableProgram);
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Statement<'a> {
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>) = 0,
}

mod traversable_statement {
    use super::*;

    #[derive(Clone)]
    #[repr(C, u8)]
    pub enum TraversableStatement<'a> {
        ExpressionStatement(SharedBox<'a, traversable::ExpressionStatement<'a>>) = 0,
    }

    link_types!(Statement, TraversableStatement);
}

#[derive(Debug)]
#[repr(C)]
pub struct ExpressionStatement<'a> {
    pub expression: Expression<'a>,
    pub parent: Parent<'a>,
}

mod traversable_expression_statement {
    use super::*;

    #[derive(Clone)]
    #[repr(C)]
    pub struct TraversableExpressionStatement<'a> {
        pub expression: traversable::Expression<'a>,
        pub parent: traversable::Parent<'a>,
    }

    link_types!(ExpressionStatement, TraversableExpressionStatement);
}

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

    #[derive(Clone)]
    #[repr(C, u8)]
    pub enum TraversableExpression<'a> {
        StringLiteral(SharedBox<'a, traversable::StringLiteral<'a>>) = 0,
        Identifier(SharedBox<'a, traversable::IdentifierReference<'a>>) = 1,
        BinaryExpression(SharedBox<'a, traversable::BinaryExpression<'a>>) = 2,
        UnaryExpression(SharedBox<'a, traversable::UnaryExpression<'a>>) = 3,
    }

    link_types!(Expression, TraversableExpression);
}

#[derive(Debug)]
#[repr(C)]
pub struct IdentifierReference<'a> {
    pub name: &'a str,
    pub parent: Parent<'a>,
}

mod traversable_identifier_reference {
    use super::*;

    #[derive(Clone)]
    #[repr(C)]
    pub struct TraversableIdentifierReference<'a> {
        pub name: &'a str,
        pub parent: traversable::Parent<'a>,
    }

    link_types!(IdentifierReference, TraversableIdentifierReference);
}

#[derive(Debug)]
#[repr(C)]
pub struct StringLiteral<'a> {
    pub value: &'a str,
    pub parent: Parent<'a>,
}

mod traversable_string_literal {
    use super::*;

    #[derive(Clone)]
    #[repr(C)]
    pub struct TraversableStringLiteral<'a> {
        pub value: &'a str,
        pub parent: traversable::Parent<'a>,
    }

    link_types!(StringLiteral, TraversableStringLiteral);
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

    #[derive(Clone)]
    #[repr(C)]
    pub struct TraversableBinaryExpression<'a> {
        pub left: traversable::Expression<'a>,
        pub operator: BinaryOperator,
        pub right: traversable::Expression<'a>,
        pub parent: traversable::Parent<'a>,
    }

    link_types!(BinaryExpression, TraversableBinaryExpression);
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

    #[derive(Clone)]
    #[repr(C)]
    pub struct TraversableUnaryExpression<'a> {
        pub operator: UnaryOperator,
        pub argument: traversable::Expression<'a>,
        pub parent: traversable::Parent<'a>,
    }

    link_types!(UnaryExpression, TraversableUnaryExpression);
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

#[derive(Clone, Copy, Debug)]
#[repr(C, u8)]
pub enum Parent<'a> {
    None = 0,
    Program(*const Program<'a>) = 1,
    ExpressionStatement(*const ExpressionStatement<'a>) = 2,
    BinaryExpressionLeft(*const BinaryExpression<'a>) = 3,
    BinaryExpressionRight(*const BinaryExpression<'a>) = 4,
    UnaryExpression(*const UnaryExpression<'a>) = 5,
}

mod traversable_parent {
    use super::*;

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
}

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
}
