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

use std::mem;

use oxc_allocator::{Box, Vec};

use crate::cell::{SharedBox, SharedVec, Token};

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

/// Trait to link "standard" AST types to their "traversable" counterparts.
/// e.g. `Expression::Traversable` = `TraverableExpression`
pub trait AsTraversable {
    type Traversable;
}

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
        pub body: SharedVec<'a, traversable::Statement<'a>>,
    }

    link_types!(Program, TraversableProgram);

    // TODO: Implement methods to mutate statements
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
        pub(super) expression: traversable::Expression<'a>,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(ExpressionStatement, TraversableExpressionStatement);

    impl<'a> traversable::ExpressionStatement<'a> {
        pub fn expression(&self) -> &traversable::Expression<'a> {
            &self.expression
        }

        pub fn set_expression(
            expr_stmt: SharedBox<'a, traversable::ExpressionStatement<'a>>,
            expr: traversable::Expression<'a>,
            tk: &mut Token,
        ) {
            expr.set_parent(traversable::Parent::ExpressionStatement(expr_stmt), tk);
            expr_stmt.borrow_mut(tk).expression = expr;
        }

        pub fn take_expression(
            expr_stmt: SharedBox<'a, traversable::ExpressionStatement<'a>>,
            tk: &mut Token,
        ) -> traversable::Expression<'a> {
            let expr = mem::replace(
                &mut expr_stmt.borrow_mut(tk).expression,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            expr
        }

        pub fn parent(&self) -> traversable::Parent<'a> {
            self.parent
        }

        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
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

    #[derive(Clone)]
    #[repr(C, u8)]
    pub enum TraversableExpression<'a> {
        Dummy = 0,
        StringLiteral(SharedBox<'a, traversable::StringLiteral<'a>>) = 1,
        Identifier(SharedBox<'a, traversable::IdentifierReference<'a>>) = 2,
        BinaryExpression(SharedBox<'a, traversable::BinaryExpression<'a>>) = 3,
        UnaryExpression(SharedBox<'a, traversable::UnaryExpression<'a>>) = 4,
    }

    link_types!(Expression, TraversableExpression);

    impl<'a> TraversableExpression<'a> {
        pub(super) fn set_parent(&self, parent: traversable::Parent<'a>, tk: &mut Token) {
            use TraversableExpression::*;
            match self {
                StringLiteral(expr) => {
                    let expr = expr.borrow_mut(tk);
                    expr.parent.assert_none();
                    expr.parent = parent;
                }
                Identifier(expr) => {
                    let expr = expr.borrow_mut(tk);
                    expr.parent.assert_none();
                    expr.parent = parent;
                }
                BinaryExpression(expr) => {
                    let expr = expr.borrow_mut(tk);
                    expr.parent.assert_none();
                    expr.parent = parent;
                }
                UnaryExpression(expr) => {
                    let expr = expr.borrow_mut(tk);
                    expr.parent.assert_none();
                    expr.parent = parent;
                }
                Dummy => unreachable!("Cannot set parent of a dummy Expression"),
            }
        }

        pub(super) fn remove_parent(&self, tk: &mut Token) {
            use TraversableExpression::*;
            match self {
                StringLiteral(expr) => {
                    expr.borrow_mut(tk).parent = traversable::Parent::None;
                }
                Identifier(expr) => {
                    expr.borrow_mut(tk).parent = traversable::Parent::None;
                }
                BinaryExpression(expr) => {
                    expr.borrow_mut(tk).parent = traversable::Parent::None;
                }
                UnaryExpression(expr) => {
                    expr.borrow_mut(tk).parent = traversable::Parent::None;
                }
                Dummy => unreachable!("Cannot set parent of a dummy Expression"),
            }
        }
    }
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
        pub(super) left: traversable::Expression<'a>,
        pub operator: BinaryOperator,
        pub(super) right: traversable::Expression<'a>,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(BinaryExpression, TraversableBinaryExpression);

    impl<'a> traversable::BinaryExpression<'a> {
        pub fn left(&self) -> &traversable::Expression<'a> {
            &self.left
        }

        pub fn right(&self) -> &traversable::Expression<'a> {
            &self.right
        }

        pub fn set_left(
            bin_expr: SharedBox<'a, traversable::BinaryExpression<'a>>,
            expr: traversable::Expression<'a>,
            tk: &mut Token,
        ) {
            expr.set_parent(traversable::Parent::BinaryExpressionLeft(bin_expr), tk);
            bin_expr.borrow_mut(tk).left = expr;
        }

        pub fn set_right(
            bin_expr: SharedBox<'a, traversable::BinaryExpression<'a>>,
            expr: traversable::Expression<'a>,
            tk: &mut Token,
        ) {
            expr.set_parent(traversable::Parent::BinaryExpressionRight(bin_expr), tk);
            bin_expr.borrow_mut(tk).right = expr;
        }

        pub fn take_left(
            bin_expr: SharedBox<'a, traversable::BinaryExpression<'a>>,
            tk: &mut Token,
        ) -> traversable::Expression<'a> {
            let expr = mem::replace(
                &mut bin_expr.borrow_mut(tk).left,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            expr
        }

        pub fn take_right(
            bin_expr: SharedBox<'a, traversable::BinaryExpression<'a>>,
            tk: &mut Token,
        ) -> traversable::Expression<'a> {
            let expr = mem::replace(
                &mut bin_expr.borrow_mut(tk).right,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            expr
        }

        pub fn parent(self) -> traversable::Parent<'a> {
            self.parent
        }

        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
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

    #[derive(Clone)]
    #[repr(C)]
    pub struct TraversableUnaryExpression<'a> {
        pub operator: UnaryOperator,
        pub(super) argument: traversable::Expression<'a>,
        pub(super) parent: traversable::Parent<'a>,
    }

    link_types!(UnaryExpression, TraversableUnaryExpression);

    impl<'a> traversable::UnaryExpression<'a> {
        pub fn argument(&self) -> &traversable::Expression<'a> {
            &self.argument
        }

        pub fn set_argument(
            unary_expr: SharedBox<'a, traversable::UnaryExpression<'a>>,
            expr: traversable::Expression<'a>,
            tk: &mut Token,
        ) {
            expr.set_parent(traversable::Parent::UnaryExpression(unary_expr), tk);
            unary_expr.borrow_mut(tk).argument = expr;
        }

        pub fn take_argument(
            unary_expr: SharedBox<'a, traversable::UnaryExpression<'a>>,
            tk: &mut Token,
        ) -> traversable::Expression<'a> {
            let expr = mem::replace(
                &mut unary_expr.borrow_mut(tk).argument,
                traversable::Expression::Dummy,
            );
            expr.remove_parent(tk);
            expr
        }

        pub fn parent(&self) -> traversable::Parent<'a> {
            self.parent
        }

        pub unsafe fn set_parent(&mut self, parent: traversable::Parent<'a>) {
            self.parent = parent;
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
/// Type is opaque to external consumer. Pointer it contains cannot be read or written.
/// Purpose is to be the substitute for `&GCell` which this is transmuted to in the traversable AST.
/// In the traversable AST, it is possible (and necessary) to alter parent.
#[derive(Debug)]
#[repr(transparent)]
pub struct ParentPointer<T>(*const T);

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

    impl<'a> TraversableParent<'a> {
        pub(super) fn is_none(&self) -> bool {
            matches!(self, Self::None)
        }

        pub(super) fn assert_none(&self) {
            assert!(self.is_none());
        }
    }
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
