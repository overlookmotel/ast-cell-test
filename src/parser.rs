use oxc_allocator::{Allocator, Box, Vec};

use crate::ast::{
    BinaryExpression, BinaryOperator, Expression, ExpressionStatement, IdentifierReference, Parent,
    Program, Statement, StringLiteral, UnaryExpression, UnaryOperator,
};

/// Create AST for `typeof foo === 'object'`.
/// Hard-coded here, but these are the steps actual parser would take to create the AST
/// with "back-links" to parents on each node.
pub fn parse(alloc: &Allocator) -> &mut Program {
    // `foo`
    let id = Box(alloc.alloc(IdentifierReference {
        name: "foo",
        parent: Parent::None,
    }));

    // `typeof foo`
    let unary_expr = Box(alloc.alloc(UnaryExpression {
        operator: UnaryOperator::Typeof,
        argument: Expression::Identifier(id),
        parent: Parent::None,
    }));

    // `'object'`
    let str_lit = Box(alloc.alloc(StringLiteral {
        value: "object",
        parent: Parent::None,
    }));

    // `typeof foo === 'object'` (as expression)
    let binary_expr = Box(alloc.alloc(BinaryExpression {
        operator: BinaryOperator::StrictEquality,
        left: Expression::UnaryExpression(unary_expr),
        right: Expression::StringLiteral(str_lit),
        parent: Parent::None,
    }));

    // `typeof foo === 'object'` (as expression statement)
    let expr_stmt = Box(alloc.alloc(ExpressionStatement {
        expression: Expression::BinaryExpression(binary_expr),
        parent: Parent::None,
    }));

    // `typeof foo === 'object'` (as statement)
    let stmt = Statement::ExpressionStatement(expr_stmt);

    // `typeof foo === 'object'` (as program)
    let mut body = Vec::new_in(alloc);
    body.push(stmt);
    let program = alloc.alloc(Program { body });

    program
}
