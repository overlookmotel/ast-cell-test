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
    let mut unary_expr = Box(alloc.alloc(UnaryExpression {
        operator: UnaryOperator::Typeof,
        argument: Expression::Identifier(id),
        parent: Parent::None,
    }));

    let unary_expr_ptr = &*unary_expr as *const _;
    if let Expression::Identifier(id) = &mut unary_expr.argument {
        id.parent = Parent::UnaryExpression(unary_expr_ptr);
    }

    // `'object'`
    let str_lit = Box(alloc.alloc(StringLiteral {
        value: "object",
        parent: Parent::None,
    }));

    // `typeof foo === 'object'` (as expression)
    let mut binary_expr = Box(alloc.alloc(BinaryExpression {
        operator: BinaryOperator::StrictEquality,
        left: Expression::UnaryExpression(unary_expr),
        right: Expression::StringLiteral(str_lit),
        parent: Parent::None,
    }));

    let binary_expr_ptr = &*binary_expr as *const _;
    if let Expression::UnaryExpression(unary_expr) = &mut binary_expr.left {
        unary_expr.parent = Parent::BinaryExpressionLeft(binary_expr_ptr);
    }
    if let Expression::StringLiteral(str_lit) = &mut binary_expr.right {
        str_lit.parent = Parent::BinaryExpressionRight(binary_expr_ptr);
    }

    // `typeof foo === 'object'` (as expression statement)
    let mut expr_stmt = Box(alloc.alloc(ExpressionStatement {
        expression: Expression::BinaryExpression(binary_expr),
        parent: Parent::None,
    }));

    let expr_stmt_ptr = &*expr_stmt as *const _;
    if let Expression::BinaryExpression(binary_expr) = &mut expr_stmt.expression {
        binary_expr.parent = Parent::ExpressionStatement(expr_stmt_ptr);
    }

    // `typeof foo === 'object'` (as statement)
    let stmt = Statement::ExpressionStatement(expr_stmt);

    // `typeof foo === 'object'` (as program)
    let mut body = Vec::new_in(alloc);
    body.push(stmt);
    let program = alloc.alloc(Program { body });

    let program_ptr = program as *const _;
    let Statement::ExpressionStatement(expr_stmt) = program.body.iter_mut().next().unwrap();
    expr_stmt.parent = Parent::Program(program_ptr);

    program
}
