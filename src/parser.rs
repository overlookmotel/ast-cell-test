use oxc_allocator::{Allocator, Box, Vec};

use crate::ast::{
    BinaryExpression, BinaryOperator, ComputedMemberExpression, Expression, ExpressionStatement,
    IdentifierReference, MemberExpression, Parent, Program, Statement, StringLiteral,
    UnaryExpression, UnaryOperator,
};

/// Create AST for `typeof foo[bar] === 'object'`.
/// Hard-coded here, but these are the steps actual parser would take to create the AST.
pub fn parse(alloc: &Allocator) -> &mut Program {
    // `foo`
    let id = Expression::Identifier(Box(alloc.alloc(IdentifierReference {
        name: "foo",
        parent: Parent::None,
    })));

    // `bar`
    let id2 = Expression::Identifier(Box(alloc.alloc(IdentifierReference {
        name: "bar",
        parent: Parent::None,
    })));

    // `foo[bar]`
    let member_expr = Expression::MemberExpression(Box(alloc.alloc(
        MemberExpression::ComputedMemberExpression(ComputedMemberExpression {
            object: id,
            expression: id2,
            optional: false,
            parent: Parent::None,
        }),
    )));

    // `typeof foo[bar]`
    let unary_expr = Box(alloc.alloc(UnaryExpression {
        operator: UnaryOperator::Typeof,
        argument: member_expr,
        parent: Parent::None,
    }));

    // `'object'`
    let str_lit = Box(alloc.alloc(StringLiteral {
        value: "object",
        parent: Parent::None,
    }));

    // `typeof foo[bar] === 'object'` (as expression)
    let binary_expr = Box(alloc.alloc(BinaryExpression {
        operator: BinaryOperator::StrictEquality,
        left: Expression::UnaryExpression(unary_expr),
        right: Expression::StringLiteral(str_lit),
        parent: Parent::None,
    }));

    // `typeof foo[bar] === 'object'` (as expression statement)
    let expr_stmt = Box(alloc.alloc(ExpressionStatement {
        expression: Expression::BinaryExpression(binary_expr),
        parent: Parent::None,
    }));

    // `typeof foo[bar] === 'object'` (as statement)
    let stmt = Statement::ExpressionStatement(expr_stmt);

    // `typeof foo[bar] === 'object'` (as program)
    let mut body = Vec::new_in(alloc);
    body.push(stmt);
    let program = alloc.alloc(Program { body });

    program
}
