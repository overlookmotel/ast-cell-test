use oxc_allocator::Allocator;

mod ast;
mod cell;
mod parser;
mod print;
mod semantic;
mod traverse;
mod visit;
use ast::{
    traversable::{Expression, Parent, UnaryExpression},
    traversable_traits::*,
    BinaryOperator, UnaryOperator,
};
use cell::{SharedBox, Token};
use print::Printer;
use semantic::semantic;
use traverse::{transform, Traverse};
use visit::Visit;

fn main() {
    let alloc = Allocator::default();
    let program = parser::parse(&alloc);
    semantic(program);
    println!("before: {}", Printer::print(program));

    transform(&mut TransformTypeof, program);
    println!("after: {}", Printer::print(program));
}

/// Transformer for `typeof x === 'y'` to `'y' === typeof x`
struct TransformTypeof;

impl<'a> Traverse<'a> for TransformTypeof {
    fn visit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        tk: &mut Token,
    ) {
        self.walk_unary_expression(unary_expr, tk);

        if unary_expr.operator(tk) == UnaryOperator::Typeof {
            if let Parent::BinaryExpressionLeft(bin_expr) = unary_expr.parent(tk) {
                if matches!(
                    bin_expr.operator(tk),
                    BinaryOperator::Equality | BinaryOperator::StrictEquality
                ) && matches!(bin_expr.right(tk), Expression::StringLiteral(_))
                {
                    // Swap left and right of binary expression
                    let left = bin_expr.take_left(tk);
                    let right = bin_expr.take_right(tk);
                    bin_expr.set_left(right, tk);
                    bin_expr.set_right(left, tk);
                }
            }
        }
    }
}

// Run these tests under Miri
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_ub_transforming() {
        let alloc = Allocator::default();
        let program = parser::parse(&alloc);
        semantic(program);
        transform(&mut TransformTypeof, program);
    }

    #[test]
    fn no_ub_mutating_standard_ast_after_transform() {
        use ast::{Expression, IdentifierReference, Parent, Statement, StringLiteral};
        use oxc_allocator::Box;
        use std::mem;

        let alloc = Allocator::default();
        let program = parser::parse(&alloc);
        semantic(program);
        transform(&mut TransformTypeof, program);

        let stmt = program.body.first_mut().unwrap();
        let expr_stmt = if let Statement::ExpressionStatement(expr_stmt) = stmt {
            expr_stmt
        } else {
            unreachable!();
        };

        let bin_expr = if let Expression::BinaryExpression(bin_expr) = &mut expr_stmt.expression {
            bin_expr
        } else {
            unreachable!();
        };
        let left = mem::replace(&mut bin_expr.left, Expression::Dummy);
        let right = mem::replace(&mut bin_expr.right, left);
        bin_expr.left = right;

        let unary_expr = if let Expression::UnaryExpression(unary_expr) = &mut bin_expr.left {
            &mut **unary_expr
        } else {
            unreachable!();
        };
        unary_expr.operator = UnaryOperator::UnaryNegation;

        let id = if let Expression::Identifier(unary_expr) = &mut unary_expr.argument {
            &mut **unary_expr
        } else {
            unreachable!();
        };
        id.name = "bar";

        unary_expr.argument = Expression::StringLiteral(Box(alloc.alloc(StringLiteral {
            value: "foo",
            parent: Parent::None,
        })));

        let str_lit = if let Expression::StringLiteral(str_lit) = &mut bin_expr.right {
            &mut **str_lit
        } else {
            unreachable!();
        };
        str_lit.value = "string";

        bin_expr.right = Expression::Identifier(Box(alloc.alloc(IdentifierReference {
            name: "qux",
            parent: Parent::None,
        })));
    }
}
