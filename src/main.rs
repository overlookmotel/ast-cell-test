use oxc_allocator::Allocator;

mod ast;
mod cell;
mod parser;
mod print;
mod semantic;
mod traverse;
mod util;
mod visit;
use ast::{
    traversable::{Expression, Parent, UnaryExpression},
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
                    let right = bin_expr.replace_right(left, tk);
                    bin_expr.replace_left(right, tk);
                }
            }
        }
    }
}

// Run these tests under Miri to ensure no UB in transmute between standard and traversable ASTs
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transform_traversable_ast() {
        let alloc = Allocator::default();
        let program = parser::parse(&alloc);
        semantic(program);
        transform(&mut TransformTypeof, program);
    }

    #[test]
    fn mutate_standard_ast_after_transform() {
        use ast::{
            Expression, IdentifierReference, MemberExpression, Parent, Statement,
            StaticMemberExpression, StringLiteral,
        };
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

        let member_expr =
            if let Expression::MemberExpression(member_expr) = &mut unary_expr.argument {
                &mut **member_expr
            } else {
                unreachable!();
            };
        *member_expr = MemberExpression::StaticMemberExpression(StaticMemberExpression {
            object: Expression::Identifier(Box(alloc.alloc(IdentifierReference {
                name: "foo",
                parent: Parent::None,
            }))),
            property: "bar",
            optional: false,
            parent: Parent::None,
        });

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
