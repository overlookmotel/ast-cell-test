use oxc_allocator::Allocator;

mod ast;
mod cell;
mod parser;
mod print;
mod traverse;
mod visit;
use ast::{
    traversable::{Expression, Parent, UnaryExpression},
    BinaryOperator, TraversableField, UnaryOperator,
};
use cell::{SharedBox, Token};
use print::Printer;
use traverse::{transform, Traverse, TraverseCtx};
use visit::Visit;

fn main() {
    let alloc = Allocator::default();
    let program = parser::parse(&alloc);
    println!("before: {}", Printer::print(program));

    transform(&mut TransformTypeof, program);
    println!("after: {}", Printer::print(program));
}

/// Transformer for `typeof x === 'y'` to `'y' === typeof x`
struct TransformTypeof;

impl<'a> Traverse<'a> for TransformTypeof {
    fn exit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
        if unary_expr.operator(tk) == UnaryOperator::Typeof {
            if let Parent::BinaryExpressionLeft(bin_expr) = ctx.parent() {
                if matches!(
                    bin_expr.operator(tk),
                    BinaryOperator::Equality | BinaryOperator::StrictEquality
                ) && matches!(bin_expr.right(tk), Expression::StringLiteral(_))
                {
                    // Swap left and right of binary expression
                    bin_expr.left_ref().swap_with(bin_expr.right_ref(), tk);
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
        transform(&mut TransformTypeof, program);
    }

    #[test]
    fn mutate_standard_ast_after_transform() {
        use ast::{Expression, IdentifierReference, Statement, StringLiteral};
        use oxc_allocator::Box;
        use std::mem;

        let alloc = Allocator::default();
        let program = parser::parse(&alloc);
        transform(&mut TransformTypeof, program);

        let stmt = program.body.first_mut().unwrap();
        #[allow(irrefutable_let_patterns)]
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
        let left = mem::replace(
            &mut bin_expr.left,
            Expression::StringLiteral(Box(alloc.alloc(StringLiteral { value: "whatever" }))),
        );
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

        unary_expr.argument =
            Expression::StringLiteral(Box(alloc.alloc(StringLiteral { value: "foo" })));

        let str_lit = if let Expression::StringLiteral(str_lit) = &mut bin_expr.right {
            &mut **str_lit
        } else {
            unreachable!();
        };
        str_lit.value = "string";

        bin_expr.right =
            Expression::Identifier(Box(alloc.alloc(IdentifierReference { name: "qux" })));
    }
}
