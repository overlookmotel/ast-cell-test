use oxc_allocator::Allocator;

mod ast;
mod cell;
mod parser;
mod print;
mod semantic;
mod traverse;
mod visit;
use ast::{
    traversable::{BinaryExpression, Expression, Parent, UnaryExpression},
    BinaryOperator, UnaryOperator,
};
use cell::{SharedBox, Token};
use print::Printer;
use semantic::semantic;
use traverse::{transform, Traverse};
use visit::Visit;

// TODO: Check that mutating the "normal" AST after it's been created does not get flagged by Miri.
// TODO: Make `parent` fields inaccessible in standard AST, so user cannot alter them.

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

        if unary_expr.borrow(tk).operator == UnaryOperator::Typeof {
            if let Parent::BinaryExpressionLeft(bin_expr) = unary_expr.borrow(tk).parent() {
                if matches!(
                    bin_expr.borrow(tk).operator,
                    BinaryOperator::Equality | BinaryOperator::StrictEquality
                ) && matches!(bin_expr.borrow(tk).right(), Expression::StringLiteral(_))
                {
                    // Swap left and right of binary expression
                    let left = BinaryExpression::take_left(bin_expr, tk);
                    let right = BinaryExpression::take_right(bin_expr, tk);
                    BinaryExpression::set_left(bin_expr, right, tk);
                    BinaryExpression::set_right(bin_expr, left, tk);
                }
            }
        }
    }
}
