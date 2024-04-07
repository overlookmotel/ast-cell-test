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
    BinaryOperator, UnaryOperator,
};
use cell::{GCell, Token};
use print::Printer;
use semantic::semantic;
use traverse::{transform, Traverse};
use visit::Visit;

// TODO: Implement semantic as a `Traverse` to set parents on nodes, rather than doing it in parser.
// Doing it in parser is unsound, as the pointers are incorrectly tagged. Miri says it's UB.
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
    fn visit_unary_expression(&mut self, unary_expr: &GCell<UnaryExpression<'a>>, tk: &mut Token) {
        self.walk_unary_expression(unary_expr, tk);

        if unary_expr.borrow(tk).operator == UnaryOperator::Typeof {
            if let Parent::BinaryExpressionLeft(bin_expr) = unary_expr.borrow(tk).parent {
                if matches!(
                    bin_expr.borrow(tk).operator,
                    BinaryOperator::Equality | BinaryOperator::StrictEquality
                ) {
                    if let Expression::StringLiteral(str_lit) = bin_expr.borrow(tk).right {
                        // Swap left and right of binary expression
                        let bin_expr_mut = bin_expr.borrow_mut(tk);
                        std::mem::swap(&mut bin_expr_mut.left, &mut bin_expr_mut.right);

                        // Update parent links of left and right
                        let temp = str_lit.borrow(tk).parent;
                        str_lit.borrow_mut(tk).parent = unary_expr.borrow(tk).parent;
                        unary_expr.borrow_mut(tk).parent = temp;
                    }
                }
            }
        }
    }
}
