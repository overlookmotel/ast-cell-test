use crate::{
    ast::{
        traversable::{
            BinaryExpression, Expression, ExpressionStatement, IdentifierReference, Parent,
            Program as TraversableProgram, Statement, StringLiteral, UnaryExpression,
        },
        Program,
    },
    cell::{SharedBox, Token},
    transform, Traverse,
};

pub fn semantic(program: &mut Program) {
    let mut semantic = Semantic {
        current_parent: Parent::None,
    };
    transform(&mut semantic, program);
}

struct Semantic<'a> {
    current_parent: Parent<'a>,
}

impl<'a> Traverse<'a> for Semantic<'a> {
    fn visit_program(&mut self, program: SharedBox<'a, TraversableProgram<'a>>, tk: &mut Token) {
        self.current_parent = Parent::Program(program);
        self.walk_program(program, tk);
    }

    fn visit_statement(&mut self, stmt: &Statement<'a>, tk: &mut Token) {
        self.walk_statement(stmt, tk)
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        tk: &mut Token,
    ) {
        expr_stmt.borrow_mut(tk).parent = self.current_parent;
        self.current_parent = Parent::ExpressionStatement(expr_stmt);
        self.walk_expression_statement(expr_stmt, tk);
    }

    fn visit_expression(&mut self, expr: &Expression<'a>, tk: &mut Token) {
        self.walk_expression(expr, tk);
    }

    #[allow(unused_variables)]
    fn visit_identifier_reference(
        &mut self,
        id: SharedBox<'a, IdentifierReference<'a>>,
        tk: &mut Token,
    ) {
    }

    #[allow(unused_variables)]
    fn visit_string_literal(&mut self, str_lit: SharedBox<'a, StringLiteral<'a>>, tk: &mut Token) {}

    fn visit_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        tk: &mut Token,
    ) {
        bin_expr.borrow_mut(tk).parent = self.current_parent;
        self.current_parent = Parent::BinaryExpressionLeft(bin_expr);
        self.visit_expression(&bin_expr.borrow(tk).left.clone(), tk);
        self.current_parent = Parent::BinaryExpressionRight(bin_expr);
        self.visit_expression(&bin_expr.borrow(tk).right.clone(), tk);
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        tk: &mut Token,
    ) {
        unary_expr.borrow_mut(tk).parent = self.current_parent;
        self.current_parent = Parent::UnaryExpression(unary_expr);
        self.walk_unary_expression(unary_expr, tk);
    }
}
