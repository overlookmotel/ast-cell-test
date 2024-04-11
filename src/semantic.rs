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
    let mut semantic = Semantic { parents: vec![] };
    transform(&mut semantic, program);
}

struct Semantic<'a> {
    parents: Vec<Parent<'a>>,
}

impl<'a> Traverse<'a> for Semantic<'a> {
    fn visit_program(&mut self, program: SharedBox<'a, TraversableProgram<'a>>, tk: &mut Token) {
        self.parents.push(Parent::Program(program));
        self.walk_program(program, tk);
        self.parents.pop();
    }

    fn visit_statement(&mut self, stmt: &Statement<'a>, tk: &mut Token) {
        self.walk_statement(stmt, tk)
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        tk: &mut Token,
    ) {
        self.parents.push(Parent::ExpressionStatement(expr_stmt));
        self.walk_expression_statement(expr_stmt, tk);
        self.parents.pop();
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
        self.parents.push(Parent::BinaryExpressionLeft(bin_expr));
        self.walk_binary_expression(bin_expr, tk);
        self.parents.pop();
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        tk: &mut Token,
    ) {
        self.walk_unary_expression(unary_expr, tk);
    }
}
