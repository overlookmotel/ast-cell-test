use oxc_allocator::Vec;

use crate::ast::{
    BinaryExpression, Expression, ExpressionStatement, IdentifierReference, Program, Statement,
    StringLiteral, UnaryExpression,
};

#[allow(clippy::single_match)]
pub trait Visit<'a> {
    fn visit_program(&mut self, program: &Program<'a>) {
        self.walk_program(program)
    }

    fn walk_program(&mut self, program: &Program<'a>) {
        self.visit_statements(&program.body);
    }

    fn visit_statements(&mut self, stmts: &Vec<'a, Statement<'a>>) {
        self.walk_statements(stmts);
    }

    fn walk_statements(&mut self, stmts: &Vec<'a, Statement<'a>>) {
        for stmt in stmts {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement<'a>) {
        self.walk_statement(stmt)
    }

    fn walk_statement(&mut self, stmt: &Statement<'a>) {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => self.visit_expression_statement(expr_stmt), // _ => {} // No other variants at present
        }
    }

    fn visit_expression_statement(&mut self, expr_stmt: &ExpressionStatement<'a>) {
        self.walk_expression_statement(expr_stmt);
    }

    fn walk_expression_statement(&mut self, expr_stmt: &ExpressionStatement<'a>) {
        self.visit_expression(&expr_stmt.expression);
    }

    fn visit_expression(&mut self, expr: &Expression<'a>) {
        self.walk_expression(expr);
    }

    fn walk_expression(&mut self, expr: &Expression<'a>) {
        match expr {
            Expression::Identifier(id) => {
                self.visit_identifier_reference(id);
            }
            Expression::StringLiteral(str_lit) => {
                self.visit_string_literal(str_lit);
            }
            Expression::BinaryExpression(bin_expr) => {
                self.visit_binary_expression(bin_expr);
            }
            Expression::UnaryExpression(unary_expr) => {
                self.visit_unary_expression(unary_expr);
            }
            Expression::Dummy => unreachable!(),
        }
    }

    #[allow(unused_variables)]
    fn visit_identifier_reference(&mut self, id: &IdentifierReference<'a>) {}

    #[allow(unused_variables)]
    fn visit_string_literal(&mut self, str_lit: &StringLiteral<'a>) {}

    fn visit_binary_expression(&mut self, bin_expr: &BinaryExpression<'a>) {
        self.walk_binary_expression(bin_expr);
    }

    fn walk_binary_expression(&mut self, bin_expr: &BinaryExpression<'a>) {
        self.visit_expression(&bin_expr.left);
        self.visit_expression(&bin_expr.right);
    }

    fn visit_unary_expression(&mut self, unary_expr: &UnaryExpression<'a>) {
        self.walk_unary_expression(unary_expr);
    }

    fn walk_unary_expression(&mut self, unary_expr: &UnaryExpression<'a>) {
        self.visit_expression(&unary_expr.argument);
    }
}
