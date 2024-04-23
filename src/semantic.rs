use crate::{
    ast::{
        traversable::{
            BinaryExpression, ComputedMemberExpression, ExpressionStatement, IdentifierReference,
            Parent, Program as TraversableProgram, StaticMemberExpression, StringLiteral,
            UnaryExpression,
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
        self.current_parent = Parent::ProgramBody(program);
        self.walk_program(program, tk);
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        tk: &mut Token,
    ) {
        let expr_stmt_mut = expr_stmt.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { expr_stmt_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::ExpressionStatementExpression(expr_stmt);
        self.walk_expression_statement(expr_stmt, tk);
    }

    #[allow(unused_variables)]
    fn visit_identifier_reference(
        &mut self,
        id: SharedBox<'a, IdentifierReference<'a>>,
        tk: &mut Token,
    ) {
        let id_mut = id.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { id_mut.set_parent(self.current_parent) };
    }

    #[allow(unused_variables)]
    fn visit_string_literal(&mut self, str_lit: SharedBox<'a, StringLiteral<'a>>, tk: &mut Token) {
        let str_lit_mut = str_lit.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { str_lit_mut.set_parent(self.current_parent) };
    }

    fn visit_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        tk: &mut Token,
    ) {
        let bin_expr_mut = bin_expr.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { bin_expr_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::BinaryExpressionLeft(bin_expr);
        self.visit_expression(bin_expr.left(tk), tk);
        self.current_parent = Parent::BinaryExpressionRight(bin_expr);
        self.visit_expression(bin_expr.right(tk), tk);
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        tk: &mut Token,
    ) {
        let unary_expr_mut = unary_expr.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { unary_expr_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::UnaryExpressionArgument(unary_expr);
        self.walk_unary_expression(unary_expr, tk);
    }

    fn visit_computed_member_expression(
        &mut self,
        expr: SharedBox<'a, ComputedMemberExpression<'a>>,
        tk: &mut Token,
    ) {
        let expr_mut = expr.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { expr_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::ComputedMemberExpressionObject(expr);
        self.visit_expression(expr.object(tk), tk);
        self.current_parent = Parent::ComputedMemberExpressionExpression(expr);
        self.visit_expression(expr.expression(tk), tk);
    }

    fn visit_static_member_expression(
        &mut self,
        expr: SharedBox<'a, StaticMemberExpression<'a>>,
        tk: &mut Token,
    ) {
        let expr_mut = expr.borrow_mut(tk);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { expr_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::StaticMemberExpressionObject(expr);
        self.visit_expression(expr.object(tk), tk);
    }
}
