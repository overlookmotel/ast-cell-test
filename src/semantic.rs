use crate::{
    ast::{
        traversable::{
            BinaryExpression, ExpressionStatement, IdentifierReference, Parent,
            Program as TraversableProgram, StringLiteral, UnaryExpression,
        },
        Program,
    },
    cell::SharedBox,
    transform, Traverse, TraverseCtx,
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
    fn visit_program(
        &mut self,
        program: SharedBox<'a, TraversableProgram<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        self.current_parent = Parent::ProgramBody(program);
        self.walk_program(program, ctx);
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        let expr_stmt_mut = expr_stmt.borrow_mut(ctx);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { expr_stmt_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::ExpressionStatementExpression(expr_stmt);
        self.walk_expression_statement(expr_stmt, ctx);
    }

    #[allow(unused_variables)]
    fn visit_identifier_reference(
        &mut self,
        id: SharedBox<'a, IdentifierReference<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        let id_mut = id.borrow_mut(ctx);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { id_mut.set_parent(self.current_parent) };
    }

    #[allow(unused_variables)]
    fn visit_string_literal(
        &mut self,
        str_lit: SharedBox<'a, StringLiteral<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        let str_lit_mut = str_lit.borrow_mut(ctx);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { str_lit_mut.set_parent(self.current_parent) };
    }

    fn visit_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        let bin_expr_mut = bin_expr.borrow_mut(ctx);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { bin_expr_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::BinaryExpressionLeft(bin_expr);
        self.visit_expression(bin_expr.left(ctx), ctx);
        self.current_parent = Parent::BinaryExpressionRight(bin_expr);
        self.visit_expression(bin_expr.right(ctx), ctx);
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &mut TraverseCtx,
    ) {
        let unary_expr_mut = unary_expr.borrow_mut(ctx);
        // SAFETY: We are here establishing the invariant of correct parent tracking
        unsafe { unary_expr_mut.set_parent(self.current_parent) };
        self.current_parent = Parent::UnaryExpressionArgument(unary_expr);
        self.walk_unary_expression(unary_expr, ctx);
    }
}
