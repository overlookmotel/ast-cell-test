pub use crate::cell::TransformCtx;
use crate::{
    ast::{
        traversable::{
            BinaryExpression, Expression, ExpressionStatement, IdentifierReference,
            Program as TraversableProgram, Statement, StringLiteral, UnaryExpression,
        },
        traversable_traits::*,
        Program,
    },
    cell::{GCell, SharedBox},
};

/// Run transform visitor on AST.
///
/// The provided transformer must implement `Traverse` and will be run on a version of the AST
/// with interior mutability, allowing traversal in any direction (up or down).
/// Once the transform is finished, caller can continue to use the standard version of the AST
/// in the usual way, without interior mutability.
pub fn transform<'a, T: Traverse<'a>>(transformer: &mut T, program: &mut Program<'a>) {
    // Generate `TransformCtx` which transformer uses to access the AST.
    // SAFETY: We only create one context object, and it never leaves this function.
    let mut ctx = unsafe { TransformCtx::new_unchecked() };

    // Convert AST to traversable version.
    // SAFETY: All standard and traversable AST types are mirrors of each other, with identical layouts.
    // This is ensured by `#[repr(C)]` on all types. Therefore one can safely be transmuted to the other.
    // As we hold a `&mut` reference to the AST, it's guaranteed there are no other live references.
    // We extend the lifetime of ref to `TraversableProgram` to `&'a TraversableProgram`.
    // This is safe because the node is in the arena, and doesn't move, so the ref is valid for `'a`.
    // `transformer` could smuggle refs out, but could not use them without a context object which is
    // only available in this function.
    // TODO: Refs could be made invalid if the allocator is reset. Hopefully this is impossible
    // because `Allocator::reset` takes a `&mut` ref to the allocator, so you can't hold any immut refs
    // to data in the arena at that time. But make sure.
    let program =
        GCell::from_mut(unsafe { &mut *(program as *mut Program as *mut TraversableProgram) });

    // Run transformer on the traversable AST
    Traverse::visit_program(transformer, program, &mut ctx);

    // Check no dummy AST nodes left in the AST
    assert!(
        ctx.dummy_count() == 0,
        "Transform left dummy AST nodes in the AST"
    );

    // The context object goes out of scope at this point, which guarantees that no references
    // (either mutable or immutable) to the traversable AST or the context object still exist.
    // If the transformer attempts to hold on to any references to the AST, or to the context object,
    // this will produce a compile-time error.
    // Therefore, the caller can now safely continue using the `&mut Statement` that they passed in.
}

pub trait Traverse<'a> {
    fn visit_program(
        &mut self,
        program: SharedBox<'a, TraversableProgram<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.walk_program(program, ctx)
    }

    fn walk_program(
        &mut self,
        program: SharedBox<'a, TraversableProgram<'a>>,
        ctx: &mut TransformCtx,
    ) {
        // Need to read `len()` on each turn of the loop, as `visit_statement` (or a child of it)
        // could add more nodes to the `Vec`
        let mut index = 0;
        while index < program.body_len(ctx) {
            let stmt = program.body_stmt(index, ctx);
            self.visit_statement(stmt, ctx);
            index += 1;
        }
    }

    fn visit_statement(&mut self, stmt: Statement<'a>, ctx: &mut TransformCtx) {
        self.walk_statement(stmt, ctx)
    }

    fn walk_statement(&mut self, stmt: Statement<'a>, ctx: &mut TransformCtx) {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                self.visit_expression_statement(expr_stmt, ctx)
            }
            Statement::Dummy => unreachable!(),
        }
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.walk_expression_statement(expr_stmt, ctx);
    }

    fn walk_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.visit_expression(expr_stmt.expression(ctx), ctx);
    }

    fn visit_expression(&mut self, expr: Expression<'a>, ctx: &mut TransformCtx) {
        self.walk_expression(expr, ctx);
    }

    fn walk_expression(&mut self, expr: Expression<'a>, ctx: &mut TransformCtx) {
        match expr {
            Expression::Identifier(id) => {
                self.visit_identifier_reference(id, ctx);
            }
            Expression::StringLiteral(str_lit) => {
                self.visit_string_literal(str_lit, ctx);
            }
            Expression::BinaryExpression(bin_expr) => {
                self.visit_binary_expression(bin_expr, ctx);
            }
            Expression::UnaryExpression(unary_expr) => {
                self.visit_unary_expression(unary_expr, ctx);
            }
            Expression::Dummy => unreachable!(),
        }
    }

    #[allow(unused_variables)]
    fn visit_identifier_reference(
        &mut self,
        id: SharedBox<'a, IdentifierReference<'a>>,
        ctx: &mut TransformCtx,
    ) {
    }

    #[allow(unused_variables)]
    fn visit_string_literal(
        &mut self,
        str_lit: SharedBox<'a, StringLiteral<'a>>,
        ctx: &mut TransformCtx,
    ) {
    }

    fn visit_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.walk_binary_expression(bin_expr, ctx);
    }

    fn walk_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.visit_expression(bin_expr.left(ctx), ctx);
        self.visit_expression(bin_expr.right(ctx), ctx);
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.walk_unary_expression(unary_expr, ctx);
    }

    fn walk_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &mut TransformCtx,
    ) {
        self.visit_expression(unary_expr.argument(ctx), ctx);
    }
}
