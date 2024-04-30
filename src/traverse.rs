use oxc_allocator::Allocator;

use crate::{
    ast::{
        traversable::{
            Ancestor, BinaryExpression, Expression, ExpressionStatement, IdentifierReference,
            Program as TraversableProgram, Statement, StringLiteral, UnaryExpression,
        },
        traverse, Program, TraversableAstBuilder,
    },
    cell::{GCell, SharedBox, Token},
};

/// Run transform visitor on AST.
///
/// The provided transformer must implement `Traverse` and will be run on a version of the AST
/// with interior mutability, allowing traversal in any direction (up or down).
/// Once the transform is finished, caller can continue to use the standard version of the AST
/// in the usual way, without interior mutability.
pub fn transform<'a, T: Traverse<'a>>(
    transformer: &mut T,
    program: &mut Program<'a>,
    allocator: &'a Allocator,
) {
    // Generate `Token` which transformer uses to access the AST.
    // SAFETY: We only create one token, and it never leaves this function.
    let mut token = unsafe { Token::new_unchecked() };

    // Create `TraverseCtx` which transformer uses to read ancestry
    let mut ctx = TraverseCtx::new(allocator);

    // Convert AST to traversable version.
    // SAFETY: All standard and traversable AST types are mirrors of each other, with identical layouts.
    // This is ensured by `#[repr(C)]` on all types. Therefore one can safely be transmuted to the other.
    // As we hold a `&mut` reference to the AST, it's guaranteed there are no other live references.
    // We extend the lifetime of ref to `TraversableProgram` to `&'a TraversableProgram`.
    // This is safe because the node is in the arena, and doesn't move, so the ref is valid for `'a`.
    // `transformer` could smuggle refs out, but could not use them without a token which is only
    // available in this function.
    // TODO: Refs could be made invalid if the allocator is reset. Hopefully this is impossible
    // because `Allocator::reset` takes a `&mut` ref to the allocator, so you can't hold any immut refs
    // to data in the arena at that time. But make sure.
    let program =
        GCell::from_mut(unsafe { &mut *(program as *mut Program as *mut TraversableProgram) });

    // Run transformer on the traversable AST
    traverse(transformer, program, &mut ctx, &mut token);

    // The access token goes out of scope at this point, which guarantees that no references
    // (either mutable or immutable) to the traversable AST or the token still exist.
    // If the transformer attempts to hold on to any references to the AST, or to the token,
    // this will produce a compile-time error.
    // Therefore, the caller can now safely continue using the `&mut Statement` that they passed in.
}

/// Traverse context.
///
/// Passed to all AST visitor functions.
///
/// Provides ability to:
/// * Query parent/ancestor of current node.
/// * Create AST nodes via `ctx.ast`.
/// * Allocate into arena via `ctx.alloc()`.
pub struct TraverseCtx<'a> {
    stack: Vec<Ancestor<'a>>,
    pub ast: TraversableAstBuilder<'a>,
}

impl<'a> TraverseCtx<'a> {
    pub fn new(allocator: &'a Allocator) -> Self {
        Self {
            stack: Vec::new(),
            ast: TraversableAstBuilder::new(allocator),
        }
    }

    #[allow(dead_code)] // TODO: Remove this attr once method is used in a transform
    #[inline]
    pub fn alloc<T>(&self, node: T) -> SharedBox<'a, T> {
        self.ast.alloc(node)
    }
}

impl<'a> TraverseCtx<'a> {
    #[inline]
    pub fn parent(&self) -> Ancestor<'a> {
        *self.stack.last().unwrap()
    }

    #[allow(dead_code)] // TODO: Remove this attr once method is used in a transform
    #[inline]
    pub fn ancestor(&self, levels: usize) -> Option<Ancestor<'a>> {
        self.stack.get(self.stack.len() - levels).copied()
    }

    #[inline]
    pub fn push_stack(&mut self, ancestor: Ancestor<'a>) {
        self.stack.push(ancestor);
    }

    #[inline]
    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    #[inline]
    pub fn replace_stack(&mut self, ancestor: Ancestor<'a>) {
        *self.stack.last_mut().unwrap() = ancestor;
    }
}

#[allow(unused_variables)]
pub trait Traverse<'a> {
    fn enter_program(
        &mut self,
        program: SharedBox<'a, TraversableProgram<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }
    fn exit_program(
        &mut self,
        program: SharedBox<'a, TraversableProgram<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }

    fn enter_statement(&mut self, stmt: Statement<'a>, ctx: &TraverseCtx<'a>, tk: &mut Token) {}
    fn exit_statement(&mut self, stmt: Statement<'a>, ctx: &TraverseCtx<'a>, tk: &mut Token) {}

    fn enter_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }
    fn exit_expression_statement(
        &mut self,
        expr_stmt: SharedBox<'a, ExpressionStatement<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }

    fn enter_expression(&mut self, expr: Expression<'a>, ctx: &TraverseCtx<'a>, tk: &mut Token) {}
    fn exit_expression(&mut self, expr: Expression<'a>, ctx: &TraverseCtx<'a>, tk: &mut Token) {}

    fn visit_identifier_reference(
        &mut self,
        id: SharedBox<'a, IdentifierReference<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }

    fn visit_string_literal(
        &mut self,
        str_lit: SharedBox<'a, StringLiteral<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }

    fn enter_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }
    fn exit_binary_expression(
        &mut self,
        bin_expr: SharedBox<'a, BinaryExpression<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }

    fn enter_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }
    fn exit_unary_expression(
        &mut self,
        unary_expr: SharedBox<'a, UnaryExpression<'a>>,
        ctx: &TraverseCtx<'a>,
        tk: &mut Token,
    ) {
    }
}
