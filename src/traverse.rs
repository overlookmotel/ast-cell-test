use crate::{
    ast::{
        traversable::{
            BinaryExpression, Expression, ExpressionStatement, IdentifierReference,
            Program as TraversableProgram, Statement, StringLiteral, UnaryExpression,
        },
        Program,
    },
    cell::{gcell, GCell, Token},
};

/// Run transform visitor on AST.
///
/// The provided transformer must implement `Traverse` and will be run on a version of the AST
/// with interior mutability, allowing traversal in any direction (up or down).
/// Once the transform is finished, caller can continue to use the standard version of the AST
/// in the usual way, without interior mutability.
pub fn transform<'a, 't, T>(transformer: &mut T, program: &mut Program<'a>)
where
    't: 'a,
    T: Traverse<'a, 't>,
{
    // Generate `GhostToken` which transformer uses to access the AST.
    // SAFETY: We only create one token, and it never leaves this function.
    let mut token = unsafe { Token::new_unchecked() };

    // Convert AST to traversable version.
    // SAFETY: `Program` and `TraversableProgram` are mirrors of each other, with identical layouts.
    // The same is true of all child types - this is ensured by `#[repr(C)]` on all types.
    // Therefore one can safely be transmuted to the other.
    // As we hold a `&mut` reference, it's guaranteed there are no other live references.
    let program = unsafe { &mut *(program as *mut Program<'a> as *mut TraversableProgram<'a, 't>) };
    let program = GCell::from_mut(program);

    // Run transformer on the traversable AST
    Traverse::visit_program(transformer, program, &mut token);

    // The access token goes out of scope at this point, which guarantees that no references
    // (either mutable or immutable) to the traversable AST or the token still exist.
    // If the transformer attempts to hold on to any references to the AST, or to the token,
    // this will produce a compile-time error.
    // Therefore, the caller can now safely continue using the `&mut Statement` that they passed in.
}

pub trait Traverse<'a, 't> {
    fn visit_program(&mut self, program: &gcell!(TraversableProgram<'a, 't>), tk: &mut Token<'t>) {
        self.walk_program(program, tk)
    }

    fn walk_program(&mut self, program: &gcell!(TraversableProgram<'a, 't>), tk: &mut Token<'t>) {
        // Need to read `len()` on each turn of the loop, as `visit_statement` (or a child of it)
        // could add more nodes to the `Vec`
        let mut index = 0;
        while index < program.borrow(tk).body.len() {
            let stmt = program.borrow(tk).body.as_slice()[index].borrow(tk).clone();
            self.visit_statement(&stmt, tk);
            index += 1;
        }
    }

    fn visit_statement(&mut self, stmt: &Statement<'a, 't>, tk: &mut Token<'t>) {
        self.walk_statement(stmt, tk)
    }

    fn walk_statement(&mut self, stmt: &Statement<'a, 't>, tk: &mut Token<'t>) {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                self.visit_expression_statement(expr_stmt, tk)
            } // _ => {} // No other variants at present
        }
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &gcell!(ExpressionStatement<'a, 't>),
        tk: &mut Token<'t>,
    ) {
        self.walk_expression_statement(expr_stmt, tk);
    }

    fn walk_expression_statement(
        &mut self,
        expr_stmt: &gcell!(ExpressionStatement<'a, 't>),
        tk: &mut Token<'t>,
    ) {
        self.visit_expression(&expr_stmt.borrow(tk).expression.clone(), tk);
    }

    fn visit_expression(&mut self, expr: &Expression<'a, 't>, tk: &mut Token<'t>) {
        self.walk_expression(expr, tk);
    }

    fn walk_expression(&mut self, expr: &Expression<'a, 't>, tk: &mut Token<'t>) {
        match expr {
            Expression::Identifier(id) => {
                self.visit_identifier_reference(id, tk);
            }
            Expression::StringLiteral(str_lit) => {
                self.visit_string_literal(str_lit, tk);
            }
            Expression::BinaryExpression(bin_expr) => {
                self.visit_binary_expression(bin_expr, tk);
            }
            Expression::UnaryExpression(unary_expr) => {
                self.visit_unary_expression(unary_expr, tk);
            }
        }
    }

    #[allow(unused_variables)]
    fn visit_identifier_reference(
        &mut self,
        id: &gcell!(IdentifierReference<'a, 't>),
        tk: &mut Token<'t>,
    ) {
    }

    #[allow(unused_variables)]
    fn visit_string_literal(
        &mut self,
        str_lit: &gcell!(StringLiteral<'a, 't>),
        tk: &mut Token<'t>,
    ) {
    }

    fn visit_binary_expression(
        &mut self,
        bin_expr: &gcell!(BinaryExpression<'a, 't>),
        tk: &mut Token<'t>,
    ) {
        self.walk_binary_expression(bin_expr, tk);
    }

    fn walk_binary_expression(
        &mut self,
        bin_expr: &gcell!(BinaryExpression<'a, 't>),
        tk: &mut Token<'t>,
    ) {
        self.visit_expression(&bin_expr.borrow(tk).left.clone(), tk);
        self.visit_expression(&bin_expr.borrow(tk).right.clone(), tk);
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: &gcell!(UnaryExpression<'a, 't>),
        tk: &mut Token<'t>,
    ) {
        self.walk_unary_expression(unary_expr, tk);
    }

    fn walk_unary_expression(
        &mut self,
        unary_expr: &gcell!(UnaryExpression<'a, 't>),
        tk: &mut Token<'t>,
    ) {
        self.visit_expression(&unary_expr.borrow(tk).argument.clone(), tk);
    }
}
