use crate::{
    ast::{
        BinaryExpression, BinaryOperator, IdentifierReference, Statement, StringLiteral,
        UnaryExpression, UnaryOperator,
    },
    Visit,
};

/// Codegen implemented as a visitor.
/// Real codegen would not be implemented like this.
/// This just a quick hack to get something to check this demo is working.
pub struct Printer {
    output: String,
}

impl Printer {
    pub fn print(stmt: &Statement<'_>) -> String {
        let mut printer = Printer {
            output: String::new(),
        };
        printer.visit_statement(stmt);
        printer.output
    }

    fn output(&mut self, s: &str) {
        self.output.push_str(s);
    }
}

impl<'a, 't> Visit<'a, 't> for Printer {
    fn visit_identifier_reference(&mut self, id: &IdentifierReference<'a>) {
        self.output(id.name);
    }

    fn visit_string_literal(&mut self, str_lit: &StringLiteral<'a>) {
        self.output(&format!("'{}'", str_lit.value));
    }

    fn visit_unary_expression(&mut self, unary_expr: &UnaryExpression<'a>) {
        match unary_expr.operator {
            UnaryOperator::UnaryNegation => self.output("-"),
            UnaryOperator::UnaryPlus => self.output("+"),
            UnaryOperator::LogicalNot => self.output("!"),
            UnaryOperator::BitwiseNot => self.output("~"),
            UnaryOperator::Typeof => self.output("typeof "),
            UnaryOperator::Void => self.output("void "),
            UnaryOperator::Delete => self.output("delete "),
        }
        self.visit_expression(&unary_expr.argument);
    }

    fn visit_binary_expression(&mut self, bin_expr: &BinaryExpression<'a>) {
        self.visit_expression(&bin_expr.left);
        self.output(&format!(
            " {} ",
            match bin_expr.operator {
                BinaryOperator::Equality => "==",
                BinaryOperator::StrictEquality => "===",
            }
        ));
        self.visit_expression(&bin_expr.right);
    }
}
