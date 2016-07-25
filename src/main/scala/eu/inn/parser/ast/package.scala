package eu.inn.parser

import eu.inn.binders.value.Value

package object ast {
  sealed trait Expression
  case class Identifier(name: String) extends Expression
  case class Const(value: Value) extends Expression
  case class UnaryOperation(op: Identifier, argument: Expression) extends Expression
  case class BinaryOperation(leftArgument: Expression, op: Identifier, rightArgument: Expression) extends Expression
  case class Function(name: Identifier, arguments: Seq[Expression]) extends Expression
}
