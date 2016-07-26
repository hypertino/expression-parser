package eu.inn.parser.eval

import eu.inn.binders.value.{Number, Value}
import eu.inn.parser.ast.Identifier

trait DefaultEvaluator extends EvaluatorAPI {
  override def binaryOperation(left: Value, operator: Identifier, right: Value): Value = {
    operator.segments.head match {
      case "+" ⇒ add(left, right)
      case "*" ⇒ mul(left, right)
      case _ ⇒ super.binaryOperation(left, operator, right)
    }
  }

  override def unaryOperation(operator: Identifier, argument: Value): Value = {
    super.unaryOperation(operator, argument)
  }

  def add(left: Value, right: Value): Value = {
    left match {
      case Number(n) ⇒ n + right.asBigDecimal
    }
  }

  def mul(left: Value, right: Value): Value = {
    left match {
      case Number(n) ⇒ n * right.asBigDecimal
    }
  }
}
