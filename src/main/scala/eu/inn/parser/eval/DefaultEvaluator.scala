package eu.inn.parser.eval

import eu.inn.binders.value.{Number, Value}

trait DefaultEvaluator extends EvaluatorApi {
  override def binaryOperation(left: Value, operator: String, right: Value): Value = {
    operator match {
      case "+" ⇒ add(left, right)
      case "*" ⇒ mul(left, right)
      case _ ⇒ super.binaryOperation(left, operator, right)
    }
  }

  override def unaryOperation(operator: String, argument: Value): Value = {
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
