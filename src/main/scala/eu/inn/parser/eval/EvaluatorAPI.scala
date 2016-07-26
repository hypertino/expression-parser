package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.EvalEntityNotFound
import eu.inn.parser.ast.Identifier

trait EvaluatorAPI {
  def context: ContextAPI

  def binaryOperation(left: Value, operator: Identifier, right: Value): Value = unknownBinaryOperation(left, operator, right)
  def unaryOperation(operator: Identifier, argument: Value): Value = unknownUnaryOperation(operator, argument)

  def unknownBinaryOperation(left: Value, operator: Identifier, right: Value): Value = {
    throw new EvalEntityNotFound(operator.toString)
  }
  def unknownUnaryOperation(operator: Identifier, argument: Value): Value = {
    throw new EvalEntityNotFound(operator.toString)
  }
}
