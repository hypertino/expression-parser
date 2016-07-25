package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.{BinaryOperatorNotFound, FunctionNotFound, IdentifierNotFound, UnaryOperatorNotFound}

trait EvaluatorApi {
  def binaryOperation(left: Value, operator: String, right: Value): Value = unknownBinaryOperation(left, operator, right)
  def unaryOperation(operator: String, argument: Value): Value = unknownUnaryOperation(operator, argument)
  def identifier(name: String): Value = unknownIdentifier(name)
  def function(name: String, arguments: Seq[Value]): Value = unknownFunction(name, arguments)

  def unknownBinaryOperation(left: Value, operator: String, right: Value): Value = {
    throw new BinaryOperatorNotFound(operator)
  }
  def unknownUnaryOperation(operator: String, argument: Value): Value = {
    throw new UnaryOperatorNotFound(operator)
  }
  def unknownIdentifier(name: String): Value = {
    throw new IdentifierNotFound(name)
  }
  def unknownFunction(name: String, arguments: Seq[Value]): Value = {
    throw new FunctionNotFound(name)
  }
}
