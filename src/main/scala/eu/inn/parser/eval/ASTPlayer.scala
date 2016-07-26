package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.ast._

trait ASTPlayer {
  def evaluator: EvaluatorAPI

  def play(rootExpression: Expression): Value = {
    rootExpression match {
      case Constant(value) ⇒ value
      case i : Identifier ⇒ evaluator.context.identifier(i)
      case UnaryOperation(operation, argument) ⇒
        evaluator.unaryOperation(operation, play(argument))
      case BinaryOperation(left, operation, right) ⇒
        evaluator.binaryOperation(play(left), operation, play(right))
      case Function(functionIdentifier, arguments) ⇒
        evaluator.context.function(functionIdentifier, arguments.map(play))
    }
  }
}
