package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.ast._

trait ASTPlayer extends EvaluatorApi {
  def play(rootExpression: Expression): Value = {
    rootExpression match {
      case Const(value) ⇒ value
      case Identifier(name) ⇒ identifier(name)
      case UnaryOperation(operation, argument) ⇒
        unaryOperation(operation.name, play(argument))
      case BinaryOperation(left, operation, right) ⇒
        binaryOperation(play(left), operation.name, play(right))
      case Function(functionIdentifier, arguments) ⇒
        function(functionIdentifier.name, arguments.map(play))
    }
  }
}
