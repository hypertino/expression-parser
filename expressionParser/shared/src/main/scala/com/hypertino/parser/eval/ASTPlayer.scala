package com.hypertino.parser.eval

import com.hypertino.binders.value.{False, Text, True, Value}
import com.hypertino.parser.ast._

import scala.util.control.NonFatal

trait ASTPlayer {
  def evaluator: Evaluator

  def play(rootExpression: Expression): Value = {
    rootExpression match {
      case Constant(value) => value
      case i : Identifier =>
        if (evaluator.identifier.isDefinedAt(i))
          evaluator.identifier(i)
        else
          unknownIdentifier(i)

      case UnaryOperation(operation, argument) =>
        val eArg = play(argument)
        if (evaluator.unaryOperation.isDefinedAt(operation, eArg))
          evaluator.unaryOperation(operation, eArg)
        else
          unknownUnaryOperation(operation, eArg)

      case BinaryOperation(left, operation, right) =>
        val eLeft = play(left)

        val resultBasedOnLeft = if (evaluator.binaryOperationLeftArgument.isDefinedAt(eLeft, operation)) {
          evaluator.binaryOperationLeftArgument(eLeft, operation)
        } else {
          None
        }
        resultBasedOnLeft.getOrElse {
          val eRight = play(right)
          if (evaluator.binaryOperation.isDefinedAt(eLeft, operation, eRight))
            evaluator.binaryOperation(eLeft, operation, eRight)
          else
            unknownBinaryOperation(eLeft, operation, eRight)
        }

      case Function(functionIdentifier, arguments) =>
        // special case
        if (functionIdentifier.segments.head == "exists" && functionIdentifier.segments.tail.isEmpty ) {
          try {
            arguments.map(play)
            True
          } catch {
            case NonFatal(e) =>
              False
          }
        }
        else {
          val eArgs = arguments.map(play)
          if (evaluator.function.isDefinedAt(functionIdentifier, eArgs))
            evaluator.function(functionIdentifier, eArgs)
          else
            unknownFunction(functionIdentifier, eArgs)
        }

      case StringInterpolation(arguments) =>
        Text(arguments.map(play).mkString)
    }
  }

  def unknownIdentifier(identifier: Identifier): Value = {
    throw new EvalIdentifierNotFound(identifier.toString)
  }
  def unknownFunction(identifier: Identifier, arguments: Seq[Value]): Value = {
    throw new EvalFunctionNotFound(identifier.toString)
  }
  def unknownBinaryOperation(left: Value, operator: Identifier, right: Value): Value = {
    throw new EvalBinaryOperationNotFound(operator.toString)
  }
  def unknownUnaryOperation(operator: Identifier, argument: Value): Value = {
    throw new EvalUnaryOperationNotFound(operator.toString)
  }
}
