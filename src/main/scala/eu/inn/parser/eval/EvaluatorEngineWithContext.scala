package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.ast.Identifier

class EvaluatorEngineWithContext(val context: Context) extends EvaluatorEngine {
  override def identifier = context.identifier orElse super.identifier
  override def function = context.function orElse super.function
  override def unaryOperation: PartialFunction[(Identifier, Value), Value] = context.unaryOperation orElse super.unaryOperation
  override def binaryOperation: PartialFunction[(Value, Identifier, Value), Value] = context.binaryOperation orElse super.binaryOperation
  override def customOperators = context.customOperators
}
