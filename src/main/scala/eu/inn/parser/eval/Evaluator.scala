package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.ast.Identifier

trait Evaluator extends Context {
  def binaryOperation: PartialFunction[(Value, Identifier, Value), Value]
  def unaryOperation: PartialFunction[(Identifier, Value), Value]
}
