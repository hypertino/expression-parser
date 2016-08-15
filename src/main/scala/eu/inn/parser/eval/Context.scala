package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.ast.Identifier

trait Context {
  def identifier: PartialFunction[Identifier, Value]
  def function: PartialFunction[(Identifier, Seq[Value]), Value]
  def unaryOperation: PartialFunction[(Identifier, Value), Value]
  def binaryOperation: PartialFunction[(Value, Identifier, Value), Value]
  def customOperators: Seq[String]
}
