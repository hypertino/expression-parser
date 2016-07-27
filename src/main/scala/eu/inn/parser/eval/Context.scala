package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.ast.Identifier

trait Context {
  def identifier: PartialFunction[Identifier, Value]
  def function: PartialFunction[(Identifier, Seq[Value]), Value]
}
