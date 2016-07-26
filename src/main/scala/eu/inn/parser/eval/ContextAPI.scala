package eu.inn.parser.eval

import eu.inn.binders.value.Value
import eu.inn.parser.EvalEntityNotFound
import eu.inn.parser.ast.Identifier

trait ContextAPI {
  def identifier(identifier: Identifier): Value = unknownIdentifier(identifier)
  def function(identifier: Identifier, arguments: Seq[Value]): Value = unknownFunction(identifier, arguments)

  def unknownIdentifier(identifier: Identifier): Value = {
    throw new EvalEntityNotFound(identifier.toString)
  }
  def unknownFunction(identifier: Identifier, arguments: Seq[Value]): Value = {
    throw new EvalEntityNotFound(identifier.toString)
  }
}
