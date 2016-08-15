package eu.inn.parser.eval
import eu.inn.binders.value.Value
import eu.inn.parser.ast.Identifier

object EmptyContext extends Context {
  override def identifier: PartialFunction[Identifier, Value] = Map.empty
  override def function: PartialFunction[(Identifier, Seq[Value]), Value] = Map.empty
  override def unaryOperation: PartialFunction[(Identifier, Value), Value] = Map.empty
  override def binaryOperation: PartialFunction[(Value, Identifier, Value), Value] = Map.empty
  override def customOperators = Seq.empty[String]
}
