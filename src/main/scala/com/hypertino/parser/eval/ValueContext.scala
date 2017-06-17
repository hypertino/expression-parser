package com.hypertino.parser.eval

import com.hypertino.binders.value.{Null, Obj, Text, Value}
import com.hypertino.parser.ast.Identifier

case class ValueContext(obj: Obj) extends Context {
  override def identifier = {
    case identifier ⇒ obj(identifier.segments.map(Text))
  }

  override def function: PartialFunction[(Identifier, Seq[Value]), Value] = Map.empty
  override def unaryOperation: PartialFunction[(Identifier, Value), Value] = Map.empty
  override def binaryOperation: PartialFunction[(Value, Identifier, Value), Value] = Map.empty
  override def customOperators = Seq.empty[String]
}
