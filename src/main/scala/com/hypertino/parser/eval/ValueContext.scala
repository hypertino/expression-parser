package com.hypertino.parser.eval

import com.hypertino.binders.value.{Null, Obj, Value}
import com.hypertino.parser.ast.Identifier

case class ValueContext(obj: Obj) extends Context {
  override def identifier = {
    case identifier ⇒ extractValue(obj, identifier.segments)
  }

  override def function: PartialFunction[(Identifier, Seq[Value]), Value] = Map.empty
  override def unaryOperation: PartialFunction[(Identifier, Value), Value] = Map.empty
  override def binaryOperation: PartialFunction[(Value, Identifier, Value), Value] = Map.empty
  override def customOperators = Seq.empty[String]

  private def extractValue(o: Obj, path: Seq[String]): Value = {
    if (path.tail.isEmpty) {
      o.v.get(path.head) match {
        case Some(v) ⇒ v
        case None ⇒ Null
      }
    }
    else {
      o.v(path.head) match {
        case child: Obj ⇒
          extractValue(child, path.tail)
        case _ ⇒
          Null
      }
    }
  }
}
