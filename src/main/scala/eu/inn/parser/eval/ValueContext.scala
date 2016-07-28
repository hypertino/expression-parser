package eu.inn.parser.eval

import eu.inn.binders.value.{Null, Obj, Value}
import eu.inn.parser.ast.Identifier

case class ValueContext(obj: Obj) extends Context {
  override def identifier = {
    case identifier ⇒ extractValue(obj, identifier.segments)
  }

  override def function: PartialFunction[(Identifier, Seq[Value]), Value] = Map.empty

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
