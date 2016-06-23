package eu.inn.parser

import fastparse.core.Parsed

trait EvaluationEngine {
  import Parsers._

  def evaluate(expr: String): Any

  def treatAsValue(expr: String): Any = {
    var value: Option[Any] = None
    bool.parse(expr) match {
      case Parsed.Success(_,_) ⇒
        value = Some(expr.toBoolean)
      case _ ⇒
    }

    if (value.isEmpty)
      digitsOnly.parse(expr) match {
        case Parsed.Success(_,_) ⇒
            value = Some(expr.toLong)
        case _ ⇒
      }
    value getOrElse expr.replace("\"", "").replace("'", "")
  }
}

class MapBasedEvaluationEngine(val values: Map[String, Any]) extends EvaluationEngine {

  override def evaluate(expr: String): Any = {
    val segments = expr.split('.')
    if (segments.length < 2) {
      val leafName = segments.head
      values.getOrElse(leafName, treatAsValue(leafName))
    } else {
      val leafName = segments.last
      val lastNotLeafNode = segments.dropRight(1).foldLeft(values) { (node, subNodeName) ⇒
        node.get(subNodeName) match {
          case Some(subNode: Map[String, Any]) ⇒ subNode
          case _ ⇒ Map.empty
        }
      }
      lastNotLeafNode.getOrElse(leafName, None)
    }
  }
}

object MapBasedEvaluationEngine {
  def apply(values: Map[String, Any]): MapBasedEvaluationEngine = {
    new MapBasedEvaluationEngine(values)
  }
}
