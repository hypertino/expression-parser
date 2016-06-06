package eu.inn.parser

trait EvaluationEngine {
  def evaluate(expr: String): Any

  def treatAsValue(expr: String): Any = {
    if (expr == "true") true
    else if (expr == "false") false
    else expr.replace("\"", "").replace("'", "")
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
          case Some(value) ⇒ throw new IllegalArgumentException(s"Field '$subNodeName' of expression '$expr' has no child fields")
          case None ⇒ throw new IllegalArgumentException(s"Cannot find value of field '$subNodeName' of expression '$expr'")
        }
      }
      lastNotLeafNode.getOrElse(leafName, throw new IllegalArgumentException(s"Cannot find value of field '$leafName' of expression '$expr'"))
    }
  }
}
