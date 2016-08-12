package eu.inn.parser

import eu.inn.binders.value.{Obj, Value}
import eu.inn.parser.ast.Expression
import eu.inn.parser.eval._
import org.slf4j.LoggerFactory

class HEval(val evaluator: Evaluator) extends ASTPlayer {
  val log = LoggerFactory.getLogger(getClass)

  def this(context: Context) = this(new EvaluatorEngineWithContext(context))
  def this(contextValue: Obj) = this(ValueContext(contextValue))
  def this() = this(EmptyContext)

  def eval(expression: Expression): Value = super.play(expression)
  def eval(expression: String): Value = eval(HParser(expression))
}

object HEval {
  def apply(expression: String) = new HEval().eval(expression)
  def apply(expression: String, contextObject: Obj) = new HEval(contextObject).eval(expression)
  def apply(expression: String, context: Context) = new HEval(context).eval(expression)
}