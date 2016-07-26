package eu.inn.parser

import eu.inn.binders.value.{Obj, Value}
import eu.inn.parser.ast.Expression
import eu.inn.parser.eval._

class HEval(val evaluator: EvaluatorAPI) extends ASTPlayer {

  def this(contextAPI: ContextAPI) = this(new EvaluatorEngine {
    override def context: ContextAPI = contextAPI
  })

  def this(contextValue: Obj) = this(ValueContext(contextValue))
  def this() = this(EmptyContext)

  def eval(expression: Expression): Value = super.play(expression)
  def eval(expression: String): Value = eval(HParser(expression))
}

object HEval {
  def eval(expression: String) = new HEval().eval(expression)
  def eval(expression: String, context: Obj) = new HEval(context).eval(expression)
}