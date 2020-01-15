package com.hypertino.parser

import com.hypertino.binders.value.{Obj, Value}
import com.hypertino.parser.ast.Expression
import com.hypertino.parser.eval._

class HEval(val evaluator: Evaluator) extends ASTPlayer {
  def this(context: Context) = this(new EvaluatorEngineWithContext(context))
  def this(contextValue: Obj) = this(ValueContext(contextValue))
  def this() = this(EmptyContext)

  def eval(expression: Expression): Value = super.play(expression)
  def eval(expression: String): Value = eval(HParser(expression, evaluator.customOperators))
}

object HEval {
  def apply(expression: String) = new HEval().eval(expression)
  def apply(expression: String, contextObject: Obj) = new HEval(contextObject).eval(expression)
  def apply(expression: String, context: Context) = new HEval(context).eval(expression)
}