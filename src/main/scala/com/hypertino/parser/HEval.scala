package com.hypertino.parser

import com.hypertino.parser.eval._
import com.hypertino.binders.value.{Obj, Value}
import com.hypertino.parser.ast.Expression
import com.hypertino.parser.eval._

import scala.util.{Failure, Success, Try}

class HEval(val evaluator: Evaluator) extends ASTPlayer {
  def this(context: Context) = this(new EvaluatorEngineWithContext(context))
  def this(contextValue: Obj) = this(ValueContext(contextValue))
  def this() = this(EmptyContext)

  def eval(expression: Expression): Value = super.play(expression)
  def eval(expression: String): Try[Value] = HParser(expression, evaluator.customOperators) match {
    case Success(parsedExpression) ⇒ Success(eval(parsedExpression))
    case Failure(ex) ⇒ Failure(ex)
  }
}

object HEval {
  def apply(expression: String) = new HEval().eval(expression)
  def apply(expression: String, contextObject: Obj) = new HEval(contextObject).eval(expression)
  def apply(expression: String, context: Context) = new HEval(context).eval(expression)
}