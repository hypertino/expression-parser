package eu.inn.parser.eval

import eu.inn.binders.value.{Bool, Lst, Null, Number, Obj, Text, Value}
import eu.inn.parser.ast.Identifier

trait EvaluatorEngine extends EvaluatorAPI {
  override def binaryOperation(left: Value, operator: Identifier, right: Value): Value = {
    operator.segments.head match {
      case "+" ⇒ add(left, right)
      case "*" ⇒ mul(left, right)
      case "++" ⇒ concat(left, right)
      case _ ⇒ super.binaryOperation(left, operator, right)
    }
  }

  override def unaryOperation(operator: Identifier, argument: Value): Value = {
    super.unaryOperation(operator, argument)
  }

  def add(left: Value, right: Value): Value = {
    left match {
      case Number(n) if right != Null ⇒ n + right.asBigDecimal
      case Number(n) if right == Null ⇒ Null
      case Text(s) ⇒ s + right.asString
      case Bool(b) ⇒ b && right.asBoolean
      case Lst(seqLeft) ⇒ seqLeft + right
      case o: Obj ⇒ o + right
    }
  }

  def concat(left: Value, right: Value): Value = {
    left match {
      case Lst(seqLeft) ⇒ right match {
        case Lst(seqRight) ⇒ seqLeft ++ seqRight
        case _ ⇒ throw new EvalOperationIsNotApplicable(s"$left ++ $right")
      }
      case _ ⇒ throw new EvalOperationIsNotApplicable(s"$left ++ $right")
    }
  }

  def mul(left: Value, right: Value): Value = {
    left match {
      case Number(n) if right != Null ⇒ n * right.asBigDecimal
      case Number(n) if right == Null ⇒ Null
      case t: Text ⇒ t.asBigDecimal + right.asBigDecimal
      case _ ⇒ throw new EvalOperationIsNotApplicable(s"$left * $right")
    }
  }
}
