package eu.inn.parser.eval

import eu.inn.binders.value.{Bool, Lst, Null, Number, Obj, Text, Value}
import eu.inn.parser.ast.Identifier

/*
 "<=" | ">="
"has not"
"has"
*/

trait EvaluatorEngine extends EvaluatorAPI {
  override def binaryOperation(left: Value, operator: Identifier, right: Value): Value = {
    operator.segments.head match {
      case "+" ⇒ add(left, right)
      case "-" ⇒ sub(left, right)
      case "*" ⇒ mul(left, right)
      case "/" ⇒ div(left, right)
      case "%" ⇒ rem(left, right)
      case "++" ⇒ concat(left, right)
      case "--" ⇒ subtract(left, right)
      case "or" ⇒ or(left, right)
      case "xor" ⇒ xor(left, right)
      case "and" ⇒ and(left, right)
      case "=" ⇒ eqop(left, right)
      case "!=" ⇒ neqop(left, right)
      case ">" ⇒ gt(left, right)
      case "<" ⇒ ls(left, right)
      case _ ⇒ super.binaryOperation(left, operator, right)
    }
  }

  override def unaryOperation(operator: Identifier, argument: Value): Value = {
    super.unaryOperation(operator, argument)
  }


}
