package eu.inn.parser.eval

import eu.inn.binders.value.{Bool, Lst, Obj, Text, Value}
import eu.inn.parser.ast.Identifier

import scala.util.matching.Regex

trait EvaluatorEngine extends Evaluator {
  val binaryOperators = Map[String,(Value,Value) ⇒ Value] (
    "+" → EvaluatorEngine.addBop,
    "-" → EvaluatorEngine.subBop,
    "*" → EvaluatorEngine.mulBop,
    "/" → EvaluatorEngine.divBop,
    "%" → EvaluatorEngine.remBop,
    "++" → EvaluatorEngine.addaddBop,
    "--" → EvaluatorEngine.subsubBop,
    "or" → EvaluatorEngine.orBop,
    "xor" → EvaluatorEngine.xorBop,
    "and" → EvaluatorEngine.andBop,
    "=" → EvaluatorEngine.eqBop,
    "!=" → EvaluatorEngine.neqBop,
    ">" → EvaluatorEngine.gtBop,
    ">=" → EvaluatorEngine.gteqBop,
    "<" → EvaluatorEngine.ltBop,
    "<=" → EvaluatorEngine.lteqBop,
    "has" → EvaluatorEngine.hasBop,
    "has not" → EvaluatorEngine.hasNotBop,
    "like" → EvaluatorEngine.likeBop,
    "not like" → EvaluatorEngine.notLikeBop,
    "in" → EvaluatorEngine.inBop,
    "not in" → EvaluatorEngine.notInBop
  )

  val unaryOperators = Map[String,Value ⇒ Value] (
    "-" → EvaluatorEngine.minusUop,
    "!" → EvaluatorEngine.invertUop
  )

  val functions = Map[String,Seq[Value] ⇒ Value] (
    "case" → EvaluatorEngine.caseFunc,
    "isEmpty" → EvaluatorEngine.isEmptyFunc,
    "isExists" → EvaluatorEngine.isExistsFunc,
    "length" → EvaluatorEngine.lengthFunc,
    "upper" → EvaluatorEngine.upperFunc,
    "lower" → EvaluatorEngine.lowerFunc,
    "split" → EvaluatorEngine.splitFunc,
    "indexOf" → EvaluatorEngine.indexOfFunc,
    "substr" → EvaluatorEngine.substrFunc,
    "compareIgnoreCase" → EvaluatorEngine.compareIgnoreCaseFunc,
    "apply" → EvaluatorEngine.applyFunc
  )

  override def binaryOperation = {
    case (left: Value, i: Identifier, right: Value) if i.segments.tail.isEmpty && binaryOperators.contains(i.segments.head) ⇒
      binaryOperators(i.segments.head)(left,right)
  }

  override def unaryOperation = {
    case (i: Identifier, arg: Value) if i.segments.tail.isEmpty && binaryOperators.contains(i.segments.head) ⇒
      unaryOperators(i.segments.head)(arg)
  }

  override def function = {
    case (i: Identifier, args: Seq[Value]) if i.segments.tail.isEmpty && functions.contains(i.segments.head) ⇒
      functions(i.segments.head)(args)
  }

  override def identifier: PartialFunction[Identifier, Value] = Map.empty
}

object EvaluatorEngine {
  def addBop(left: Value, right:Value) = left + right
  def subBop(left: Value, right:Value) = left - right
  def mulBop(left: Value, right:Value) = left * right
  def divBop(left: Value, right:Value) = left / right
  def remBop(left: Value, right:Value) = left % right
  def addaddBop(left: Value, right:Value) = left ++ right
  def subsubBop(left: Value, right:Value) = left -- right
  def orBop(left: Value, right:Value) = left | right
  def xorBop(left: Value, right:Value) = left ^ right
  def andBop(left: Value, right:Value) = left & right
  def eqBop(left: Value, right:Value) = left == right
  def neqBop(left: Value, right:Value) = left != right
  def gtBop(left: Value, right:Value) = left > right
  def gteqBop(left: Value, right:Value) = left >= right
  def ltBop(left: Value, right:Value) = left < right
  def lteqBop(left: Value, right:Value) = left <= right

  def hasBop(left: Value, right:Value): Boolean = right match {
    case Lst(seq) ⇒ seq.forall(left.contains)
    case other ⇒ left.contains(other)
  }

  def hasNotBop(left: Value, right:Value): Boolean = !hasBop(left, right)

  def inBop(left: Value, right:Value): Boolean = left match {
      case Lst(seq) ⇒ seq.forall(right.contains)
      case other ⇒ right.contains(other)
  }

  def notInBop(left: Value, right:Value): Boolean = !inBop(left, right)

  def likeBop(left: Value, right:Value): Value = {
    val r = new Regex(right.asString)
    r.findFirstIn(left.asString).isDefined
  }

  def notLikeBop(left: Value, right:Value): Value = !likeBop(left, right)

  def minusUop(arg:Value) = -arg
  def invertUop(arg:Value) = !arg

  def caseFunc(arguments: Seq[Value]): Value = {
    if (arguments.size < 2)
      throw new IllegalArgumentException("`case` expects at least two parameters: index and value")

    val index = arguments.head.asInt
    arguments(index)
  }

  def isEmptyFunc(arguments: Seq[Value]): Value = {
    arguments.forall(_.isEmpty)
  }

  def isExistsFunc(arguments: Seq[Value]): Value = ??? // this is a special case, supported in ASTPlayer

  def lengthFunc(arguments: Seq[Value]): Value = {
    arguments.foldLeft(0l)({
      case (sum, Obj(o)) ⇒ sum + o.size
      case (sum, Lst(l)) ⇒ sum + l.size
      case (sum, Bool(b)) ⇒ sum + (if (b) 1 else 0)
      case (sum, v: Value) ⇒ sum + v.asString.length
    })
  }

  def upperFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 1)
      throw new IllegalArgumentException("`upper` expects an argument")
    arguments.head.asString.toUpperCase
  }

  def lowerFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 1)
      throw new IllegalArgumentException("`lower` expects an argument")
    arguments.head.asString.toLowerCase
  }

  def compareIgnoreCaseFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`compareIgnoreCase` expects two arguments")

    arguments.head.asString.compareToIgnoreCase(arguments.tail.head.asString)
  }

  def applyFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`apply` expects two arguments")

    val index = arguments.tail.head.asInt
    arguments.head match {
      case Lst(l) ⇒ l(index)
      case Obj(m) ⇒ m(m.keys.toVector(index))
      case s ⇒ s.asString(index).toString
    }
  }

  def splitFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`split` expects two arguments")

    val s = arguments.head.asString
    Lst(s.split(arguments.tail.head.asString).map(Text))
  }

  def indexOfFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`indexOf` expects two arguments")

    val s = arguments.head.asString
    s.indexOf(arguments.tail.head.asString)
  }

  def substrFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2 && arguments.size != 3)
      throw new IllegalArgumentException("`substr` expects two or three arguments")

    val s = arguments.head.asString
    val from = arguments.tail.head.asInt
    val to = if (arguments.size != 3) s.length else arguments.tail.tail.head.asInt
    s.substring(from,to)
  }
}