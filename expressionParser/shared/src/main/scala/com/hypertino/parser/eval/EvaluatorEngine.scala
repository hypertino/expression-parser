package com.hypertino.parser.eval

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.{Date, TimeZone}

import com.hypertino.binders.value._
import com.hypertino.parser.ast.Identifier

import scala.util.matching.Regex

trait EvaluatorEngine extends Evaluator {
  val binaryOperators = Map[String,((Value,Value) => Value)] (
    "+" -> EvaluatorEngine.addBop,
    "-" -> EvaluatorEngine.subBop,
    "*" -> EvaluatorEngine.mulBop,
    "/" -> EvaluatorEngine.divBop,
    "%" -> EvaluatorEngine.remBop,
    "++" -> EvaluatorEngine.addaddBop,
    "--" -> EvaluatorEngine.subsubBop,
    "or" -> EvaluatorEngine.orBop,
    "xor" -> EvaluatorEngine.xorBop,
    "and" -> EvaluatorEngine.andBop,
    "=" -> EvaluatorEngine.eqBop,
    "!=" -> EvaluatorEngine.neqBop,
    ">" -> EvaluatorEngine.gtBop,
    ">=" -> EvaluatorEngine.gteqBop,
    "<" -> EvaluatorEngine.ltBop,
    "<=" -> EvaluatorEngine.lteqBop,
    "has" -> EvaluatorEngine.hasBop,
    "has not" -> EvaluatorEngine.hasNotBop,
    "like" -> EvaluatorEngine.likeBop,
    "not like" -> EvaluatorEngine.notLikeBop
  )

  val binaryOperatorsLeftArgument = Map[String,((Value) => Option[Value])] (
    "or" -> EvaluatorEngine.orLeftBop,
    "and" -> EvaluatorEngine.andLeftBop
  )

  val unaryOperators = Map[String,Value => Value] (
    "-" -> EvaluatorEngine.minusUop,
    "!" -> EvaluatorEngine.invertUop
  )

  val functions = Map[String,Seq[Value] => Value] (
    "case" -> EvaluatorEngine.caseFunc,
    "empty" -> EvaluatorEngine.isEmptyFunc,
    "exists" -> EvaluatorEngine.isExistsFunc,
    "length" -> EvaluatorEngine.lengthFunc,
    "upper" -> EvaluatorEngine.upperFunc,
    "lower" -> EvaluatorEngine.lowerFunc,
    "split" -> EvaluatorEngine.splitFunc,
    "index_of" -> EvaluatorEngine.indexOfFunc,
    "substr" -> EvaluatorEngine.substrFunc,
    "compare_ignoring_case" -> EvaluatorEngine.compareIgnoreCaseFunc,
    "apply" -> EvaluatorEngine.applyFunc,
    "format_unix_time" -> EvaluatorEngine.formatUnixTimeFunc,
    "parse_unix_time" -> EvaluatorEngine.parseUnixTimeFunc,
    "unix_time" -> EvaluatorEngine.unixTimeFunc
  )

  override def binaryOperation = {
    case (left: Value, i: Identifier, right: Value) if i.segments.tail.isEmpty && binaryOperators.contains(i.segments.head) =>
      binaryOperators(i.segments.head)(left,right)
  }

  def binaryOperationLeftArgument: PartialFunction[(Value, Identifier), Option[Value]] = {
    case (left: Value, i: Identifier) if i.segments.tail.isEmpty && binaryOperatorsLeftArgument.contains(i.segments.head) =>
      binaryOperatorsLeftArgument(i.segments.head)(left)
  }

  override def unaryOperation = {
    case (i: Identifier, arg: Value) if i.segments.tail.isEmpty && unaryOperators.contains(i.segments.head) =>
      unaryOperators(i.segments.head)(arg)
  }

  override def function = {
    case (i: Identifier, args: Seq[Value]) if i.segments.tail.isEmpty && functions.contains(i.segments.head) =>
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

  def orLeftBop(left: Value): Option[Value] = if (left.toBoolean) Some(left) else None
  def andLeftBop(left: Value): Option[Value] = if (!left.toBoolean) Some(left) else None

  def hasBop(left: Value, right:Value): Boolean = right match {
    case Lst(seq) => seq.forall(left.contains)
    case other => left.contains(other)
  }

  def hasNotBop(left: Value, right:Value): Boolean = !hasBop(left, right)

  def likeBop(left: Value, right:Value): Value = {
    val r = new Regex(right.toString)
    r.findFirstIn(left.toString).isDefined
  }

  def notLikeBop(left: Value, right:Value): Value = !likeBop(left, right)

  def minusUop(arg:Value) = -arg
  def invertUop(arg:Value) = !arg

  def caseFunc(arguments: Seq[Value]): Value = {
    if (arguments.size < 2)
      throw new IllegalArgumentException("`case` expects at least two parameters: index and value")

    val index = arguments.head.toInt
    arguments(index)
  }

  def isEmptyFunc(arguments: Seq[Value]): Value = {
    arguments.forall(_.isEmpty)
  }

  def isExistsFunc(arguments: Seq[Value]): Value = ??? // this is a special case, supported in ASTPlayer

  def lengthFunc(arguments: Seq[Value]): Value = {
    arguments.foldLeft(0L)({
      case (sum, Obj(o)) => sum + o.size
      case (sum, Lst(l)) => sum + l.size
      case (sum, Bool(b)) => sum + (if (b) 1 else 0)
      case (sum, v: Value) => sum + v.toString.length
    })
  }

  def upperFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 1)
      throw new IllegalArgumentException("`upper` expects an argument")
    arguments.head.toString.toUpperCase
  }

  def lowerFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 1)
      throw new IllegalArgumentException("`lower` expects an argument")
    arguments.head.toString.toLowerCase
  }

  def compareIgnoreCaseFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`compare_ignoring_case` expects two arguments")

    arguments.head.toString.compareToIgnoreCase(arguments.tail.head.toString)
  }

  def applyFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`apply` expects two arguments")

    val index = arguments.tail.head.toInt
    arguments.head match {
      case Lst(l) => l(index)
      case Obj(m) => m(m.keys.toVector(index))
      case s => s.toString.charAt(index).toString
    }
  }

  def splitFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`split` expects two arguments")

    val s = arguments.head.toString
    Lst(s.split(arguments.tail.head.toString).map(Text))
  }

  def indexOfFunc(arguments: Seq[Value]): Value = {
    if (arguments.size != 2)
      throw new IllegalArgumentException("`index_of` expects two arguments")

    val s = arguments.head.toString
    s.indexOf(arguments.tail.head.toString)
  }

  def substrFunc(arguments: Seq[Value]): Value = {
    if (arguments.size < 2 || arguments.size > 3)
      throw new IllegalArgumentException("`substr` expects two or three arguments")

    val s = arguments.head.toString
    val from = arguments.tail.head.toInt
    val to = if (arguments.size != 3) s.length else arguments.tail.tail.head.toInt
    s.substring(from, to)
  }

  protected def isoDateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  lazy val defaultDateFormat = new SimpleDateFormat(isoDateFormat)

  def parseUnixTimeFunc(arguments: Seq[Value]): Value = {
    if (arguments.size < 1 || arguments.size > 3)
      throw new IllegalArgumentException("`parse_unix_time` expects one - three arguments")

    dateFormatter(arguments).parse(arguments.head.toString).toInstant.toEpochMilli
  }

  def formatUnixTimeFunc(arguments: Seq[Value]): Value = {
    if (arguments.size < 1 || arguments.size > 3)
      throw new IllegalArgumentException("`format_unix_time` expects one - three arguments")

    dateFormatter(arguments).format(Date.from(Instant.ofEpochMilli(arguments.head.toLong)))
  }

  def unixTimeFunc(arguments: Seq[Value]): Value = {
    if (arguments.nonEmpty)
      throw new IllegalArgumentException("`unix_time` doesn't accepts arguments")

    Instant.now().toEpochMilli
  }

  protected def dateFormatter(arguments: Seq[Value]): SimpleDateFormat = {
    if(arguments.size > 1) {
      val fmt = if (arguments.tail.head.nonEmpty) arguments.tail.head.toString else isoDateFormat
      val df = new SimpleDateFormat(fmt)
      if(arguments.size > 2) {
        df.setTimeZone(TimeZone.getTimeZone(arguments.tail.tail.head.toString))
      }
      df
    }
    else
      defaultDateFormat
  }
}