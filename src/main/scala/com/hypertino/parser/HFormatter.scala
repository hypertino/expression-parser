package com.hypertino.parser

import com.hypertino.binders.value.{Bool, Lst, Null, Number, Obj, Text, Value}
import com.hypertino.parser.ast._
import com.hypertino.parser.eval.ASTPlayer

trait HFormatter extends ASTPlayer {
  override def evaluator = throw new RuntimeException("HFormatter.evaluator shouldn't be called")

  override def play(rootExpression: Expression): Value = {
    val sb = new StringBuilder
    format(rootExpression, sb)
    Text(sb.toString())
  }

  private def format(e: Expression, sb: StringBuilder): Unit = {
    e match {
      case Constant(value) ⇒
        appendEscaped(value, sb)

      case Identifier(segments) ⇒
        val it = segments.toIterator
        while (it.hasNext) {
          val s = it.next()
          if (Identifier.isSafeIdentifierSegment(s)) {
            sb.append(s)
          }
          else {
            sb.append("`" + s.replace("`","``") + "`")
          }
          if (it.hasNext) {
            sb.append(".")
          }
        }

      case UnaryOperation(operation, argument) ⇒
        sb.append(operation.segments.head)
        formatAndWrap(argument, sb)

      case BinaryOperation(left, operation, right) ⇒
        formatAndWrap(left, sb)
        sb.append(" ")
        sb.append(operation.segments.head)
        sb.append(" ")
        formatAndWrap(right, sb)

      case Function(functionIdentifier, arguments) ⇒
        format(functionIdentifier, sb)
        sb.append("(")
        val it = arguments.toIterator
        while (it.hasNext) {
          format(it.next(), sb)
          if (it.hasNext) {
            sb.append(",")
          }
        }
        sb.append(")")

      case StringInterpolation(arguments) ⇒
        sb.append("s\"")
        arguments.foreach {
          case Constant(Text(s)) ⇒
            sb.append(s.replace("$", "$$"))

          case other ⇒
            sb.append("${")
            format(other, sb)
            sb.append("}")
        }
        sb.append("\"")
    }
  }

  private def formatAndWrap(e: Expression, sb: StringBuilder): Unit = {
    e match {
      case _: Constant | _: Identifier | _: StringInterpolation ⇒
        format(e, sb)

      case other ⇒
        sb.append("(")
        format(e, sb)
        sb.append(")")
    }
  }

  private def appendEscaped(v: Value, sb: StringBuilder): Unit = {
    v match {
      case Text(s) ⇒
        sb.append('"')
        s.foreach {
          case '"' ⇒ sb.append("\\\"")
          case '\\' ⇒ sb.append("\\")
          case '\b' ⇒ sb.append("\\b")
          case '\f' ⇒ sb.append("\\f")
          case '\n' ⇒ sb.append("\\n")
          case '\r' ⇒ sb.append("\\r")
          case '\t' ⇒ sb.append("\\t")
          case c if Character.isISOControl(c) ⇒
            sb.append("\\u")
            sb.append(c.toHexString)
          case c ⇒ sb.append(c)
        }
        sb.append('"')

      case Number(n) ⇒
        sb.append(n.toString)

      case Bool(b) ⇒
        sb.append(b.toString)

      case Null ⇒
        sb.append("null")

      case Obj(els) ⇒
        sb.append("{")
        val it = els.toIterator
        while (it.hasNext) {
          val pair = it.next()
          appendEscaped(Text(pair._1), sb)
          sb.append(":")
          appendEscaped(pair._2, sb)
          if (it.hasNext) {
            sb.append(",")
          }
        }
        sb.append("}")

      case Lst(els) ⇒
        sb.append("[")
        val it = els.toIterator
        while (it.hasNext) {
          appendEscaped(it.next(), sb)
          if (it.hasNext) {
            sb.append(",")
          }
        }
        sb.append("]")
    }
  }
}

object HFormatter extends HFormatter {
  def apply(expression: Expression): String = play(expression).toString
}