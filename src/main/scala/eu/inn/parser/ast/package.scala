package eu.inn.parser

import eu.inn.binders.value.Value

package object ast {
  sealed trait Expression
  case class Identifier(segments: Seq[String]) extends Expression {
    override def toString: String = segments.map { s ⇒
      if (Identifier.isSafeIdentifierSegment(s)) {
        s
      }
      else {
        "`" + s.replace("`","``") + "`"
      }
    } mkString("Identifier(", ".", ")")
  }

  case class Constant(value: Value) extends Expression
  case class UnaryOperation(op: Identifier, argument: Expression) extends Expression
  case class BinaryOperation(leftArgument: Expression, op: Identifier, rightArgument: Expression) extends Expression
  case class Function(name: Identifier, arguments: Seq[Expression]) extends Expression

  object Identifier {
    def isSafeFirstChar(c: Char) = c.isLetter || c == '$' || c == '_'
    def isSafeChar(c: Char) = c.isDigit || isSafeFirstChar(c)
    def isSafeIdentifierSegment(segment: String) = isSafeFirstChar(segment.head) && segment.tail.forall(isSafeChar)

    def parse(s: String): Identifier = {
      new HParser(s).Ident.run().get
    }

    def apply(s: String): Identifier = Identifier(Seq(s))
  }
}

