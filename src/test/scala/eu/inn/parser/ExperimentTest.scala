package eu.inn.parser

import java.math.BigInteger

import eu.inn.binders.value.{False, LstV, Null, Number, ObjV, Text, True, Value}
import fastparse.all._
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import org.scalatest.{FreeSpec, Matchers}

object Parsers {
  val Whitespace = NamedFunction(" \t\r\n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space         = P( CharsWhile(Whitespace).? )
  val digits        = P( CharsWhile(Digits))
  val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = P( "." ~ digits )
  val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val hexNumber = P( IgnoreCase("0x") ~/ hexDigit.rep(1)).!.map(s ⇒ Number(BigDecimal(new BigInteger(s.substring(2), 16))))
  val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(s ⇒ Number(BigDecimal(s)))

  val `null`        = P( "null" ).map(_ => Null)
  val `false`       = P( "false" ).map(_ => False)
  val `true`        = P( "true" ).map(_ => True)

  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  val strChars = P( CharsWhile(StringChars) )
  val string =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Text)

  val array =
    P( "[" ~/ const.rep(sep=",".~/) ~ space ~ "]").map(LstV(_:_*))

  val pair = P( string.map(_.v) ~/ ":" ~/ const )

  val obj =
    P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(ObjV(_:_*))

  val const: P[Value] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | hexNumber | number) ~ space
  )

  val constExpression = const.map(AST.Const)

  val identifierFirstChars = P(CharIn('a' to 'z', 'A' to 'Z') | "_" | "$")
  val identifierChars = identifierFirstChars | digits
  val subIdentifier = P(".") ~ identifierChars.rep(1)
  val identifier = P( identifierFirstChars ~/ (subIdentifier.rep(1) | identifierChars).rep ).!.map(AST.Identifier)

  val unaryOps = P("!") | "-"
  val unaryExpression = P( unaryOps.! ~ expression ).map{case (l,r) ⇒ AST.UnaryOperation(l,r)}

  val binaryOps = P("+") | "-"

  /*val binaryOps = P("&") | "|" | "%" | "*" | "/" | "+" | "-" | "<<" | ">>" | "^" |
    "&&" | "||" | ">" | "<" | ">=" | "<=" | "between" | "=" |
    "in" | P("not" ~ space ~ "in") | "has" | P("has" ~ space ~ "not")*/

  val binaryExpression = P( expression ~ binaryOps.! ~ expression ).map { case(l,o,r) ⇒
    AST.BinaryOperation(o, l, r)
  }

  val expression: P[AST.Expression] = binaryExpression | constExpression | identifier | unaryExpression
}

object AST {
  sealed trait Expression
  case class Identifier(name: String) extends Expression
  case class Const(value: Value) extends Expression
  case class UnaryOperation(name: String, argument: Expression) extends Expression
  case class BinaryOperation(name: String, leftArgument: Expression, rightArgument: Expression) extends Expression
  case class Function(name: String, arguments: Expression) extends Expression
}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name
}

object Helpers extends Matchers{
  implicit class ExtendParsered[T](p: Parsed[T]) {
    def shouldBeSuccess(other: Any) = {
      p shouldBe a[Success[_]]
      p.asInstanceOf[Success[T]].value shouldBe other
    }
  }
}
class ETest extends FreeSpec with Matchers {
  import Helpers._
  import Parsers._
  import AST._

  "testing parsers" - {
    "numbers" in {
      const.parse("1234") shouldBeSuccess Number(1234)
      const.parse("0.123") shouldBeSuccess Number(0.123)
      const.parse("1e3") shouldBeSuccess Number(1e3)
      const.parse("12.8E3") shouldBeSuccess Number(12.8E3)
      const.parse("0xFF") shouldBeSuccess Number(0xFF)
      const.parse("0X12") shouldBeSuccess Number(0x12)
    }

    "bool" in {
      const.parse("true") shouldBeSuccess True
      const.parse("false") shouldBeSuccess False
    }

    "identifier" in {
      identifier.parse("abc") shouldBeSuccess Identifier("abc")
      identifier.parse("abc.xyz") shouldBeSuccess Identifier("abc.xyz")
      identifier.parse("abc.xyz.klm") shouldBeSuccess Identifier("abc.xyz.klm")
    }

    "unary expressions" in {
      expression.parse("!abc") shouldBeSuccess UnaryOperation("!", Identifier("abc"))
      expression.parse("!abc.xyz") shouldBeSuccess UnaryOperation("!", Identifier("abc.xyz"))
      expression.parse("!true") shouldBeSuccess UnaryOperation("!", Const(True))
    }

    "binary expressions" in {
      expression.parse("1+2") shouldBeSuccess BinaryOperation("+", Const(Number(1)), Const(Number(2)))
    }
  }
}
