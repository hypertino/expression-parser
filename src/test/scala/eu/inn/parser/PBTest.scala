package eu.inn.parser

import java.math.BigInteger

import eu.inn.binders.{value ⇒ bn}
import org.scalatest.{FreeSpec, Matchers}

import scala.annotation.switch
import org.parboiled2.{CharPredicate, _}

import scala.util.{Failure, Success, Try}

class HParser(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{Digit, Digit19, HexDigit}
  import HParser._

  // the root rule
  def Const = rule { WhiteSpace ~ Value }

  def Object: Rule1[bn.Obj] = rule {
    ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> ((fields: Seq[(String,bn.Value)]) => bn.ObjV(fields :_*))
  }

  def Pair = rule { StringUnwrapped ~ ws(':') ~ Value ~> ((_, _)) }

  def Value: Rule1[bn.Value] = rule {
    run {
      (cursorChar: @switch) match {
        case '"' => String
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => Number
        case '{' => Object
        case '[' => List
        case 't' => True
        case 'f' => False
        case 'n' => Null
        case _ => MISMATCH
      }
    }
  }

  def String = rule { StringUnwrapped ~> bn.Text }

  def StringUnwrapped = rule { '"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString) }

  def Number = rule { HexNumber | DecNumber }

  def HexNumber = rule { ignoreCase("0x") ~ clearSB() ~ capture(oneOrMore(HexDigit)) ~> (s ⇒ bn.Number(BigDecimal(new BigInteger(s, 16)))) ~ WhiteSpace }

  def DecNumber = rule { capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (s ⇒ bn.Number(BigDecimal(s))) ~ WhiteSpace }

  def List = rule { ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (bn.LstV(_ :_*)) }

  def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  def NormalChar = rule { !QuoteBackslash ~ ANY ~ appendSB() }

  def EscapedChar = rule (
    QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  )

  def Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  def Integer = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }

  def Digits = rule { oneOrMore(Digit) }

  def Frac = rule { "." ~ Digits }

  def Exp = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }

  def True = rule { "true" ~ WhiteSpace ~ push(bn.True) }

  def False = rule { "false" ~ WhiteSpace ~ push(bn.False) }

  def Null = rule { "null" ~ WhiteSpace ~ push(bn.Null) }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }

  // identifiers

  def IdentFirstChar = CharPredicate.Alpha ++ CharPredicate("$_")
  def IdentChar = IdentFirstChar ++ CharPredicate.Digit
  def SubIdent = rule { "." ~ IdentFirstChar ~ zeroOrMore(IdentChar) }
  def Ident = rule { capture( IdentFirstChar ~ zeroOrMore(IdentChar) ~ zeroOrMore(SubIdent) ) ~ WhiteSpace ~> AST.Identifier }

  def UnaryOps = rule { capture ("!" | "-") ~ WhiteSpace ~> AST.Identifier }

  def UnaryExpression = rule { UnaryOps ~ Expression ~> AST.UnaryOperation }

  def BinaryOps = rule { capture ("+" | "-") ~ WhiteSpace ~> AST.Identifier }

  def BinaryExpression = rule { SingleExpression ~ BinaryOps ~ Expression ~> AST.BinaryOperation }

  def ConstExpression = rule { Const ~> AST.Const }

  def SingleExpression = rule { ConstExpression | Ident | UnaryExpression}

  def Expression: Rule1[AST.Expression] = rule {
    BinaryExpression | SingleExpression
  }

  def InputLine = rule { Expression ~ EOI }
}

object HParser {
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"

  def apply(input: ParserInput) = new HParser(input)
}

object Helpers2 extends Matchers{
  implicit class ExtendParsered[T](p: Try[T]) {
    def shouldBeSuccess(other: Any) = {
      p shouldBe a[Success[_]]
      p.asInstanceOf[Success[T]].value shouldBe other
    }
  }
}
class PBTest extends FreeSpec with Matchers {
  import Helpers2._

  "testing parsers pb2" - {
    "numbers" in {
      HParser("1234").Const.run() shouldBeSuccess bn.Number(1234)
      HParser("0.123").Const.run() shouldBeSuccess bn.Number(0.123)
      HParser("1e3").Const.run() shouldBeSuccess bn.Number(1e3)
      HParser("12.8E3").Const.run() shouldBeSuccess bn.Number(12.8E3)
      HParser("0xFF").Const.run() shouldBeSuccess bn.Number(0xFF)
      HParser("0X12").Const.run() shouldBeSuccess bn.Number(0x12)
    }

    "bool" in {
      HParser("true").Const.run() shouldBeSuccess bn.True
      HParser("false").Const.run() shouldBeSuccess bn.False
    }

    "identifier" in {
      HParser("abc").Ident.run() shouldBeSuccess AST.Identifier("abc")
      HParser("abc.xyz").Ident.run() shouldBeSuccess AST.Identifier("abc.xyz")
      HParser("abc.xyz.klm").Ident.run() shouldBeSuccess AST.Identifier("abc.xyz.klm")
    }

    "expressions" in {
      HParser("1234").InputLine.run() shouldBeSuccess AST.Const(bn.Number(1234))
      HParser("true").InputLine.run() shouldBeSuccess AST.Const(bn.True)
      HParser("abc.xyz").InputLine.run() shouldBeSuccess AST.Identifier("abc.xyz")
    }

    "unary expressions" in {
      HParser("!abc").InputLine.run() shouldBeSuccess AST.UnaryOperation(AST.Identifier("!"), AST.Identifier("abc"))
      HParser("!abc.xyz").InputLine.run() shouldBeSuccess AST.UnaryOperation(AST.Identifier("!"), AST.Identifier("abc.xyz"))
      HParser("!true").InputLine.run() shouldBeSuccess AST.UnaryOperation(AST.Identifier("!"), AST.Const(bn.True))
    }

    "binary expressions" in {
      HParser("1+2").InputLine.run() shouldBeSuccess AST.BinaryOperation(AST.Const(bn.Number(1)), AST.Identifier("+"), AST.Const(bn.Number(2)))
      HParser("1+-2").InputLine.run() shouldBeSuccess AST.BinaryOperation(AST.Const(bn.Number(1)), AST.Identifier("+"), AST.Const(bn.Number(-2)))
      HParser("1+2-3").InputLine.run() shouldBeSuccess AST.BinaryOperation(AST.Const(bn.Number(1)), AST.Identifier("+"),
        AST.BinaryOperation(AST.Const(bn.Number(2)), AST.Identifier("-"), AST.Const(bn.Number(3)))
      )

      /*
        val parser = HParser("1 + 2")
        parser.InputLine.run() match {
        case Success(v) ⇒ println("ok")
        case Failure(e: ParseError) ⇒
          val errorMsg = parser.formatError(e, new ErrorFormatter(showTraces = true))
          println(errorMsg)
      }*/
    }
  }
}
