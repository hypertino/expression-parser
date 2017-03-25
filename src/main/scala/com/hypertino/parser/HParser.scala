package com.hypertino.parser

import java.math.BigInteger

import com.hypertino.binders.{value ⇒ bn}
import com.hypertino.parser.ast._
import org.parboiled2.{CharPredicate, Parser, ParserInput, StringBuilding, _}

import scala.annotation.switch
import scala.util.Try

class HParser(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{Digit, Digit19, HexDigit}
  import HParser._

  def customOperators: Vector[Rule1[Identifier]] = Vector.empty

  def Literal = rule { WhiteSpace ~ Value }

  def Object: Rule1[bn.Obj] = rule {
    ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> ((fields: Seq[(String,bn.Value)]) ⇒ bn.Obj.from(fields :_*))
  }

  def Pair = rule { StringUnwrapped ~ ws(':') ~ Value ~> ((_, _)) }

  def Value: Rule1[bn.Value] = rule {
    run {
      (cursorChar: @switch) match {
        case '"' ⇒ String
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' ⇒ Number
        case '{' ⇒ Object
        case '[' ⇒ List
        case 't' ⇒ True
        case 'f' ⇒ False
        case 'n' ⇒ Null
        case _ ⇒ MISMATCH
      }
    }
  }

  def String = rule { StringUnwrapped ~> bn.Text }

  def StringUnwrapped = rule { '"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString) }

  def Number = rule { HexNumber | DecNumber }

  def HexNumber = rule { ignoreCase("0x") ~ capture(oneOrMore(HexDigit)) ~> (s ⇒ bn.Number(BigDecimal(new BigInteger(s, 16)))) ~ WhiteSpace }

  def DecNumber = rule { capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (s ⇒ bn.Number(BigDecimal(s))) ~ WhiteSpace }

  def List = rule { ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (bn.Lst.from(_ :_*)) }

  def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  def NormalChar = rule { !QuoteBackslash ~ ANY ~ appendSB() }

  def EscapedChar = rule (
    QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code ⇒ sb.append(code.asInstanceOf[Char]); () }
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

  // Backtick escaped string, like `abc dfg` or `abc``dfg`
  def TicCharacters = rule { zeroOrMore( (!'`' ~ ANY ~ appendSB()) | ("``" ~ appendSB('`'))) }
  def TickString = rule { '`' ~ clearSB() ~ TicCharacters ~ ws('`') ~ push(sb.toString) }

  // identifiers

  def IdentFirstChar = CharPredicate.Alpha ++ CharPredicate("$_")
  def IdentChar = IdentFirstChar ++ CharPredicate.Digit
  def IdentFirstSegment = rule { capture( IdentFirstChar ~ zeroOrMore(IdentChar)) }
  def IdentFirstSegmentTic: Rule1[String] = rule { TickString }
  def IdentSegmentTic: Rule1[String] = rule { '.' ~ WhiteSpace ~ TickString }
  def IdentSegment = rule { "." ~ WhiteSpace ~ capture (IdentFirstChar ~ zeroOrMore(IdentChar)) }
  def Ident = rule { (IdentFirstSegmentTic | IdentFirstSegment) ~ zeroOrMore(IdentSegmentTic | IdentSegment) ~ WhiteSpace ~> ConstructIdentifier _ }

  def ConstructIdentifier(firstSegment: String, subSegments: Seq[String]) = Identifier(Seq(firstSegment) ++ subSegments)

  def FuncArgs = rule { Expression ~ zeroOrMore(',' ~ Expression) ~> (Seq(_) ++ _)}
  def Func = rule { Ident ~ '(' ~ WhiteSpace ~ optional (FuncArgs) ~ ')' ~ WhiteSpace ~> {(i:Identifier,e:Option[Seq[Expression]]) ⇒ {com.hypertino.parser.ast.Function(i,e.getOrElse(Seq.empty))}}}

  def UnaryOps = rule { capture ( CharPredicate("!-") ) ~ WhiteSpace ~> OpIdentifier _ }
  def OpIdentifier(name: String) = Identifier(name)

  def UnaryExpression = rule { UnaryOps ~ Expression ~> UnaryOperation }

  // sorted by precedence
  def BinaryOps = Vector(
    rule { capture("or") ~> OpIdentifier _ },
    rule { capture("xor") ~> OpIdentifier _ },
    rule { capture("and") ~> OpIdentifier _ },
    rule { capture("=" | "!=") ~> OpIdentifier _ },
    rule { capture("<=" | "<" | ">=" | ">") ~> OpIdentifier _ },
    rule {
      { capture("has" ~ oneOrMore(WhiteSpaceChar) ~ "not") ~> (_ ⇒ OpIdentifier("has not")) } |
      { capture("has") ~> OpIdentifier _ } |
      { capture("not" ~ oneOrMore(WhiteSpaceChar) ~ "like") ~> (_ ⇒ OpIdentifier("not like")) } |
      { capture("like") ~> OpIdentifier _ }
    }

  ) ++ customOperators ++ Vector(
    rule { capture(CharPredicate("+-") | "++" | "--") ~> OpIdentifier _ },
    rule { capture(CharPredicate("*/%")) ~> OpIdentifier _ }
  )

  def binaryOpsSize = 7 + customOperators.length

  def BinaryExpression(index: Int): Rule1[Expression] = {
    if (index > binaryOpsSize)
      SingleExpression
    else rule {
      BinaryExpression(index + 1) ~ zeroOrMore(
        WhiteSpace ~ BinaryOps(index) ~ WhiteSpace ~ BinaryExpression(index + 1) ~> BinaryOperation
      )
    }
  }

  def ConstExpression = rule { Literal ~> Constant }

  def ParensExpression = rule { '(' ~ Expression ~ ')' ~ WhiteSpace }

  //def ApplyExpression = rule { (ConstExpression | Func | UnaryExpression | ParensExpression) ~ '(' ~ Expression ~ ')' ~> ApplyFunction _ }

  //def ApplyFunction(left: Expression, right: Expression): Expression = Function(Identifier("apply"), Seq(left,right))

  def SingleExpression: Rule1[Expression] = rule { WhiteSpace ~ (/*ApplyExpression | */ConstExpression | Func | Ident | UnaryExpression | ParensExpression) ~ WhiteSpace }

  def Expression: Rule1[Expression] = BinaryExpression(0)

  def InputLine = rule { Expression ~ EOI }
}

object HParser {
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"

  def apply(input: ParserInput): Try[Expression] = new HParser(input).InputLine.run()

  def apply(input: ParserInput, operators: Seq[String]): Try[Expression] = new HParser(input) {
    override def customOperators = {
      operators.foldLeft(Vector.newBuilder[Rule1[Identifier]]) { (ops, op) ⇒
        val opByWhiteSpaces = op.split(" ")
        var isFirst = true
        val operationRule = opByWhiteSpaces.foldLeft(WhiteSpace) { (currentRule, segment) ⇒
          if (isFirst) {
            isFirst = false
            rule { currentRule ~ segment }
          } else {
            rule { currentRule ~ oneOrMore(WhiteSpaceChar) ~ segment }
          }
        }
        ops += rule { capture(operationRule) ~ WhiteSpace ~> ((_:String) ⇒ Identifier(op)) }
      }.result()
    }
  }.InputLine.run()
}