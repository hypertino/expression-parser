package eu.inn.parser

import eu.inn.binders.value.Number
import eu.inn.binders.{value ⇒ bn}
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Success, Try}

object Helpers extends Matchers{
  implicit class ExtendParsered[T](p: Try[T]) {
    def shouldBeSuccess(other: Any) = {
      p shouldBe a[Success[_]]
      p.asInstanceOf[Success[T]].value shouldBe other
    }
  }
}
class PBTest extends FreeSpec with Matchers {
  import Helpers._
  import eu.inn.parser.ast._

  "testing parsers pb2" - {
    "numbers" in {
      HParser("1234").Literal.run() shouldBeSuccess bn.Number(1234)
      HParser("0.123").Literal.run() shouldBeSuccess bn.Number(0.123)
      HParser("1e3").Literal.run() shouldBeSuccess bn.Number(1e3)
      HParser("12.8E3").Literal.run() shouldBeSuccess bn.Number(12.8E3)
      HParser("0xFF").Literal.run() shouldBeSuccess bn.Number(0xFF)
      HParser("0X12").Literal.run() shouldBeSuccess bn.Number(0x12)
    }

    "bool" in {
      HParser("true").Literal.run() shouldBeSuccess bn.True
      HParser("false").Literal.run() shouldBeSuccess bn.False
    }

    "identifier" in {
      HParser("abc").Ident.run() shouldBeSuccess Identifier("abc")
      HParser("abc.xyz").Ident.run() shouldBeSuccess Identifier("abc.xyz")
      HParser("abc.xyz.klm").Ident.run() shouldBeSuccess Identifier("abc.xyz.klm")
    }

    "expressions" in {
      HParser("1234").InputLine.run() shouldBeSuccess Const(bn.Number(1234))
      HParser("true").InputLine.run() shouldBeSuccess Const(bn.True)
      HParser("abc.xyz").InputLine.run() shouldBeSuccess Identifier("abc.xyz")
    }

    "unary expressions" in {
      HParser("!abc").InputLine.run() shouldBeSuccess UnaryOperation(Identifier("!"), Identifier("abc"))
      HParser("!abc.xyz").InputLine.run() shouldBeSuccess UnaryOperation(Identifier("!"), Identifier("abc.xyz"))
      HParser("!true").InputLine.run() shouldBeSuccess UnaryOperation(Identifier("!"), Const(bn.True))
    }

    "binary expressions" in {
      HParser("1+2").InputLine.run() shouldBeSuccess BinaryOperation(Const(bn.Number(1)), Identifier("+"), Const(bn.Number(2)))
      HParser("1+-2").InputLine.run() shouldBeSuccess BinaryOperation(Const(bn.Number(1)), Identifier("+"), Const(bn.Number(-2)))
      HParser("1+2-3").InputLine.run() shouldBeSuccess BinaryOperation(
        BinaryOperation(Const(bn.Number(1)), Identifier("+"),Const(bn.Number(2))),
        Identifier("-"), Const(bn.Number(3))
      )

      HParser("1 has 2").InputLine.run() shouldBeSuccess BinaryOperation(Const(bn.Number(1)), Identifier("has"), Const(bn.Number(2)))
      HParser("1 has not 2").InputLine.run() shouldBeSuccess BinaryOperation(Const(bn.Number(1)), Identifier("has not"), Const(bn.Number(2)))
      HParser("1 has \t not 2").InputLine.run() shouldBeSuccess BinaryOperation(Const(bn.Number(1)), Identifier("has not"), Const(bn.Number(2)))
    }

    "binary operator precedence" in {
      HParser("5+10*3").InputLine.run() shouldBeSuccess BinaryOperation(Const(bn.Number(5)), Identifier("+"),
        BinaryOperation(Const(bn.Number(10)), Identifier("*"), Const(bn.Number(3)))
      )
      HParser("5*10+3").InputLine.run() shouldBeSuccess BinaryOperation(
        BinaryOperation(Const(bn.Number(5)), Identifier("*"), Const(bn.Number(10))),
        Identifier("+"), Const(bn.Number(3))
      )
      HParser("5*10 - 3/4").InputLine.run() shouldBeSuccess BinaryOperation(
        BinaryOperation(Const(bn.Number(5)), Identifier("*"), Const(bn.Number(10))),
        Identifier("-"),
        BinaryOperation(Const(bn.Number(3)), Identifier("/"), Const(bn.Number(4)))
      )
    }

    "parens expression" in {
      HParser("(5+10)*3").InputLine.run() shouldBeSuccess BinaryOperation(
        BinaryOperation(Const(bn.Number(5)), Identifier("+"), Const(bn.Number(10))),
        Identifier("*"), Const(bn.Number(3))
      )
    }

    "eval test" in {
      val result = HParser.eval("4*5+3*2")
      result shouldBe Number(26)
    }

    /*"details" in {
      val parser = HParser("1 + 2")
      parser.InputLine.run() match {
        case Success(v) ⇒ println("ok")
        case Failure(e: ParseError) ⇒
          val errorMsg = parser.formatError(e, new ErrorFormatter(showTraces = true))
          println(errorMsg)
      }
    }*/
  }
}
