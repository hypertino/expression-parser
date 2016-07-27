package eu.inn.parser

import eu.inn.binders.value.Text
import eu.inn.binders.{value ⇒ bn}
import org.scalatest.{FreeSpec, Matchers}

class HParserTest extends FreeSpec with Matchers {
  import eu.inn.parser.ast._

  "HParser" - {
    "numbers" in {
      HParser("1234") shouldBe Constant(bn.Number(1234))
      HParser("0.123") shouldBe Constant(bn.Number(0.123))
      HParser("1e3") shouldBe Constant(bn.Number(1e3))
      HParser("12.8E3") shouldBe Constant(bn.Number(12.8E3))
      HParser("0xFF") shouldBe Constant(bn.Number(0xFF))
      HParser("\n0X12") shouldBe Constant(bn.Number(0x12))
    }

    "bool" in {
      HParser("true") shouldBe Constant(bn.True)
      HParser("\nfalse") shouldBe Constant(bn.False)
    }

    "identifier" in {
      HParser("abc") shouldBe Identifier("abc")
      HParser("abc.xyz") shouldBe Identifier(Seq("abc","xyz"))
      HParser("abc.xyz.klm") shouldBe Identifier(Seq("abc","xyz","klm"))
      HParser("`with space`") shouldBe Identifier("with space")
      HParser("test.`with space`") shouldBe Identifier(Seq("test","with space"))
      HParser("`with``backtick`") shouldBe Identifier("with`backtick")
    }

    "unary expressions" in {
      HParser("!abc") shouldBe UnaryOperation(Identifier("!"), Identifier("abc"))
      HParser("!abc.xyz") shouldBe UnaryOperation(Identifier("!"), Identifier(Seq("abc","xyz")))
      HParser("!true") shouldBe UnaryOperation(Identifier("!"), Constant(bn.True))
    }

    "binary expressions" in {
      HParser("1+2") shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(2)))
      HParser("1+-2") shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(-2)))
      HParser("1+2-3") shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(1)), Identifier("+"),Constant(bn.Number(2))),
        Identifier("-"), Constant(bn.Number(3))
      )

      HParser("1 has 2") shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("has"), Constant(bn.Number(2)))
      HParser("1 has not 2") shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("has not"), Constant(bn.Number(2)))
      HParser("1 has \t not 2") shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("has not"), Constant(bn.Number(2)))
    }

    "binary operator precedence" in {
      HParser("5+10*3") shouldBe BinaryOperation(Constant(bn.Number(5)), Identifier("+"),
        BinaryOperation(Constant(bn.Number(10)), Identifier("*"), Constant(bn.Number(3)))
      )
      HParser("5*10+3") shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(5)), Identifier("*"), Constant(bn.Number(10))),
        Identifier("+"), Constant(bn.Number(3))
      )
      HParser("5*10 - 3/4") shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(5)), Identifier("*"), Constant(bn.Number(10))),
        Identifier("-"),
        BinaryOperation(Constant(bn.Number(3)), Identifier("/"), Constant(bn.Number(4)))
      )
    }

    "parens expression" in {
      HParser("\n(5+10)*3") shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(5)), Identifier("+"), Constant(bn.Number(10))),
        Identifier("*"), Constant(bn.Number(3))
      )
    }

    "function" in {
      HParser("test()") shouldBe Function(Identifier("test"),
        Seq.empty
      )
      HParser("\ntest()\n") shouldBe Function(Identifier("test"),
        Seq.empty
      )
      HParser("test(1,2,3)") shouldBe Function(Identifier("test"),
        Seq(
          Constant(bn.Number(1)),Constant(bn.Number(2)),Constant(bn.Number(3))
        )
      )
      HParser("test(5+10)") shouldBe Function(Identifier("test"),
        Seq(BinaryOperation(Constant(bn.Number(5)), Identifier("+"), Constant(bn.Number(10))))
      )
    }

    "string tests" in {
      HParser("\"\\t\"") shouldBe Constant(Text("\t"))
      HParser("\"\\u000A\"") shouldBe Constant(Text("\n"))
    }
  }
}
