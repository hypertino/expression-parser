package com.hypertino.parser

import com.hypertino.binders.value.{Number, Text}
import com.hypertino.binders.{value ⇒ bn}
import org.scalatest.{FreeSpec, Matchers}

class HParserTest extends FreeSpec with Matchers {
  import com.hypertino.parser.ast._

  "HParser" - {
    "numbers" in {
      HParser("1234").get shouldBe Constant(bn.Number(1234))
      HParser("0.123").get shouldBe Constant(bn.Number(0.123))
      HParser("1e3").get shouldBe Constant(bn.Number(1e3))
      HParser("12.8E3").get shouldBe Constant(bn.Number(12.8E3))
      HParser("0xFF").get shouldBe Constant(bn.Number(0xFF))
      HParser("\n0X12").get shouldBe Constant(bn.Number(0x12))
    }

    "bool" in {
      HParser("true").get shouldBe Constant(bn.True)
      HParser("\nfalse").get shouldBe Constant(bn.False)
    }

    "identifier" in {
      HParser("abc").get shouldBe Identifier("abc")
      HParser("abc.xyz").get shouldBe Identifier(Seq("abc","xyz"))
      HParser("abc.xyz.klm").get shouldBe Identifier(Seq("abc","xyz","klm"))
      HParser("`with space`").get shouldBe Identifier("with space")
      HParser("test.`with space`").get shouldBe Identifier(Seq("test","with space"))
      HParser("`with``backtick`").get shouldBe Identifier("with`backtick")
    }

    "unary expressions" in {
      HParser("!abc").get shouldBe UnaryOperation(Identifier("!"), Identifier("abc"))
      HParser("!abc.xyz").get shouldBe UnaryOperation(Identifier("!"), Identifier(Seq("abc","xyz")))
      HParser("!true").get shouldBe UnaryOperation(Identifier("!"), Constant(bn.True))
      HParser("!(false)").get shouldBe UnaryOperation(Identifier("!"), Constant(bn.False))
      HParser("!isEmpty(\"\")").get shouldBe UnaryOperation(Identifier("!"),Function(Identifier("isEmpty"),List(Constant(Text("")))))
    }

    "binary expressions" in {
      HParser("1+2").get shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(2)))
      HParser("1+-2").get shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(-2)))
      HParser("1+2-3").get shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(1)), Identifier("+"),Constant(bn.Number(2))),
        Identifier("-"), Constant(bn.Number(3))
      )

      HParser("1 has 2").get shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("has"), Constant(bn.Number(2)))
      HParser("1 has not 2").get shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("has not"), Constant(bn.Number(2)))
      HParser("1 has \t not 2").get shouldBe BinaryOperation(Constant(bn.Number(1)), Identifier("has not"), Constant(bn.Number(2)))

      HParser("x <= 2").get shouldBe BinaryOperation(Identifier("x"), Identifier("<="), Constant(bn.Number(2)))
      HParser("2 >= y").get shouldBe BinaryOperation(Constant(bn.Number(2)), Identifier(">="), Identifier("y"))
    }

    "multiple binary expressions" in {
      HParser("id > \"10\" or x < 5").get shouldBe BinaryOperation(BinaryOperation(Identifier("id"),Identifier(">"),Constant(Text("10"))),Identifier("or"),BinaryOperation(Identifier("x"),Identifier("<"),Constant(Number(5))))
    }

    /*"apply precedence" in {
      val parser = new HParser("""!(true)""")
      parser.InputLine.run() match {
        case Failure(e: ParseError) ⇒
          val errorMsg = parser.formatError(e, new ErrorFormatter(showTraces = true, traceCutOff = 320))
          println(errorMsg)
        case other ⇒
          println(other)
      }
    }*/

    "binary operator precedence" in {
      HParser("5+10*3").get shouldBe BinaryOperation(Constant(bn.Number(5)), Identifier("+"),
        BinaryOperation(Constant(bn.Number(10)), Identifier("*"), Constant(bn.Number(3)))
      )
      HParser("5*10+3").get shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(5)), Identifier("*"), Constant(bn.Number(10))),
        Identifier("+"), Constant(bn.Number(3))
      )
      HParser("5*10 - 3/4").get shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(5)), Identifier("*"), Constant(bn.Number(10))),
        Identifier("-"),
        BinaryOperation(Constant(bn.Number(3)), Identifier("/"), Constant(bn.Number(4)))
      )
    }

    "parens expression" in {
      HParser("\n(5+10)*3").get shouldBe BinaryOperation(
        BinaryOperation(Constant(bn.Number(5)), Identifier("+"), Constant(bn.Number(10))),
        Identifier("*"), Constant(bn.Number(3))
      )
    }

    "function" in {
      HParser("test()").get shouldBe Function(Identifier("test"),
        Seq.empty
      )
      HParser("\ntest()\n").get shouldBe Function(Identifier("test"),
        Seq.empty
      )
      HParser("test(1,2,3)").get shouldBe Function(Identifier("test"),
        Seq(
          Constant(bn.Number(1)),Constant(bn.Number(2)),Constant(bn.Number(3))
        )
      )
      HParser("test(5+10)").get shouldBe Function(Identifier("test"),
        Seq(BinaryOperation(Constant(bn.Number(5)), Identifier("+"), Constant(bn.Number(10))))
      )
    }

    "string tests" in {
      HParser("\"\\t\"").get shouldBe Constant(Text("\t"))
      HParser("\"\\u000A\"").get shouldBe Constant(Text("\n"))
    }
  }
}
