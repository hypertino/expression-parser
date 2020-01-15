package com.hypertino.parser

import com.hypertino.binders.value.{Number, Text}
import com.hypertino.binders.{value => bn}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HParserTest extends AnyFreeSpec with Matchers {
  import com.hypertino.parser.ast._

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
      HParser("!(false)") shouldBe UnaryOperation(Identifier("!"), Constant(bn.False))
      HParser("!isEmpty(\"\")") shouldBe UnaryOperation(Identifier("!"),Function(Identifier("isEmpty"),List(Constant(Text("")))))
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

      HParser("x <= 2") shouldBe BinaryOperation(Identifier("x"), Identifier("<="), Constant(bn.Number(2)))
      HParser("2 >= y") shouldBe BinaryOperation(Constant(bn.Number(2)), Identifier(">="), Identifier("y"))
    }

    "multiple binary expressions" in {
      HParser("id > \"10\" or x < 5") shouldBe BinaryOperation(BinaryOperation(Identifier("id"),Identifier(">"),Constant(Text("10"))),Identifier("or"),BinaryOperation(Identifier("x"),Identifier("<"),Constant(Number(5))))
    }

    /*"apply precedence" in {
      val parser = new HParser("""!(true)""")
      parser.InputLine.run() match {
        case Failure(e: ParseError) =>
          val errorMsg = parser.formatError(e, new ErrorFormatter(showTraces = true, traceCutOff = 320))
          println(errorMsg)
        case other =>
          println(other)
      }
    }*/

    "unary + binary operator precedence" in {
      HParser("!a or b") shouldBe BinaryOperation(
        UnaryOperation(Identifier("!"), Identifier("a")),
        Identifier("or"),
        Identifier("b")
      )

      HParser("!isEmpty(a) or b") shouldBe BinaryOperation(
        UnaryOperation(Identifier("!"), Function(Identifier("isEmpty"),Seq(Identifier("a")))),
        Identifier("or"),
        Identifier("b")
      )
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
      HParser("""'abc'""") shouldBe Constant(Text("abc"))
      HParser("\"\\u000A\"") shouldBe Constant(Text("\n"))
    }

    "string interpolation" in {
      HParser("s\"${abc}\"") shouldBe StringInterpolation(Seq(Identifier("abc")))
      HParser("s\"${abc} ${ekl}\"") shouldBe StringInterpolation(Seq(Identifier("abc"), Constant(" "), Identifier("ekl")))
      HParser("s\"$${abc} ${ekl}\"") shouldBe StringInterpolation(Seq(Constant("${abc} "), Identifier("ekl")))

      HParser("s\"$abc\"") shouldBe StringInterpolation(Seq(Identifier("abc")))
      HParser("s\"$abc hey $ekl\"") shouldBe StringInterpolation(Seq(Identifier("abc"), Constant(" hey "), Identifier("ekl")))
      HParser("s\"$$abc $ekl\"") shouldBe StringInterpolation(Seq(Constant("$abc "), Identifier("ekl")))
      HParser("s\"$$abc $$ekl\"") shouldBe StringInterpolation(Seq(Constant("$abc $ekl")))
    }
  }
}
