package com.hypertino.parser

import com.hypertino.binders.value.{Number, Text}
import com.hypertino.binders.{value => bn}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HFormatterTest extends AnyFlatSpec with Matchers {
  import com.hypertino.parser.ast._

  "HFormatter" should "format numbers" in {
    HFormatter(Constant(bn.Number(1234))) shouldBe "1234"
    HFormatter(Constant(bn.Number(0.123))) shouldBe "0.123"
    HFormatter(Constant(bn.Number(1e3))) shouldBe "1000.0"
    HFormatter(Constant(bn.Number(12.8E3))) shouldBe "12800.0"
    HFormatter(Constant(bn.Number(0xFF))) shouldBe "255"
    HFormatter(Constant(bn.Number(0x12))) shouldBe "18"
  }

  it should "format string" in {
    HFormatter(Constant("abc")) shouldBe """"abc""""
    HFormatter(Constant("\"")) shouldBe """"\"""""
    HFormatter(Constant(Text("\t"))) shouldBe "\"\\t\""
    HFormatter(Constant(Text("\n"))) shouldBe "\"\\n\""
  }

  it should "format bool" in {
    HFormatter(Constant(bn.True)) shouldBe "true"
    HFormatter(Constant(bn.False)) shouldBe "false"
  }

  it should "format identifier" in {
    HFormatter(Identifier("abc")) shouldBe "abc"
    HFormatter(Identifier(Seq("abc", "xyz"))) shouldBe "abc.xyz"
    HFormatter(Identifier(Seq("abc", "xyz", "klm"))) shouldBe "abc.xyz.klm"
    HFormatter(Identifier("with space")) shouldBe "`with space`"
    HFormatter(Identifier(Seq("test", "with space"))) shouldBe "test.`with space`"
    HFormatter(Identifier("with`backtick")) shouldBe "`with``backtick`"
  }

  it should "format unary expressions" in {
    HFormatter(UnaryOperation(Identifier("!"), Identifier("abc"))) shouldBe "!abc"
    HFormatter(UnaryOperation(Identifier("!"), Identifier(Seq("abc", "xyz")))) shouldBe "!abc.xyz"
    HFormatter(UnaryOperation(Identifier("!"), Constant(bn.True))) shouldBe "!true"
    HFormatter(UnaryOperation(Identifier("!"), Constant(bn.False))) shouldBe "!false"
    HFormatter(UnaryOperation(Identifier("!"), Function(Identifier("isEmpty"), List(Constant(Text("")))))) shouldBe "!(isEmpty(\"\"))"
  }

  it should "format binary expressions" in {
    HFormatter(BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(2)))) shouldBe "1 + 2"
    HFormatter(BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(-2)))) shouldBe "1 + -2"
    HFormatter(BinaryOperation(
      BinaryOperation(Constant(bn.Number(1)), Identifier("+"), Constant(bn.Number(2))),
      Identifier("-"), Constant(bn.Number(3))
    )) shouldBe "(1 + 2) - 3"

    HFormatter(BinaryOperation(Constant(bn.Number(1)), Identifier("has"), Constant(bn.Number(2)))) shouldBe "1 has 2"
    HFormatter(BinaryOperation(Constant(bn.Number(1)), Identifier("has not"), Constant(bn.Number(2)))) shouldBe "1 has not 2"

    HFormatter(BinaryOperation(Identifier("x"), Identifier("<="), Constant(bn.Number(2)))) shouldBe "x <= 2"
    HFormatter(BinaryOperation(Constant(bn.Number(2)), Identifier(">="), Identifier("y"))) shouldBe "2 >= y"
  }

  it should "format multiple binary expressions" in {
    HFormatter(BinaryOperation(BinaryOperation(Identifier("id"), Identifier(">"), Constant(Text("10"))), Identifier("or"), BinaryOperation(Identifier("x"), Identifier("<"), Constant(Number(5))))) shouldBe "(id > \"10\") or (x < 5)"
  }

  it should "format function" in {
    HFormatter(Function(Identifier("test"),
      Seq.empty
    )) shouldBe "test()"

    HFormatter(Function(Identifier("test"),
      Seq(
        Constant(bn.Number(1)), Constant(bn.Number(2)), Constant(bn.Number(3))
      )
    )) shouldBe "test(1,2,3)"

    HFormatter(Function(Identifier("test"),
      Seq(BinaryOperation(Constant(bn.Number(5)), Identifier("+"), Constant(bn.Number(10))))
    )) shouldBe "test(5 + 10)"
  }

  it should "format string interpolation" in {
    HFormatter(StringInterpolation(Seq(Identifier("abc")))) shouldBe "s\"${abc}\""
    HFormatter(StringInterpolation(Seq(Identifier("abc"), Constant(" "), Identifier("ekl")))) shouldBe "s\"${abc} ${ekl}\""
    HFormatter(StringInterpolation(Seq(Constant("${abc} "), Identifier("ekl")))) shouldBe "s\"$${abc} ${ekl}\""
    HFormatter(StringInterpolation(Seq(Constant("$abc $ekl")))) shouldBe "s\"$$abc $$ekl\""
  }
}
