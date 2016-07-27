package eu.inn.parser

import eu.inn.binders.value.{False, LstV, Number, Obj, ObjV, Text, True, Value}
import eu.inn.binders.{value ⇒ bn}
import eu.inn.parser.ast.Identifier
import eu.inn.parser.eval.{EvalEntityNotFound, ValueContext}
import org.scalatest.{FreeSpec, Matchers}

class HEvalTest extends FreeSpec with Matchers {
  "HEval" - {
    "math" in {
      val result = HEval("4*5+3*2")
      result shouldBe Number(26)
    }

    "math with variables" in {
      val context = ObjV("x" → 100)
      val result = HEval("4*x+3*2", context)
      result shouldBe Number(406)
    }

    "math with variables that doesn't exists" in {
      intercept[EvalEntityNotFound]{
        HEval("4*x+3*2")
      }
    }

    "math with inner variables" in {
      val context = ObjV("data" → ObjV("x" → 10))
      val result = HEval.apply("4*data.x+1*2", context)
      result shouldBe Number(42)
    }

    "bool logic" in {
      HEval("10 > 3") shouldBe True
      HEval("5 > 6") shouldBe False
      HEval("[5,6,7] has 6") shouldBe True
      HEval("[5,6,7] has [6,8]") shouldBe False
      HEval("[5,6,7] has [6,7]") shouldBe True

      HEval("""
        "hello" has "el"
      """) shouldBe True
    }

    "regex like test" in {
      HEval("""
      "abc12" like "[a-c]+\\d+"
      """)

      HEval("""
      "12" not like "[a-c]+\\d+"
      """)
    }

    "functions test" in {
      HEval("""isEmpty("")""") shouldBe True
      HEval("""isEmpty("b")""") shouldBe False
      HEval("""isExists(a)""") shouldBe False
    }

    "text functions" in {
      HEval("""
        split("a,b,c", ",")
      """) shouldBe LstV("a","b","c")

      HEval("""
        indexOf("abc","b")
      """) shouldBe Number(1)

      HEval("""
        substr("abc",1)
      """) shouldBe Text("bc")

      HEval("""
        substr("abc",1,2)
      """) shouldBe Text("b")
    }

    "apply test" in {
      HEval("""["a","b","c"](1)""") shouldBe Text("b")
    }

    "custom function test" in {
      val context = new ValueContext(Obj.empty) {
        override def function: PartialFunction[(Identifier, Seq[Value]), Value] = {
          case (Identifier(Seq("pow")), args) ⇒ args.head.asBigDecimal.pow(args.tail.head.asInt)
        }
      }

      HEval("""pow(2,8)""", context) shouldBe Number(256)
    }
  }
}
