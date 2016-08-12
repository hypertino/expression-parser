package eu.inn.parser

import eu.inn.binders.value.{False, LstV, Number, Obj, ObjV, Text, True, Value}
import eu.inn.parser.ast.Identifier
import eu.inn.parser.eval.{EvalEntityNotFound, ValueContext}
import org.scalatest.{FreeSpec, Matchers}

class HEvalTest extends FreeSpec with Matchers {
  "HEval" - {
    "math" in {
      val result = HEval("4*5+3*2")
      result.get shouldBe Number(26)
    }

    "math with variables" in {
      val context = ObjV("x" → 100)
      val result = HEval("4*x+3*2", context)
      result.get shouldBe Number(406)
    }

    "math with variables that doesn't exists" in {
      intercept[EvalEntityNotFound]{
        HEval("4*x+3*2").get
      }
    }

    "math with inner variables" in {
      val context = ObjV("data" → ObjV("x" → 10))
      val result = HEval.apply("4*data.x+1*2", context)
      result.get shouldBe Number(42)
    }

    "bool logic" in {
      HEval("10 > 3").get shouldBe True
      HEval("5 > 6").get shouldBe False
      HEval("[5,6,7] has 6").get shouldBe True
      HEval("[5,6,7] has [6,8]").get shouldBe False
      HEval("[5,6,7] has [6,7]").get shouldBe True
      HEval(""""127.0.0.0/24" has "127.0.0.1"""").get shouldBe True
      HEval(""""127.0.0.0/24" has "126.0.0.1"""").get shouldBe False
      HEval(""""127.0.0.1 - 128.0.0.1" has "127.0.0.1"""").get shouldBe True
      HEval(""""127.0.0.1 - 128.0.0.1" has "126.0.0.1"""").get shouldBe False

      HEval("""
        "hello" has "el"
      """).get shouldBe True
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
      HEval("!false").get shouldBe True
      HEval("""isEmpty("")""").get shouldBe True
      // HEval("""!isEmpty("")""").get shouldBe False //todo: uncomment when fixed
      HEval("""isEmpty("b")""").get shouldBe False
      HEval("""isExists(a)""").get shouldBe False
    }

    "text functions" in {
      HEval("""
        split("a,b,c", ",")
      """).get shouldBe LstV("a","b","c")

      HEval("""
        indexOf("abc","b")
      """).get shouldBe Number(1)

      HEval("""
        substr("abc",1)
      """).get shouldBe Text("bc")

      HEval("""
        substr("abc",1,2)
      """).get shouldBe Text("b")
    }

    "apply test" in {
      HEval("""["a","b","c"](1)""").get shouldBe Text("b")
    }

    "custom function test" in {
      val context = new ValueContext(Obj.empty) {
        override def function: PartialFunction[(Identifier, Seq[Value]), Value] = {
          case (Identifier(Seq("pow")), args) ⇒ args.head.asBigDecimal.pow(args.tail.head.asInt)
        }
      }

      HEval("""pow(2,8)""", context).get shouldBe Number(256)
    }
  }
}
