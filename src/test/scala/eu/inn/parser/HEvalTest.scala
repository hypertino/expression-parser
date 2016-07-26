package eu.inn.parser

import eu.inn.binders.value.{Number, ObjV, Text}
import eu.inn.binders.{value ⇒ bn}
import org.scalatest.{FreeSpec, Matchers}

class HEvalTest extends FreeSpec with Matchers {
  "HEval" - {
    "math" in {
      val result = HEval.eval("4*5+3*2")
      result shouldBe Number(26)
    }

    "math with variables" in {
      val context = ObjV("x" → 100)
      val result = HEval.eval("4*x+3*2", context)
      result shouldBe Number(406)
    }

    "math with variables that doesn't exists" in {
      intercept[EvalEntityNotFound]{
        HEval.eval("4*x+3*2")
      }
    }

    "math with inner variables" in {
      val context = ObjV("data" → ObjV("x" → 10))
      val result = HEval.eval("4*data.x+1*2", context)
      result shouldBe Number(42)
    }
  }
}
