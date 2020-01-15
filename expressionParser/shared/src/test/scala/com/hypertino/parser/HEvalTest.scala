package com.hypertino.parser

import com.hypertino.binders.value.{False, Lst, Number, Obj, Text, True, Value}
import com.hypertino.parser.ast.Identifier
import com.hypertino.parser.eval.{EvalEntityNotFound, EvalFunctionNotFound, ValueContext}
import org.scalatest.{FreeSpec, Matchers}

class HEvalTest extends FreeSpec with Matchers {
  "HEval" - {
    "math" in {
      val result = HEval("4*5+3*2")
      result shouldBe Number(26)
    }

    "math with variables" in {
      val context = Obj.from("x" → 100)
      val result = HEval("4*x+3*2", context)
      result shouldBe Number(406)
    }

    "math with variables that doesn't exists" in {
      intercept[EvalEntityNotFound]{
        HEval("4*x+3*2")
      }
    }

    "math with inner variables" in {
      val context = Obj.from("data" → Obj.from("x" → 10))
      val result = HEval("4*data.x+1*2", context)
      result shouldBe Number(42)
    }

    "bool logic" in {
      HEval("10 > 3") shouldBe True
      HEval("10 > 3 and 3 > 2") shouldBe True
      HEval("5 > 6") shouldBe False
      HEval("[5,6,7] has 6") shouldBe True
      HEval("[5,6,7] has 8") shouldBe False
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
      HEval("!false") shouldBe True
      HEval("""empty("")""") shouldBe True
      HEval("""!empty("")""") shouldBe False
      HEval("""empty("b")""") shouldBe False
      HEval("""exists(a)""") shouldBe False
    }

    "left only binary op's test" in {
      HEval("""exists(a.b) and a.b""") shouldBe False
      HEval("""true or a.b""") shouldBe True
    }

    "text functions" in {
      HEval("""
        split("a,b,c", ",")
      """) shouldBe Lst.from("a","b","c")

      HEval("""
        index_of("abc","b")
      """) shouldBe Number(1)

      HEval("""
        substr("abc",1)
      """) shouldBe Text("bc")

      HEval("""
        substr("abc",1,2)
      """) shouldBe Text("b")
    }

//    "apply test" in {
//      HEval("""["a","b","c"](1)""") shouldBe Text("b")
//    }

    "custom function test" in {
      val context = new ValueContext(Obj.empty) {
        override def function: PartialFunction[(Identifier, Seq[Value]), Value] = {
          case (Identifier(Seq("pow")), args) ⇒ args.head.toBigDecimal.pow(args.tail.head.toInt)
        }
      }

      HEval("""pow(2,8)""", context) shouldBe Number(256)
    }

    "math with function that doesn't exists" in {
      intercept[EvalFunctionNotFound]{
        HEval("pow(2,8)")
      }
    }

    "string interpolation" in {
      val context = Obj.from("id" → 100500, "name" → "John")
      HEval("s\"user id is: $id\"", context) shouldBe Text("user id is: 100500")
      HEval("s\"user name is: ${name}\"", context) shouldBe Text("user name is: John")
    }

    "parse unix-time functions" in {
      HEval("parse_unix_time('2001-07-04T12:08:56.235-0700')") shouldBe Number(994273736235l)
      HEval("parse_unix_time('2001-07-04','yyyy-MM-dd','+0002')") shouldBe Number(994204800000l)
      HEval("parse_unix_time('04-07-2001','dd-MM-yyyy','+0002')") shouldBe Number(994204800000l)
    }

    "format unix-time functions" in {
      HEval("format_unix_time(994273736235,'','UTC')") shouldBe Text("2001-07-04T19:08:56.235+0000")
      HEval("format_unix_time(994229700000, 'yyyy-MM-dd', 'UTC')") shouldBe Text("2001-07-04")

      HEval("format_unix_time(parse_unix_time('04-07-2001','dd-MM-yyyy', 'UTC') + 24*60*60*1000, 'dd-MM-yyyy', 'UTC')") shouldBe Text("05-07-2001")
    }

    "unix-time (now)" in {
      val ms = System.currentTimeMillis()
      HEval("unix_time()").asInstanceOf[Number].v.toLong should be >= ms
    }
  }
}
