package eu.inn.parser

import org.scalatest.{FreeSpec, Matchers}

class ExpressionEngineTest extends FreeSpec with Matchers {

  val evalEngine = new MapBasedEvaluationEngine(Map(
    "user" → Map(
      "isDefined" → true,
      "roles" → Seq("dev", "qa", "ops")
    )
  ))
  val exprEngine = new ExpressionEngine(evalEngine)

  "ExpressionEngine" - {
    "evaluate simple expression" in {
      exprEngine.parse("user.isDefined") shouldBe true
      exprEngine.parse("!user.isDefined") shouldBe false
      exprEngine.parse("user.isDefined = true") shouldBe true
      exprEngine.parse("user.isDefined != true") shouldBe false
      exprEngine.parse("""user.roles has "qa" """) shouldBe true
      exprEngine.parse("""user.roles has "admin" """) shouldBe false
    }

    "evaluate complex expression" in {
      exprEngine.parse("""(user.isDefined = true) and (user.roles has "admin")""") shouldBe false
      exprEngine.parse("""(user.isDefined = true) and (user.roles has "qa")""") shouldBe true
      exprEngine.parse("""(user.isDefined) or (user.roles has "qa")""") shouldBe true
      exprEngine.parse("""(!user.isDefined) or (user.roles has "qa")""") shouldBe true
      exprEngine.parse("""(!user.isDefined) or (user.roles has "admin")""") shouldBe false
    }
  }
}
