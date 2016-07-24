package eu.inn.parser

import org.scalatest.{FreeSpec, Matchers}

class ExpressionEngineTest extends FreeSpec with Matchers {

  val evalEngine = new MapBasedEvaluationEngine(Map(
    "user" → Map(
      "isDefined" → true,
      "bankAccounts" → Seq(15, 26, Int.MaxValue + 100),
      "roles" → Seq("dev", "qa", "ops"),
      "name" → "12345"
    )
  ))
  val exprEngine = new ExpressionEngine(evalEngine)

  "ExpressionEngine" - {
    "simple expressions" in {
      exprEngine.evaluatePredicate("user.isDefined") shouldBe true
      exprEngine.evaluatePredicate("user.name = 12345") shouldBe true
      exprEngine.evaluatePredicate("!user.isDefined") shouldBe false
      exprEngine.evaluatePredicate("user.isDefined = true") shouldBe true
      exprEngine.evaluatePredicate("user.isDefined != true") shouldBe false
      exprEngine.evaluatePredicate("""user.roles has 'qa' """) shouldBe true
      exprEngine.evaluatePredicate("""user.roles has "admin" """) shouldBe false
      exprEngine.evaluatePredicate("""user.roles has not "admin" """) shouldBe true
      exprEngine.evaluatePredicate("""user.bankAccounts has 26 """) shouldBe true
      exprEngine.evaluatePredicate("""user.bankAccounts has not 111111111111111111 """) shouldBe true
      exprEngine.evaluatePredicate("""user.notExistingField has someValue """) shouldBe false
    }

    "complex expressions" in {
      exprEngine.evaluatePredicate("""(user.isDefined = true) and (user.roles has "admin")""") shouldBe false
      exprEngine.evaluatePredicate("""(user.isDefined = true) and (user.roles has "qa")""") shouldBe true
      exprEngine.evaluatePredicate("""(user.isDefined) or (user.roles has "qa")""") shouldBe true
      exprEngine.evaluatePredicate("""(!user.isDefined) or (user.roles has "qa")""") shouldBe true
      exprEngine.evaluatePredicate("""(!user.isDefined) or (user.roles has "admin")""") shouldBe false
    }

    "ip and ip range expressions" in {
      exprEngine.evaluatePredicate("""10.10.10.10 in (10.10.0.0 - 10.10.20.20)""") shouldBe true
      exprEngine.evaluatePredicate("""10.10.10.10 in 10.10.0.0 - 10.10.9.9""") shouldBe false
      exprEngine.evaluatePredicate("""10.10.10.10 not in 10.10.0.0 - 10.10.20.20""") shouldBe false
      exprEngine.evaluatePredicate("""10.10.10.10 not in 10.10.0.0 - 10.10.9.9""") shouldBe true
      exprEngine.evaluatePredicate("""'qa' in user.roles""") shouldBe true
      exprEngine.evaluatePredicate("""admin not in user.roles""") shouldBe true
    }
  }
}
