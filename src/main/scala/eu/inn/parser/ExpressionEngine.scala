package eu.inn.parser

import fastparse.all._

class ExpressionEngine(evaluationEngine: EvaluationEngine) {

  import ExpressionEngine._

  def evaluatePredicate(predicate: String): Boolean = {
    var result: Option[Boolean] = None
    aggregationFunctions.foreach { function ⇒
      if (result.isEmpty)
        function.parser.parse(predicate) match {
          case Parsed.Success((left, right), _) ⇒
            result = Some(function.op(evaluatePredicate(left), evaluatePredicate(right)))
          case Parsed.Failure(_, _, _) ⇒ // ignore
        }
    }
    result getOrElse evaluateSimplePredicate(predicate)
  }

  def evaluateSimplePredicate(predicate: String): Boolean = {
    var result: Option[Boolean] = None
    simpleFunctions.foreach { function ⇒
      if (result.isEmpty)
        function.parser.parse(predicate) match {
          case Parsed.Success((left, right), _) ⇒
            result = Some(function.op(evaluate(left), evaluate(right)))
          case Parsed.Failure(_, _, _) ⇒ // ignore
        }
    }
    result match {
      case Some(value) ⇒
        value
      case None ⇒
        evaluate(predicate) match {
          case boolResult: Boolean ⇒ boolResult
          case _ ⇒ throw new IllegalArgumentException(s"Value of predicate '$predicate' is not of Boolean type, please check it and correct")
        }
    }
  }

  def evaluate(expr: String): Any = {
    Parsers.not.parse(expr) match {
      case Parsed.Success(parsedExpr, _) ⇒
        !evaluate(parsedExpr).asInstanceOf[Boolean]
      case Parsed.Failure(_, _, _) ⇒
        if (Ip.isIp(expr))
          expr
        else
          IpRange.parse(expr) match {
            case Some(ipRange) ⇒
              ipRange
            case None ⇒
              evaluationEngine.evaluate(expr)
          }
    }
  }
}

object ExpressionEngine {

  import Parsers._

  val orFunction = BooleanFunction(or, (a, b) ⇒ a.asInstanceOf[Boolean] || b.asInstanceOf[Boolean])
  val andFunction = BooleanFunction(and, (a, b) ⇒ a.asInstanceOf[Boolean] && b.asInstanceOf[Boolean])

  val eqFunction = BooleanFunction(Parsers.eq, (a, b) ⇒ {
    a == b || a.toString == b.toString
  })
  val nEqFunction = BooleanFunction(nEq, (a, b) ⇒ {
    !eqFunction.op(a, b)
  })

  val hasFunction = BooleanFunction(has, (a, b) ⇒ {
    a match {
      case seq: Seq[Any] ⇒ seq.contains(b)
      case set: Set[Any] ⇒ set.contains(b)
      case map: Map[Any, Any] ⇒ map.contains(b)
      case _ ⇒ false
    }
  })
  val hasNotFunction = BooleanFunction(hasNot, (a, b) ⇒ {
    !hasFunction.op(a, b)
  })

  val inFunction = BooleanFunction(in, (a, b) ⇒ {
    (a, b) match {
      case (ip: String, ipRange: IpRange) ⇒
        val ip = a.asInstanceOf[String]
        val ipRange = b.asInstanceOf[IpRange]
        ipRange.contains(ip)
      case _ ⇒
        b match {
          case seq: Seq[Any] ⇒ seq.contains(a)
          case set: Set[Any] ⇒ set.contains(a)
          case map: Map[Any, Any] ⇒ map.contains(a)
          case _ ⇒ false
        }
    }
  })
  val notInFunction = BooleanFunction(notIn, (a, b) ⇒ !inFunction.op(a, b))

  val aggregationFunctions = Seq(andFunction, orFunction)
  val simpleFunctions = Seq(nEqFunction, eqFunction, hasNotFunction, hasFunction, inFunction, notInFunction)

  def apply(evaluationEngine: EvaluationEngine): ExpressionEngine = {
    new ExpressionEngine(evaluationEngine)
  }
}

object Parsers {
  val bool = P("true" | "false")
  val digits = P(CharIn('0' to '9').rep)
  val digitsOnly = P(CharIn('0' to '9').rep ~ End)
  val phrase = CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "!", ".", "'", "\"", "=", "-", " ")
  val wordWithDots = CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "!", ".", "'", "\"", "-")

  val or = P("(" ~ phrase.rep.! ~ ")" ~ " or " ~ AnyChar.rep.!)
  val and = P("(" ~ phrase.rep.! ~ ")" ~ " and " ~ AnyChar.rep.!)
  val eq = P("(".? ~ wordWithDots.rep.! ~ " ".? ~ "=" ~ " ".? ~ wordWithDots.rep.! ~ ")".?)
  val nEq = P("(".? ~ wordWithDots.rep.! ~ " ".? ~ "!=" ~ " ".? ~ wordWithDots.rep.! ~ ")".?)
  val has = P("(".? ~ wordWithDots.rep.! ~ " has " ~ wordWithDots.rep.! ~ ")".?)
  val hasNot = P("(".? ~ wordWithDots.rep.! ~ " has not " ~ wordWithDots.rep.! ~ ")".?)
  val in = P("(".? ~ wordWithDots.rep.! ~ " in " ~ "(".? ~ phrase.rep.! ~ ")".?)
  val notIn = P("(".? ~ wordWithDots.rep.! ~ " not in " ~ "(".? ~ phrase.rep.! ~ ")".rep.?)
  val not = P("(".? ~ "!" ~ wordWithDots.rep.! ~ ")".?)

  val ipOctet = P(digits.rep(min = 1, max = 4).!)
  val ip = P(ipOctet.rep(sep = ".", min = 4, max = 4).!)
  val singleIp = P(ip.! ~ End)
  val ipRange = P(ip.rep.! ~ " ".? ~ "-" ~ " ".? ~ ip.rep.!)
}

case class BooleanFunction(parser: Parser[(String, String)], op: (Any, Any) ⇒ Boolean)