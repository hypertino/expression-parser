package eu.inn.parser.eval

import eu.inn.parser.HParser
import org.parboiled2._

import scala.util.{Failure, Success}

class IpParser(val input: ParserInput) extends Parser {
  import CharPredicate.Digit

  private def WhiteSpace = rule { zeroOrMore(HParser.WhiteSpaceChar) }
  private def IpOctet = rule { (1 to 3).times(Digit) }
  private def IpRule = rule { 4.times(IpOctet).separatedBy('.') }
  private def IpAddress = rule { WhiteSpace ~ capture (IpRule) ~ WhiteSpace ~> (i ⇒ i) }

  private def IpRangeByFromTo = rule { capture(IpRule) ~ WhiteSpace ~ '-' ~ WhiteSpace ~ capture(IpRule) ~> ((from, to) ⇒ (from, to)) }
  private def IpRangeBySubnet = rule { (capture(IpRule) ~ WhiteSpace ~ '/' ~ WhiteSpace ~ capture((1 to 2).times(Digit))) ~> ((subnet, mask) ⇒ (subnet, mask)) }

  def IpInputLine = rule { IpAddress ~ EOI }
  def IpRangeInputLine = rule { (IpRangeByFromTo | IpRangeBySubnet) ~ EOI }
}

object IpParser {

  def rangeContainsIp(rangeExpr: String, ipExpr: String): Option[Boolean] = {
    (parseIpRange(rangeExpr), parseIp(ipExpr)) match {
      case (Some(range), Some(ip)) ⇒
        Some(rangeContainsIp(range._1, range._2, ip))
      case _ ⇒
        None
    }
  }

  private def parseIp(expr: String): Option[String] = {
    IpParser(expr).IpInputLine.run() match {
      case Success(ip) ⇒ Some(ip)
      case Failure(_) ⇒ None
    }
  }

  private def parseIpRange(expr: String): Option[(String, String)] = {
    new IpParser(expr).IpRangeInputLine.run() match {
      case Success(range) ⇒
        Some(range)
      case Failure(_) ⇒
        None
    }
  }

  private def apply(expr: String): IpParser = {
    new IpParser(expr)
  }

  private def rangeContainsIp(rangeStart: String, rangeEnd: String, ip: String) = {
    if (rangeEnd.length <= 2)
      rangeBySubnetContainsIp(rangeStart, rangeEnd, ip)
    else
      rangeByAddrContainsIp(rangeStart, rangeEnd, ip)
  }

  private def rangeByAddrContainsIp(rangeStart: String, rangeEnd: String, ip: String) = {
    val from = ipToLong(rangeStart)
    val to = ipToLong(rangeEnd)
    val addr = ipToLong(ip)
    addr >= from && addr <= to
  }

  private def rangeBySubnetContainsIp(subnet: String, mask: String, ip: String) = {
    val lowerBits = (1l << (32 - mask.toInt)) - 1
    val from = ipToLong(subnet)
    val to = from + lowerBits
    val addr = ipToLong(ip)
    addr >= from && addr <= to
  }

  private def ipToLong(ip: String): Long = {
    var ipAddress: Long = 0
    val segments = ip.split('.').reverse
    for (i ← 3 to 0 by -1) {
      ipAddress += segments(i).toLong << (i * 8)
    }
    ipAddress
  }
}

