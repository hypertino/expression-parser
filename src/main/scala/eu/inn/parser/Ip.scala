package eu.inn.parser

import fastparse.all._
import fastparse.core.Parsed

object Ip {
  val digits = P(CharIn('0' to '9').rep(min = 1, max = 4).!)
  val ip = P(digits.rep(sep = ".", min = 4, max = 4).!)
  val singleIp = P(ip.! ~ End)

  def ipToLong(ip: String): Long = {
    var ipAddress: Long = 0
    val segments = ip.split('.').reverse
    for (i ← 3 to 0 by -1) {
      ipAddress += segments(i).toLong << (i * 8)
    }
    ipAddress
  }

  def isIp(value: String): Boolean = {
    singleIp.parse(value) match {
      case Parsed.Success(_,_) ⇒ true
      case _ ⇒ false
    }
  }
}

case class IpRange(from: Long, to: Long) {
  def contains(ip: String): Boolean = {
    val ipLong = Ip.ipToLong(ip)
    ipLong >= from && ipLong <= to
  }
}

object IpRange {
  import Ip.ip
  val ipRange = P(ip.rep.! ~ " ".? ~ "-" ~ " ".? ~ ip.rep.!)

  def apply(from: String, to: String): IpRange = {
    IpRange(Ip.ipToLong(from), Ip.ipToLong(to))
  }

  def parse(value: String): Option[IpRange] = {
    ipRange.parse(value) match {
      case Parsed.Success((start, end),_) ⇒
        Some(IpRange(start, end))
      case ot ⇒
        None
    }
  }
}
