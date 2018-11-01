package com.sos.jobscheduler.base.utils

/**
  * @author Joacim Zschimmer
  */
object DecimalPrefixes
{
  private val PrefixToFactor = Map(
    'k' → 1000,
    'M' → (1000*1000),
    'G' → (1000*1000*1000))

  def toInt(string: String): Int =
    if (string.nonEmpty && string.last.isLetter) {
      val prefix = string.last
      val factor = PrefixToFactor.getOrElse(prefix,
        throw new IllegalArgumentException(s"Unknown SI-Prefix: '$prefix', expected: ${PrefixToFactor.keys.mkString(", ")}"))
      val number = string.take(string.length - 1).toInt
      Math.multiplyExact(number, factor)
    } else
      string.toInt
}
