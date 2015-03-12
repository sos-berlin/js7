package com.sos.scheduler.engine.common.utils

/**
 * @author Joacim Zschimmer
 */
object TcpUtils {

  def parseTcpPort(string: String): Int =
    try {
      val result = BigDecimal(string)
      require(result >= 1 && result <= 0xffff)
      result.toInt
    }
    catch { case _: Exception ⇒ throw new IllegalArgumentException(s"Invalid TCP Port: $string") }
}
