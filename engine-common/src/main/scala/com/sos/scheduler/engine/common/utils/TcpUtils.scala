package com.sos.scheduler.engine.common.utils

/**
 * @author Joacim Zschimmer
 */
object TcpUtils {

  def parseTcpPort(string: String): Int =
    requireTcpPort(
      try BigDecimal(string).toIntExact
      catch { case _: Exception ⇒ throw new IllegalArgumentException(s"Invalid TCP port: $string") })

  def requireTcpPort(port: Int): Int = {
    require(port >= 1 && port <= 0xffff, s"Invalid TCP port: $port")
    port
  }
}
