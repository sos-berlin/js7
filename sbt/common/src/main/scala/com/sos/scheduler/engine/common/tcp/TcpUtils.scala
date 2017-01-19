package com.sos.scheduler.engine.common.tcp

/**
 * @author Joacim Zschimmer
 */
object TcpUtils {

  def parseTcpPort(string: String): Int =
    requireTcpPortNumber(
      try BigDecimal(string).toIntExact
      catch { case _: Exception ⇒ throw new IllegalArgumentException(s"Invalid TCP port: $string") })

  def requireTcpPortNumber(port: Int): Int = {
    require(port >= 1 && port <= 0xffff, s"Invalid TCP port: $port")
    port
  }
}
