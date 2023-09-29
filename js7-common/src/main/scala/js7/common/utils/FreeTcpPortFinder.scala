package js7.common.utils

import java.net.{BindException, ServerSocket}
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isMac
import js7.base.web.Uri
import scala.math.abs
import scala.sys.process.*
import scala.util.Random
import scala.util.control.NonFatal

object FreeTcpPortFinder:
  private val logger = Logger[this.type]

  // Do not overlap with ephemeral port range to avoid collision.
  // IANA recommends for ephemeral ports the range 49152 to 65535.
  // Linux may use 32767 to 60999, see  /proc/sys/net/ipv4/ip_local_port_range
  private val firstEphemeralPort: Int =
    if isMac then
      try
        val output = new StringBuilder
        val processLogger = new ProcessLogger:
          def out(s: => String) = output.append(s)

          def err(s: => String) = ()

          def buffer[T](f: => T): T = f
        "sysctl -n net.inet.ip.portrange.first" ! processLogger
        output.toString.takeWhile(_ != '\n').toInt
      catch { case NonFatal(t) =>
        logger.error(s"firstEphemeralPort => $t", t)
        32767
      }
    else
      32767

  private val availablePorts = 20000 to firstEphemeralPort -1
  private val requiredPortCount = 1000

  private val freePortNumberIterator =
    val first = availablePorts.head + abs(Random.nextInt(availablePorts.length - requiredPortCount))
    Iterator.range(first, availablePorts.last).filter(portIsFree)

  def findFreeLocalUri(): Uri =
    Uri("http://localhost:" + findFreeTcpPort())

  def findFreeTcpPort(): Int =
    findFreeTcpPorts(1).head

  def findFreeTcpPorts(n: Int): List[Int] =
    freePortNumberIterator.synchronized:
      val result = freePortNumberIterator.take(n).toList
      if result.length != n then sys.error("Not enough free tcp ports available")
      logger.debug("findFreeTcpPort => " + result.mkString(", "))
      result

  private def portIsFree(port: Int) =
    try
      new ServerSocket(port, /*backlog=*/1).close()
      true
    catch
      case _: BindException =>
        logger.debug(s"findFreeTcpPort => $port is unavailable")
        false
