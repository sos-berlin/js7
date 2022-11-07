package js7.common.utils

import java.net.{BindException, ServerSocket}
import js7.base.log.Logger
import scala.math.abs
import scala.util.Random

object FreeTcpPortFinder
{
  private val logger = Logger(getClass)
  // Do not overlap with ephemeral port range to avoid collision.
  // IANA recommends for ephemeral ports the range 49152 to 65535.
  // Linux may use 32767 to 60999, see  /proc/sys/net/ipv4/ip_local_port_range
  private val availablePorts = 20000 to 32767
  private val requiredPortCount = 5000

  private val freePortNumberIterator = {
    val first = availablePorts.head + abs(Random.nextInt(availablePorts.length - requiredPortCount))
    Iterator.range(first, availablePorts.last).filter(portIsFree)
  }

  def findFreeTcpPort(): Int =
    findFreeTcpPorts(1).head

  def findFreeTcpPorts(n: Int): List[Int] =
    freePortNumberIterator.synchronized {
      val result = freePortNumberIterator.take(n).toList
      if (result.length != n) sys.error("Not enough free tcp ports available")
      logger.debug("findFreeTcpPort => " + result.mkString(", "))
      result
    }

  private def portIsFree(port: Int) =
    try {
      new ServerSocket(port, /*backlog=*/1).close()
      true
    } catch {
      case _: BindException =>
        logger.debug(s"findFreeTcpPort => $port is unavailable")
        false
    }
}
