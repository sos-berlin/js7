package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.scalautil.Logger
import java.net.{BindException, ServerSocket}
import scala.math.abs
import scala.util.Random

object FreeTcpPortFinder
{
  private val logger = Logger(getClass)

  private val freePortNumberIterator = {
    val start = 40000 + abs(Random.nextInt(9000))  // 40000..48999
    Iterator.range(start, 0x10000).filter(portIsFree)
  }

  def findFreeTcpPort(): Int =
    findFreeTcpPorts(1).head

  def findFreeTcpPorts(n: Int): List[Int] =
    synchronized {
      val result = freePortNumberIterator
        .take(n)
        .map { o =>
          logger.debug(s"findFreeTcpPort => $o")
          o
        }
        .toList
      if (result.length != n) sys.error(s"Not enough free tcp ports available")
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
