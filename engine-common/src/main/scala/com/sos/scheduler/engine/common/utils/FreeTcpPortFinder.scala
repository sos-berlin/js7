package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.utils.Randoms._
import java.net.{BindException, ServerSocket}

object FreeTcpPortFinder {

  val StandardTcpPortRange = 10000 until 20000
  val AlternateTcpPortRange = 20000 until 30000   // Für JS1052IT, der viele Ports belegt, um zufällige Überschneidung zu verhindern

  def findRandomFreeTcpPort(ports: Iterable[Int] = StandardTcpPortRange): Int =
    findRandomFreeTcpPorts(1, ports).head

  def findRandomFreeTcpPorts(n: Int, ports: Iterable[Int] = StandardTcpPortRange): List[Int] = {
    val result = (findRandomFreeTcpPortIterator(ports) take n).toList
    if (result.size != n) sys.error(s"Not enough tcp ports available in $ports")
    result
  }

  def findRandomFreeTcpPortIterator(ports: Iterable[Int] = StandardTcpPortRange): Iterator[Int] =
    randomStartInts(ports) filter portIsFree

  private def portIsFree(port: Int) =
    try {
      val backlog = 1
      new ServerSocket(port, backlog).close()
      true
    } catch {
      case _: BindException ⇒ false
    }
}
