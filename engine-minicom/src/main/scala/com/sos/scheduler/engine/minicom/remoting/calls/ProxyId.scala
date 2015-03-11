package com.sos.scheduler.engine.minicom.remoting.calls

import org.jetbrains.annotations.TestOnly
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
case class ProxyId(value: Long) {
  @TestOnly private[remoting] def index = (value >> 32).toInt
}

object ProxyId {
  val Null = ProxyId(0)

  def newGenerator(): Iterator[ProxyId] =
    Iterator from 1 map { i ⇒ ProxyId(salt(i)) }  // FIXME Eindeutig zu Proxy-IDs der Gegenstelle / Implementierung wie RemoteTaskId

  private def salt(i: Int): Long = {
    val Bitmask: Long = 0x7fffffffL   // No negatives due to conversion error in com_remote.cxx Input_message::read_int64 (up to v1.8-RC1)
    (i.toLong << 32) | (Random.nextInt().toLong & Bitmask)
  }
}
