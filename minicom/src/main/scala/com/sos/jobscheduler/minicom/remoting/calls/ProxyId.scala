package com.sos.scheduler.engine.minicom.remoting.calls

import org.jetbrains.annotations.TestOnly
import scala.util.Random

/**
 * Like C++ Object_id in com_remote.cxx.
 *
 * @author Joacim Zschimmer
 */
final case class ProxyId(value: Long) {
  @TestOnly private[remoting] def index = value.toInt

  override def toString = f"ProxyId(${(value >> 32).toInt}%08X.${value.toInt}%08X)"
}

object ProxyId {
  val Null = ProxyId(0)
  private val StartNumber = 0x40000001  // Should be different from client side (C++ com_remote.cxx)

  def newGenerator(): Iterator[ProxyId] =
    Iterator from StartNumber map { i ⇒ ProxyId(salt(i)) }   // Start number

  private def salt(i: Int): Long = {
    val bitmask: Long = 0x7fffffffL   // No negatives due to conversion error in com_remote.cxx Input_message::read_int64 (up to v1.8-RC1)
    (Random.nextInt().toLong << 32) | (i.toLong & bitmask)
  }
}
