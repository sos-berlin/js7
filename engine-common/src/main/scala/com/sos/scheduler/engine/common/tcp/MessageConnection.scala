package com.sos.scheduler.engine.common.tcp

import java.nio.ByteBuffer

/**
 * @author Joacim Zschimmer
 */
trait MessageConnection {
  def receiveMessage(): Option[ByteBuffer]
  def sendMessage(data: Array[Byte], length: Int): Unit
}
