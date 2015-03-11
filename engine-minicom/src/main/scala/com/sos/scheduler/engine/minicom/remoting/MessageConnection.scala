package com.sos.scheduler.engine.minicom.remoting

import java.nio.ByteBuffer

/**
 * @author Joacim Zschimmer
 */
trait MessageConnection {
  def receiveMessage(): Option[ByteBuffer]
  def sendMessage(data: Array[Byte], length: Int): Unit
}
