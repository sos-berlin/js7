package com.sos.scheduler.engine.common.tcp

import akka.util.ByteString

/**
 * @author Joacim Zschimmer
 */
trait BlockingMessageConnection {
  def receiveMessage(): Option[ByteString]
  def sendMessage(data: ByteString): Unit
}
