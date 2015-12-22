package com.sos.scheduler.engine.minicom.remoting

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.MessageConnection
import com.sos.scheduler.engine.minicom.remoting.DialogConnection._
import java.util.concurrent.locks.ReentrantLock
import org.scalactic.Requirements._

/**
 * Enforces a ping-pong-style dialog, where each call of `sendMessage` is paired with a call of `receiveMessage`.
 * After a initial message has been received, each pair of send and receive is synchronized.
 * So each `receiveMessage` returns the response to the corresponding `sendMessage`.
 *
 * @author Joacim Zschimmer
 */
final class DialogConnection(connection: MessageConnection) {

  private var firstMessageReceived = false
  private var lastMessageSent = false
  private val lock = new ReentrantLock

  def receiveFirstMessage(): Option[ByteString] = {
    requireState(!firstMessageReceived)
    val r = connection.receiveMessage()
    firstMessageReceived = true
    r
  }

  def sendAndReceive(data: ByteString): Option[ByteString] = {
    requireState(firstMessageReceived && !lastMessageSent)
    exclusive {
      connection.sendMessage(data)
      connection.receiveMessage()
    }
  }

  def sendLastMessage(data: ByteString) = {
    requireState(firstMessageReceived && !lastMessageSent)
    lastMessageSent = true
    exclusive {
      connection.sendMessage(data)
    }
  }

  private def exclusive[A](body: ⇒ A): A = {
    if (lock.isLocked) logger.trace("Waiting for completion of a concurrent connection dialog")
    lock.lock()
    try body
    finally lock.unlock()
  }
}

object DialogConnection {
  private val logger = Logger(getClass)
}
