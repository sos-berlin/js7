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

  private var firstMessageRead = false
  private val lock = new ReentrantLock

  def receiveFirstMessage(): Option[ByteString] = {
    requireState(!firstMessageRead)
    val r = connection.receiveMessage()
    firstMessageRead = true
    r
  }

  def sendAndReceive(data: ByteString): Option[ByteString] = {
    requireState(firstMessageRead)
    if (lock.isLocked) logger.trace("Waiting for completion of a concurrent connection dialog")
    lock.lock()
    try {
      connection.sendMessage(data)
      connection.receiveMessage()
    }
    finally lock.unlock()
  }
}

object DialogConnection {
  private val logger = Logger(getClass)
}
