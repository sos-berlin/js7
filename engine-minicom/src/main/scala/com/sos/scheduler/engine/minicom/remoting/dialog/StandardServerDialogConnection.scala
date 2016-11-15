package com.sos.scheduler.engine.minicom.remoting.dialog

import akka.util.ByteString
import com.sos.scheduler.engine.common.tcp.MessageConnection
import org.scalactic.Requirements._

/**
 * Enforces a ping-pong-style dialog, where each call of `sendMessage` is paired with a call of `receiveMessage`.
 * After a initial message has been received, each pair of send and receive is synchronized.
 * So each `receiveMessage` returns the response to the corresponding `sendMessage`.
 *
 * @author Joacim Zschimmer
 */
final class StandardServerDialogConnection(protected val connection: MessageConnection)
extends ServerDialogConnection with StandardClientDialogConnection {

  private var firstMessageReceived = false
  private var lastMessageSent = false

  def receiveFirstMessage(): Option[ByteString] = {
    requireState(!firstMessageReceived)
    val r = connection.receiveMessage()
    firstMessageReceived = true
    r
  }

  override def sendAndReceive(data: ByteString): Option[ByteString] = {
    requireState(firstMessageReceived && !lastMessageSent)
    super.sendAndReceive(data)
  }

  def sendLastMessage(data: ByteString): Unit = {
    requireState(firstMessageReceived && !lastMessageSent)
    lastMessageSent = true
    exclusive {
      connection.sendMessage(data)
    }
  }
}
