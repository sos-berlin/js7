package com.sos.scheduler.engine.minicom.remoting.dialog

import akka.util.ByteString
import com.sos.scheduler.engine.common.tcp.MessageConnection

/**
  * @author Joacim Zschimmer
  */
trait StandardClientDialogConnection
extends ClientDialogConnection with ExclusiveLock {

  protected def connection: MessageConnection

  def sendAndReceive(data: ByteString): Option[ByteString] = {
    exclusive {
      connection.sendMessage(data)
      connection.receiveMessage()
    }
  }
}
