package com.sos.scheduler.engine.minicom.remoting.dialog

import akka.util.ByteString

/**
  * A ByteString message connection for a server
  * with strict dialog protocol, starting with receive, ending wih send.
  *
  * @author Joacim Zschimmer
  */
trait ServerDialogConnection extends ClientDialogConnection {

  def receiveFirstMessage(): Option[ByteString]

  def sendLastMessage(data: ByteString): Unit
}
