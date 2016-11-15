package com.sos.scheduler.engine.minicom.remoting.dialog

import akka.util.ByteString

/**
  * A ByteString message connection with strict dialog protocol: send, receive, send, receive.
  *
  * @author Joacim Zschimmer
  */
trait ClientDialogConnection {

  def sendAndReceive(data: ByteString): Option[ByteString]
}
