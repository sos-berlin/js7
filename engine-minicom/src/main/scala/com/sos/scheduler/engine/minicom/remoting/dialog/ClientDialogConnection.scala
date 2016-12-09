package com.sos.scheduler.engine.minicom.remoting.dialog

import akka.util.ByteString
import scala.concurrent.Future

/**
  * A ByteString message connection with strict dialog protocol: send, receive, send, receive.
  *
  * @author Joacim Zschimmer
  */
trait ClientDialogConnection {

  def sendAndReceive(data: ByteString): Future[Option[ByteString]]
}
