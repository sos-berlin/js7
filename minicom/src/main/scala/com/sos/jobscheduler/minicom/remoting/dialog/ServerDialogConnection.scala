package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import scala.concurrent.Future

/**
  * A ByteString message connection for a server
  * with strict dialog protocol, starting with receive, ending wih send.
  *
  * @author Joacim Zschimmer
  */
trait ServerDialogConnection extends ClientDialogConnection {

  def receiveFirstMessage(): Future[Option[ByteString]]

  def blockingSendLastMessage(data: ByteString): Unit
}
