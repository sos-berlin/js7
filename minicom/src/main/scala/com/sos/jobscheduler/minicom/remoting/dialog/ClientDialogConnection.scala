package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import scala.concurrent.Future

/**
  * A ByteString message connection with strict dialog protocol: send, receive, send, receive.
  *
  * @author Joacim Zschimmer
  */
trait ClientDialogConnection {

  def sendAndReceive(data: ByteString): Future[Option[ByteString]]

  def blockingSendAndReceive(data: ByteString): Option[ByteString]
}

object ClientDialogConnection {

  trait ImplementBlocking {
    this: ClientDialogConnection ⇒

    def blockingSendAndReceive(data: ByteString): Option[ByteString] =
      sendAndReceive(data).awaitInfinite
  }
}
