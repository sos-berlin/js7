package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import com.sos.jobscheduler.common.tcp.BlockingMessageConnection
import scala.concurrent.{ExecutionContext, Future, blocking}

/**
  * @author Joacim Zschimmer
  */
trait StandardClientDialogConnection
extends ClientDialogConnection with ExclusiveLock {

  protected def connection: BlockingMessageConnection
  protected implicit def executionContext: ExecutionContext

  def sendAndReceive(data: ByteString): Future[Option[ByteString]] =
    Future {
      blocking {
        exclusive {
          connection.sendMessage(data)
          connection.receiveMessage()
        }
      }
    }
}
