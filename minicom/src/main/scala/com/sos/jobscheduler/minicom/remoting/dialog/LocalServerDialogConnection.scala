package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import org.scalactic.Requirements._
import scala.concurrent.{ExecutionContext, Future, Promise, blocking}

/**
  * @author Joacim Zschimmer
  */
final class LocalServerDialogConnection(implicit protected val executionContext: ExecutionContext)
extends ServerDialogConnection with ClientDialogConnection.ImplementBlocking with ExclusiveLock {

  private var firstMessageReceived = false
  private var lastMessageSent = false
  @volatile private var leftPromise = Promise[ByteString]()
  @volatile private var rightPromise = Promise[Option[ByteString]]()

  def leftSendAndReceive(request: ByteString): Future[ByteString] = {
    leftPromise = Promise[ByteString]()
    rightPromise.success(Some(request))
    leftPromise.future
  }

  def leftClose(): Unit = {
    rightPromise.trySuccess(None)
  }

  def receiveFirstMessage(): Future[Option[ByteString]] = {
    requireState(!firstMessageReceived)
    rightPromise.future andThen { case o ⇒
      firstMessageReceived = true
      o
    }
  }

  def sendAndReceive(data: ByteString): Future[Option[ByteString]] = {
    blocking {
      enterExclusively()
    }
    rightPromise = Promise[Option[ByteString]]()
    leftPromise.success(data)
    rightPromise.future andThen { case o ⇒
      leaveExclusively()
      o
    }
  }

  def blockingSendLastMessage(data: ByteString): Unit =
    exclusive {
      requireState(firstMessageReceived && !lastMessageSent)
      lastMessageSent = true
      leftPromise.success(data)
    }
}
