package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import org.scalactic.Requirements._
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class LocalServerDialogConnection extends ServerDialogConnection with ExclusiveLock {

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

  def blockingReceiveFirstMessage(): Option[ByteString] = {
    requireState(!firstMessageReceived)
    val r = Await.result(rightPromise.future, Inf)
    firstMessageReceived = true
    r
  }

  def sendAndReceive(data: ByteString): Future[Option[ByteString]] = {
    requireState(firstMessageReceived && !lastMessageSent)
    exclusive {  // Queue up parallel calls and process each after the other
      rightPromise = Promise[Option[ByteString]]()
      leftPromise.success(data)
      Future.fromTry(Await.ready(rightPromise.future, Inf).value.get)
    }
  }

  def blockingSendLastMessage(data: ByteString): Unit =
    exclusive {
      requireState(firstMessageReceived && !lastMessageSent)
      lastMessageSent = true
      leftPromise.success(data)
    }
}
