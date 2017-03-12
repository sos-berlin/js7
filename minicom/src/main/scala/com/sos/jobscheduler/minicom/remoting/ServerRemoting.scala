package com.sos.jobscheduler.minicom.remoting

import akka.util.ByteString
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.minicom.idispatch.IUnknownFactory
import com.sos.jobscheduler.minicom.remoting.ServerRemoting._
import com.sos.jobscheduler.minicom.remoting.dialog.ServerDialogConnection
import com.sos.jobscheduler.minicom.remoting.proxy.ProxyIDispatchFactory
import com.sos.jobscheduler.minicom.types.{CLSID, IID, IUnknown}
import java.time.{Duration, Instant}
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class ServerRemoting(
  protected val connection: ServerDialogConnection,
  protected val name: String,
  protected val iUnknownFactories: immutable.Iterable[IUnknownFactory] = Nil,
  protected val proxyIDispatchFactories: immutable.Iterable[ProxyIDispatchFactory] = Nil,
  returnAfterReleaseOf: IUnknown ⇒ Boolean = _ ⇒ false,
  keepaliveDurationOption: Option[Duration] = None)
  (implicit protected val executionContext: ExecutionContext)
extends Remoting
{
  private var end = false

  protected def onReleased(iUnknown: IUnknown): Unit = {
    end = returnAfterReleaseOf(iUnknown)
  }

  def run(): Future[Completed] = {
    def executeAndContinue(messageOption: Option[ByteString]): Future[Completed] =
      messageOption match {
        case Some(message) ⇒
          val response = executeMessage(message)
          if (!end)
            (connection.sendAndReceive(response) map executeAndContinue).flatten
          else {
            connection.blockingSendLastMessage(response)
            Future.successful(Completed)
          }
        case None ⇒
          Future.successful(Completed)
      }

    logger.trace("Started")
    (for (firstRequest ← connection.receiveFirstMessage()) yield {
      val result = keepaliveDurationOption match {
        case Some(keepaliveDuration) ⇒
          withKeepaliveThread(1.s max keepaliveDuration) {
            executeAndContinue(firstRequest)
          }
        case None ⇒
          executeAndContinue(firstRequest)
      }
      result onComplete { o ⇒
        logger.trace(s"Ended $o")
      }
      result
    }).flatten
  }

  private def withKeepaliveThread[A](duration: Duration)(body: ⇒ A): A = {
    val keepaliveThread = new KeepaliveThread(1.s max duration)
    keepaliveThread.start()
    try body
    finally {
      keepaliveThread.interrupt()
      blocking {
        keepaliveThread.join()
      }
    }
  }

  private class KeepaliveThread(keepaliveDuration: Duration) extends Thread {
    setName("Remoting.Keepalive")

    override def run() =
      try {
        var t = Instant.now()
        while (true) {
          t += keepaliveDuration
          if (t > Instant.now()) sleepUntil(t)
          else t = Instant.now()
          sendReceiveKeepalive() await 2 * keepaliveDuration
        }
      } catch {
        case _: InterruptedException ⇒
        case NonFatal(t) ⇒ logger.error(t.toString)
      }
  }
}

object ServerRemoting {
  private type CreateIUnknownByCLSID = (CLSID, IID) ⇒ IUnknown
  private val logger = Logger(getClass)
}
