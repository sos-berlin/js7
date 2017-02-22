package com.sos.scheduler.engine.minicom.remoting

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.{RichFutureFuture, _}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.minicom.idispatch.IUnknownFactory
import com.sos.scheduler.engine.minicom.remoting.ServerRemoting._
import com.sos.scheduler.engine.minicom.remoting.dialog.ServerDialogConnection
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyIDispatchFactory
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, IUnknown}
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

  def run(): Future[Unit] = {
    def executeAndContinue(messageOption: Option[ByteString]): Future[Unit] =
      messageOption match {
        case Some(message) ⇒
          val response = executeMessage(message)
          if (!end)
            (connection.sendAndReceive(response) map executeAndContinue).flatten
          else
            Future.successful(connection.blockingSendLastMessage(response))
        case None ⇒
          Future.successful(())
      }

    logger.trace("Started")
    val firstRequest = connection.blockingReceiveFirstMessage()
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
