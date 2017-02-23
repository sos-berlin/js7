package com.sos.scheduler.engine.http.server.idempotence

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.timer.{Timer, TimerService}
import com.sos.scheduler.engine.http.client.idempotence.IdempotentHeaders.`X-JobScheduler-Request-ID`
import com.sos.scheduler.engine.http.client.idempotence.RequestId
import com.sos.scheduler.engine.http.server.idempotence.Idempotence._
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicReference
import org.jetbrains.annotations.TestOnly
import scala.concurrent._
import spray.http.StatusCodes.BadRequest
import spray.http._
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
final class Idempotence(implicit timerService: TimerService) {

  private val eatRequestId = new RequestId.Eater
  private val pendingOperation = new AtomicReference[Operation]

  /**
    * Registers a new idempotent request.
    * stealKnownRequest should have been called before.
    * @param body is only executed with the first request
    */
  def apply(body: ⇒ Future[HttpResponse])(implicit actorRefFactory: ActorRefFactory): Route = {
    import actorRefFactory.dispatcher
    headerValueByName(`X-JobScheduler-Request-ID`.name) { case `X-JobScheduler-Request-ID`.Value(id, lifetimeOption) ⇒
      requestUri { uri ⇒
        complete {
          apply(id, lifetimeOption, uri)(body)
        }
      }
    } ~ {
      // No RequestID, not idempotent
      complete { body }
    }
  }

  private def apply(id: RequestId, lifetimeOption: Option[Duration], uri: Uri)(body: ⇒ Future[HttpResponse])(implicit ec: ExecutionContext): Future[HttpResponse] = {
    val newPromise = Promise[HttpResponse]()
    val newOperation = Operation(id, uri, newPromise.future)
    if (eatRequestId(id)) {
      pendingOperation.getAndSet(newOperation) match {
        case null ⇒
        case oldOperation ⇒ for (timer ← Option(oldOperation.lifetimeTimer.get)) timerService.cancel(timer)
      }
      logger.trace(s"$uri new $id")
      body onComplete newPromise.complete
      for (lifetime ← lifetimeOption) {
        newOperation.lifetimeTimer set
          timerService.delay(lifetime, s"$uri $id lifetime").onElapsed {
            pendingOperation.compareAndSet(newOperation, null)  // Release memory of maybe big HttpResponse
          }
      }
      newOperation.future
    } else {
      val known = pendingOperation.get
      if (known != null && known.id == id) {
        if (uri == known.uri) {
          logger.debug(s"$uri Duplicate HTTP request for " + (if (known.future.isCompleted) "completed" else "outstanding") + s" $id of ${known.instant}")
          known.future
        } else
          Future.successful(HttpResponse(BadRequest, s"Duplicate HTTP request does not match URI"))
      } else {
        val expectedId = eatRequestId.expectedId  // Maybe changed since
        var msg: String = null
        if (id < expectedId) {
          msg = s"HTTP request with expired (duplicate) $id is ignored"
          logger.debug(s"$uri $msg, now expecting $expectedId")
        } else {
          msg = s"HTTP request with unexpected $id"
          logger.warn(s"$uri $msg, now expecting $expectedId")
        }
        Future.successful(HttpResponse(BadRequest, msg))
      }
    }
  }

  @TestOnly
  private[server] def pendingRequestIds: Option[RequestId] = Option(pendingOperation.get) map { _.id }
}

object Idempotence {
  private val logger = Logger(getClass)

  private final case class Operation(id: RequestId, uri: Uri, future: Future[HttpResponse]) {
    val lifetimeTimer = new AtomicReference[Timer[Unit]]
    val instant = Instant.now
  }
}
