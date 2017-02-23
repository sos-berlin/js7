package com.sos.scheduler.engine.http.client.heartbeat

import akka.actor.ActorRefFactory
import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService._
import com.sos.scheduler.engine.common.time.timer.{Timer, TimerService}
import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestHeaders._
import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestor._
import com.sos.scheduler.engine.http.client.idempotence.IdempotentRequestor
import java.time.Instant.now
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicReference
import javax.inject.{Inject, Singleton}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.{Future, blocking}
import scala.util.{Failure, Success}
import spray.client.pipelining._
import spray.http.StatusCodes.{Accepted, OK}
import spray.http.{HttpEntity, HttpRequest, HttpResponse}

/**
  * @author Joacim Zschimmer
  *
  * @see http://kamon.io/teamblog/2014/11/02/understanding-spray-client-timeout-settings/
  */
final class HeartbeatRequestor private[http](timing: HttpHeartbeatTiming,
  debug: Debug = new Debug)(
  implicit timerService: TimerService, actorRefFactory: ActorRefFactory)
extends AutoCloseable {

  import actorRefFactory.dispatcher

  private val idempotentRequestor = new IdempotentRequestor(requestTimeout = debug.clientTimeout getOrElse timing.timeout)
  private var _serverHeartbeatCount = 0
  private var _clientHeartbeatCount = 0
  @volatile
  private var closed = false

  def close() = {
    // Interrupt client heartbeat chain
    closed = true
    clientHeartbeat.cancelTimeout()
  }

  def apply(mySendReceive: SendReceive, httpRequest: HttpRequest): Future[HttpResponse] =
    apply(firstRequestTransformer = identity, mySendReceive, httpRequest)

  /**
    * @param firstRequestTransformer only used for the initial request, not used for heartbeat acknowledges
    * @param mySendReceive used for initial request and for heartbeat acknowledges
    * @param request the initial request. Without payload it is used for heartbeat
    * @return the response for the initial request
    */
  private def apply(firstRequestTransformer: RequestTransformer, mySendReceive: SendReceive, request: HttpRequest): Future[HttpResponse] = {
    clientHeartbeat.cancelTimeout()
    val emptyRequest = request withEntity HttpEntity.Empty
    val sentAt = now
    val myRequest = request withHeaders `X-JobScheduler-Heartbeat-Start`(timing) :: request.headers
    val whenResponded = idempotentRequestor.sendAndRetry(firstRequestTransformer ~> mySendReceive, myRequest, requestDuration = timing.period)
      .flatMap(handleResponse(mySendReceive, emptyRequest))
    for (_ ← whenResponded) clientHeartbeat(sentAt, mySendReceive, emptyRequest)
    whenResponded
  }

  private def handleResponse(mySendReceive: SendReceive, emptyRequest: HttpRequest)(httpResponse: HttpResponse): Future[HttpResponse] = {
    if (!debug.heartbeatDelay.isZero) blocking { sleep(debug.heartbeatDelay) }
    heartbeatIdOption(httpResponse) match {
      case Some(heartbeatId) ⇒
        _serverHeartbeatCount += 1
        val heartbeatRequest = emptyRequest withHeaders `X-JobScheduler-Heartbeat-Continue`(heartbeatId, timing) :: emptyRequest.headers
        idempotentRequestor.sendAndRetry(mySendReceive, heartbeatRequest, requestDuration = timing.period) flatMap handleResponse(mySendReceive, emptyRequest)
      case None ⇒
        Future.successful(httpResponse)
    }
  }

  private object clientHeartbeat {
    private val currentClientTimeout = new AtomicReference[Timer[Unit]]
    var throwableOption: Option[Throwable] = None // How to inform C++ JobScheduler about an error ???

    def cancelTimeout(): Unit =
      for (o ← Option(currentClientTimeout.get)) {
        timerService.cancel(o)
        currentClientTimeout.compareAndSet(o, null)
      }

    def apply(lastRequestSentAt: Instant, mySendReceive: SendReceive, emptyRequest: HttpRequest): Unit = {
      if (!closed) {
        val timeoutAt = lastRequestSentAt + timing.period roundDownTo ClientHeartbeatRoundTo max now + ClientHeartbeatMinimumDelay
        val timer = timerService.at(timeoutAt, s"${emptyRequest.uri} client-side heartbeat") onElapsed {
          if (!closed) {
            val heartbeatRequest = emptyRequest withHeaders `X-JobScheduler-Heartbeat`(timing)
            if (debug.suppressed) logger.debug(s"suppressed $heartbeatRequest")
            else {
              _clientHeartbeatCount += 1
              val sentAt = now
              mySendReceive(heartbeatRequest).timeoutAfter(IdempotentRequestor.RetryTimeout, s"${heartbeatRequest.uri} client heartbeat retry") map {
                case HttpResponse(OK, HttpEntity.Empty, _, _) ⇒
                case HttpResponse(OK, entity, _, _) ⇒ sys.error(s"Unexpected heartbeat payload: $entity")
                case HttpResponse(status, entity, _, _) ⇒ sys.error(s"Unexpected heartbeat response: $status" + (if (status.isFailure) s": ${entity.asString take 500}" else ""))
              } recover {
                case _: Timer.ElapsedException ⇒
              } onComplete {
                case Success(()) ⇒ apply(sentAt, mySendReceive, emptyRequest)
                case Failure(t) ⇒ onClientHeartbeatTimeout(t)
              }
            }
          }
        }
        currentClientTimeout.set(timer)
      }
    }

    def onClientHeartbeatTimeout(throwable: Throwable): Unit = {
      logger.error(s"$throwable")
      throwableOption = Some(throwable)
    }
  }

  @TestOnly
  def serverHeartbeatCount = _serverHeartbeatCount

  @TestOnly
  def clientHeartbeatCount = _clientHeartbeatCount

  override def toString = clientHeartbeat.throwableOption mkString ("HeartbeatRequestor(", "", ")")
}

object HeartbeatRequestor {
  private val logger = Logger(getClass)
  private[http] val ClientHeartbeatMinimumDelay = 1.s  // Client-side heartbeat is sent after this delay after last response without new regular request
  private val ClientHeartbeatRoundTo = 1.s

  @ImplementedBy(classOf[StandardFactory])
  trait Factory extends (HttpHeartbeatTiming ⇒ HeartbeatRequestor)

  @Singleton
  final class StandardFactory @Inject private(implicit timerService: TimerService, actorRefFactory: ActorRefFactory, debug: Debug) extends Factory {
    def apply(timing: HttpHeartbeatTiming): HeartbeatRequestor = new HeartbeatRequestor(timing, debug = debug)
  }

  private def heartbeatIdOption(httpResponse: HttpResponse): Option[HeartbeatId] =
    if (httpResponse.status == Accepted && httpResponse.entity.isEmpty)
      httpResponse.headers collectFirst { case HeartbeatResponseHeaders.`X-JobScheduler-Heartbeat`(id) ⇒ id }
    else
      None

  final class HttpRequestTimeoutException(timeout: Duration) extends RuntimeException(s"HTTP request timed out due to http-heartbeat-timeout=${timeout.pretty}")

  @Singleton final class Debug @Inject() () {
    var heartbeatDelay: Duration = 0.s
    var clientTimeout: Option[Duration] = None
    var suppressed = false
  }
}
