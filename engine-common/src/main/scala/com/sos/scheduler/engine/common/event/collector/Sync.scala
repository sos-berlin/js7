package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.common.time.timer.TimerService.TimeoutFuture
import com.sos.scheduler.engine.data.event._
import java.time.Instant.now
import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
private[collector] final class Sync(timerService: TimerService) {

  @volatile private var promise: Promise[Boolean] = null
  @volatile private var lastEventId = EventId.BeforeFirst

  def onNewEvent(eventId: EventId): Unit =
    synchronized {
      lastEventId = eventId
      if (promise != null) {
        promise.success(true)
        promise = null
      }
    }

  def whenEventIsAvailable(after: EventId, duration: Duration)(implicit ec: ExecutionContext): Future[Boolean] =
    whenEventIsAvailable(after, now + duration)

  def whenEventIsAvailable(after: EventId, until: Instant)(implicit ec: ExecutionContext): Future[Boolean] =
    if (after < lastEventId)
      Future.successful(true)
    else
    if (until <= now)
      Future.successful(false)
    else {
      val future = synchronized {
        if (after < lastEventId)
          Future.successful(true)
        else {
          if (promise == null) {
            promise = Promise[Boolean]()
          }
          promise.future
        }
      }
      if (future.isCompleted)
        future
      else
        future.timeoutAt(until, Success(false), getClass.getName)(timerService, ec)
    }
}
