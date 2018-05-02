package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.time.timer.TimerService.TimeoutFuture
import com.sos.jobscheduler.data.event._
import java.time.Instant.now
import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
private[event] final class Sync(initialLastEventId: EventId, timerService: TimerService) {

  @volatile private var promise: Promise[Boolean] = null
  @volatile private var lastEventId = initialLastEventId

  def onEventAdded(eventId: EventId): Unit =
    synchronized {
      lastEventId = eventId
      if (promise != null) {
        promise.success(true)
        promise = null
      }
    }

  /**
    * @param delay When waiting for events, don't succeed after the first event but wait for further events
    */
  def whenEventIsAvailable(after: EventId, until: Instant, delay: Duration = Duration.ZERO)(implicit ec: ExecutionContext): Future[Boolean] = {
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
        future
          .thenDelay(delay min (until - now))(timerService, ec)
          .timeoutAt(until, Success(false), getClass.getName)(timerService, ec)
    }
  }
}
