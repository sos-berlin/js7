package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.time.ScalaTime.finiteToJavaDuration
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.time.timer.TimerService.TimeoutFuture
import com.sos.jobscheduler.data.event._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
/*private[event]*/ final class Sync(initialLastEventId: EventId, timerService: TimerService) {

  @volatile private var promise: Promise[Boolean] = null
  @volatile private var lastEventId = initialLastEventId

  def onEventAdded(eventId: EventId): Unit =
    synchronized {
      if (eventId < lastEventId) throw new IllegalArgumentException(s"Added EventId ${EventId.toString(eventId)} < last EventId ${EventId.toString(lastEventId)}")
      lastEventId = eventId
      if (promise != null) {
        promise.success(true)
        promise = null
      }
    }

  /**
    * @param delay When waiting for events, don't succeed after the first event but wait for further events
    */
  def whenEventIsAvailable(after: EventId, until: Timestamp, delay: FiniteDuration = Duration.Zero)(implicit ec: ExecutionContext): Future[Boolean] = {
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
          .timeoutAt(until.toInstant, Success(false), getClass.getName)(timerService, ec)
    }
  }

  def lastAddedEventId = lastEventId
}
