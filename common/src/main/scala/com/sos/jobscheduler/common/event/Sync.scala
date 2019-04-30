package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.data.event._
import monix.eval.Task
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
/*private[event]*/ final class Sync(initialLastEventId: EventId)
{
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
  def whenEventIsAvailable(after: EventId, until: Timestamp, delay: FiniteDuration = Duration.Zero): Task[Boolean] =
    if (until <= now)
      Task.pure(false)
    else if (after < lastEventId)
      Task.pure(true)
    else
      (promise match {
        case p if p != null =>
          Task.fromFuture(p.future)
        case _ =>
          synchronized {
            if (after < lastEventId)
              Task.pure(true)
            else {
              if (promise == null) {
                promise = Promise[Boolean]()
              }
              Task.fromFuture(promise.future)
            }
          }
      })
      .delayResult(delay min (until - now)).timeoutTo(until - now, Task.pure(false))

  def lastAddedEventId = lastEventId
}
