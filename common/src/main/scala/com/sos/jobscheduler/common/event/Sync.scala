package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.time.ScalaTime._
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
  def whenEventIsAvailable(after: EventId, until: Option[Deadline], delay: FiniteDuration = Duration.Zero): Task[Boolean] =
    if (after < lastEventId)
      Task.pure(true)
    else if (until.exists(_.hasElapsed))
      Task.pure(false)
    else {
      val task = (promise match {
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
      }).delayResult(delay min until.fold(FiniteDuration.MaxValue)(_.timeLeftOrZero))
      until.fold(task)(u => task.timeoutTo(u.timeLeftOrZero, Task.pure(false)))
    }

  def lastAddedEventId = lastEventId
}
