package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.RealEventWatchTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, Stamped}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RealEventWatchTest extends FreeSpec {

  "observe without stack overflow" in {
    val eventWatch = new TestEventWatch().strict
    var expectedNext = Stamped(1, 1 <-: TestEvent(1))
    val n = 100000
    val observed = eventWatch.observe(EventRequest.singleClass[TestEvent](limit = n, timeout = 99.seconds))
      .foreach { stamped ⇒
        assert(stamped == expectedNext)
        expectedNext = Stamped(stamped.eventId + 1, (stamped.value.key + 1) <-: TestEvent(stamped.value.event.number + 1))
      }
    observed await 99.s
    assert(expectedNext.eventId == n + 1)
  }
}

object RealEventWatchTest {
  private val EventsPerIteration = 3

  private case class TestEvent(number: Long) extends Event {
    type Key = Long
  }

  private def toStampedEvent(i: Long) = Stamped(i, i <-: TestEvent(i))

  private class TestEventWatch extends RealEventWatch[TestEvent] {
    protected val timerService = new TimerService(Some(1.s))
    val tornEventId = 0

    protected def reverseEventsAfter(after: EventId) = CloseableIterator.empty

    onEventsAdded(1)

    def eventsAfter(after: EventId) =
      Some(CloseableIterator.fromIterator(
        Iterator.from(1) take EventsPerIteration map { i ⇒
          onEventsAdded(after + i + 1)  // Announce following event
          toStampedEvent(after + i)
        }))
  }
}
