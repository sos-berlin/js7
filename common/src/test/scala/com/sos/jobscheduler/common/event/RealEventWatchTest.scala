package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.RealEventWatchTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RealEventWatchTest extends FreeSpec {

  "tornOlder" in {
    val events = Stamped(1, 1 <-: TestEvent(1)) :: Nil  // Event 1 = 1970-01-01, very old
    val eventWatch = new RealEventWatch[TestEvent] {
      protected val timerService = new TimerService(Some(1.s))
      def tornEventId = 0
      protected def reverseEventsAfter(after: EventId) = CloseableIterator.empty
      protected def eventsAfter(after: EventId) = Some(CloseableIterator.fromIterator(events.iterator dropWhile (_.eventId <= after)))
      onEventsAdded(events.last.eventId)
    }
    val a = eventWatch.observe(EventRequest.singleClass[TestEvent](limit = 1)).toListL.runAsync await 99.seconds
    assert(a == events)

    // Event from 1970-01-01 is older than 1s
    val observable = eventWatch.observe(EventRequest.singleClass[TestEvent](tornOlder = 1.second)).toListL.runAsync
    intercept[TornException] { observable await 99.seconds }
    observable.cancel()

    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](limit = 7, after = 1, tornOlder = 1.second))
      .toListL.runAsync.await(99.seconds).isEmpty)
  }

  "observe without stack overflow" in {
    val eventWatch = new EndlessEventWatch().strict
    var expectedNext = Stamped(1, 1 <-: TestEvent(1))
    val events = mutable.Buffer[Stamped[KeyedEvent[TestEvent]]]()
    val n = 100000
    eventWatch.observe(EventRequest.singleClass[TestEvent](limit = n, timeout = 99.seconds))
      .foreach { stamped ⇒
        assert(stamped == expectedNext)
        expectedNext = Stamped(stamped.eventId + 1, (stamped.value.key + 1) <-: TestEvent(stamped.value.event.number + 1))
        events += stamped
      }
      .await(99.seconds)
    assert(expectedNext.eventId == n + 1)
    assert(events == (1L to n).map(toStampedEvent))
  }
}

object RealEventWatchTest {
  private val EventsPerIteration = 3

  private case class TestEvent(number: Long) extends Event {
    type Key = Long
  }

  private def toStampedEvent(i: Long) = Stamped(i, i <-: TestEvent(i))

  private class EndlessEventWatch extends RealEventWatch[TestEvent] {
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
