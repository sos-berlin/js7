package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.RealEventWatchTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class RealEventWatchTest extends FreeSpec {

  "tornOlder" in {
    val events = Stamped(1, 1 <-: TestEvent(1)) :: Nil  // Event 1 = 1970-01-01, very old
    val eventWatch = new RealEventWatch[TestEvent] {
      protected def scheduler = Scheduler.global
      def tornEventId = 0
      protected def reverseEventsAfter(after: EventId) = CloseableIterator.empty
      protected def eventsAfter(after: EventId) = Some(CloseableIterator.fromIterator(events.iterator dropWhile (_.eventId <= after)))
      def snapshotObjectsFor(after: EventId) = None
      onEventsAdded(events.last.eventId)
    }
    val a = eventWatch.observe(EventRequest.singleClass[TestEvent](limit = 1)).toListL.runToFuture await 99.s
    assert(a == events)

    // Event from 1970-01-01 is older than 1s
    val observable = eventWatch.observe(EventRequest.singleClass[TestEvent](tornOlder = Some(1.s))).toListL.runToFuture
    intercept[TornException] { observable await 99.s }
    observable.cancel()

    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](limit = 7, after = 1, tornOlder = Some(1.s)))
      .toListL.runToFuture.await(99.s).isEmpty)
  }

  "observe without stack overflow" in {
    val eventWatch = new EndlessEventWatch().strict
    var expectedNext = Stamped(1, 1 <-: TestEvent(1))
    val events = mutable.Buffer[Stamped[KeyedEvent[TestEvent]]]()
    val n = 100000
    eventWatch.observe(EventRequest.singleClass[TestEvent](limit = n, timeout = Some(99.s)))
      .foreach { stamped =>
        assert(stamped == expectedNext)
        expectedNext = Stamped(stamped.eventId + 1, (stamped.value.key + 1) <-: TestEvent(stamped.value.event.number + 1))
        events += stamped
      }
      .await(99.s)
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
    val tornEventId = 0

    def snapshotObjectsFor(after: EventId) = None

    protected def reverseEventsAfter(after: EventId) = CloseableIterator.empty

    onEventsAdded(1)

    def eventsAfter(after: EventId) =
      Some(CloseableIterator.fromIterator(
        Iterator.from(1) take EventsPerIteration map { i =>
          onEventsAdded(after + i + 1)  // Announce following event
          toStampedEvent(after + i)
        }))
  }
}
