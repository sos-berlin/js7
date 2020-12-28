package js7.journal.watch

import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.CloseableIterator
import js7.common.scalautil.Futures.implicits._
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.journal.watch.RealEventWatchTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final class RealEventWatchTest extends AnyFreeSpec
{
  "tornOlder" in {
    val events = Stamped(1L, 1L <-: TestEvent(1)) :: Nil  // Event 1 = 1970-01-01, very old
    val eventWatch = new RealEventWatch {
      def isActiveNode = true
      def fileEventIds = EventId.BeforeFirst :: Nil
      protected def eventsAfter(after: EventId) = Some(CloseableIterator.fromIterator(events.iterator dropWhile (_.eventId <= after)))
      def snapshotAfter(after: EventId) = None
      def rawSnapshotAfter(after: EventId) = None
      def observeFile(fileEventId: Option[EventId], position: Option[Long], timeout: FiniteDuration, markEOF: Boolean, onlyAcks: Boolean) =
        throw new NotImplementedError
      onEventsCommitted(events.last.eventId)
      def journalInfo = throw new NotImplementedError
    }
    val a = eventWatch.observe(EventRequest.singleClass[TestEvent](limit = 1)).toListL.runToFuture await 99.s
    assert(a == events)

    // Event from 1970-01-01 is older than 1s
    val observable = eventWatch.observe(EventRequest.singleClass[TestEvent](tornOlder = Some(1.s))).toListL.runToFuture
    intercept[TornException] { observable await 99.s }
    observable.cancel()

    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](limit = 7, after = 1L, tornOlder = Some(1.s)))
      .toListL.runToFuture.await(99.s).isEmpty)
  }

  "observe without stack overflow" in {
    val eventWatch = new EndlessEventWatch().strict
    var expectedNext = Stamped(1L, 1 <-: TestEvent(1))
    val events = mutable.Buffer[Stamped[KeyedEvent[TestEvent]]]()
    val n = 100000
    eventWatch.observe(EventRequest.singleClass[TestEvent](limit = n, timeout = Some(99.s)), onlyAcks = false)
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

  private class EndlessEventWatch extends RealEventWatch {
    def isActiveNode = true

    def fileEventIds = EventId.BeforeFirst :: Nil

    def snapshotAfter(after: EventId) = None

    def rawSnapshotAfter(after: EventId) = None

    def journalInfo = throw new NotImplementedError

    onEventsCommitted(1L)

    def eventsAfter(after: EventId) =
      Some(CloseableIterator.fromIterator(
        Iterator.from(1) take EventsPerIteration map { i =>
          onEventsCommitted(after + i + 1)  // Announce following event
          toStampedEvent(after + i)
        }))

    def observeFile(fileEventId: Option[EventId], position: Option[Long], timeout: FiniteDuration, markEOF: Boolean, onlyAcks: Boolean) =
      Task(Left(Problem("EndlessEventWatch.observeFile is not implemented")))
  }
}
