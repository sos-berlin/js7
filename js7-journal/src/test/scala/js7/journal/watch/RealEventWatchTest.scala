package js7.journal.watch

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import js7.base.monixlike.MonixLikeExtensions.unsafeToCancelableFuture
import js7.base.test.{OurTestSuite}
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CloseableIterator
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.journal.watch.RealEventWatchTest.*
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class RealEventWatchTest extends OurTestSuite:

  private given IORuntime = ioRuntime
  private given Scheduler = ioRuntime.scheduler

  "tornOlder" in:
    val events = Stamped(1L, 1L <-: TestEvent(1)) :: Nil  // Event 1 = 1970-01-01, very old
    val eventWatch = new RealEventWatch:
      protected val scheduler = ioRuntime.scheduler
      def isActiveNode = true
      def tornEventId = EventId.BeforeFirst
      protected def eventsAfter(after: EventId) = Some(CloseableIterator.fromIterator(events.iterator dropWhile (_.eventId <= after)))
      onEventsCommitted(events.last.eventId)
      def journalInfo = throw new NotImplementedError
    val a = eventWatch
      .stream(EventRequest.singleClass[TestEvent](limit = 1, timeout = Some(0.s)))
      .compile.toList
      .unsafeToFuture().await(99.s)
    assert(a == events)

    // Event from 1970-01-01 is older than 1s
    val stream = eventWatch
      .stream(EventRequest.singleClass[TestEvent](tornOlder = Some(1.s), timeout = Some(0.s)))
      .compile.toList
      .unsafeToCancelableFuture()
    intercept[TornException]:
      stream.await(99.s)
    stream.cancelAndForget()

    assert:
      eventWatch
        .stream:
          EventRequest.singleClass[TestEvent](
            limit = 7, timeout = Some(0.s), after = 1L, tornOlder = Some(1.s))
        .compile.toList
        .await(99.s).isEmpty

  "stream without stack overflow" in:
    val eventWatch = new EndlessEventWatch()
    var expectedNext = Stamped(1L, 1 <-: TestEvent(1))
    val events = mutable.Buffer[Stamped[KeyedEvent[TestEvent]]]()
    val n = 100000
    eventWatch.stream(EventRequest.singleClass[TestEvent](limit = n, timeout = Some(99.s)), onlyAcks = false)
      .foreach(stamped => IO:
        assert(stamped == expectedNext)
        expectedNext = Stamped(stamped.eventId + 1, (stamped.value.key + 1) <-: TestEvent(stamped.value.event.number + 1))
        events += stamped)
      .compile.drain
      .await(99.s)
    assert(expectedNext.eventId == n + 1)
    assert(events == (1L to n).map(toStampedEvent))


object RealEventWatchTest:
  private val EventsPerIteration = 3

  private case class TestEvent(number: Long) extends Event.IsKeyBase[TestEvent]:
    val keyCompanion: TestEvent.type = TestEvent
  private object TestEvent extends Event.CompanionForKey[Long, TestEvent]:
    implicit val implicitSelf: TestEvent.type = this

  private def toStampedEvent(i: Long) = Stamped(i, i <-: TestEvent(i))

  private class EndlessEventWatch()(using Scheduler) extends RealEventWatch:
    protected val scheduler = summon[Scheduler]

    def isActiveNode = true

    def tornEventId = EventId.BeforeFirst

    def journalInfo = throw new NotImplementedError

    onEventsCommitted(1L)

    def eventsAfter(after: EventId) =
      Some(CloseableIterator.fromIterator(
        Iterator.from(1) take EventsPerIteration map { i =>
          onEventsCommitted(after + i + 1)  // Announce following event
          toStampedEvent(after + i)
        }))
