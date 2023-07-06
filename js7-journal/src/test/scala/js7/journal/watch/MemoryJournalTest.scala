package js7.journal.watch

import cats.instances.try_.*
import cats.syntax.foldable.*
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.event.{EventId, EventRequest, KeyedEvent, Stamped}
import js7.journal.test.{TestAggregate, TestEvent, TestState}
import js7.journal.watch.MemoryJournalTest.*
import js7.journal.{EventIdClock, EventIdGenerator, MemoryJournal}
import monix.execution.Scheduler.Implicits.traced
import scala.collection.mutable

final class MemoryJournalTest extends OurTestSuite
{
  "Initial values" in {
    val journal = newJournal()
    import journal.eventWatch
    assert(eventWatch.tornEventId == EventId.BeforeFirst)
    assert(eventWatch.lastAddedEventId == EventId.BeforeFirst)
  }

  "persist" in {
    val journal = newJournal()
    import journal.eventWatch

    journal.persistKeyedEvent("A" <-: TestEvent.Added("a")).await(99.s).orThrow
    assert(journal.unsafeCurrentState() == TestState(1000, keyToAggregate = Map(
      "A" -> TestAggregate("A", "a"))))
    assert(eventWatch.tornEventId == EventId.BeforeFirst)
    assert(eventWatch.lastAddedEventId == 1000)
    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](0)).toListL.await(99.s) == List(
      Stamped(1000, "A" <-: TestEvent.Added("a"))))

    journal.persistKeyedEvent("A" <-: TestEvent.Appended('1')).await(99.s).orThrow
    assert(journal.unsafeCurrentState() == TestState(1001, keyToAggregate = Map(
      "A" -> TestAggregate("A", "a1"))))
    assert(eventWatch.tornEventId == EventId.BeforeFirst)
    assert(eventWatch.lastAddedEventId == 1001)

    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](0)).toListL.await(99.s) == List(
      Stamped(1000, "A" <-: TestEvent.Added("a")),
      Stamped(1001, "A" <-: TestEvent.Appended('1'))))
    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](1000)).toListL.await(99.s) == List(
      Stamped(1001, "A" <-: TestEvent.Appended('1'))))
    assert(eventWatch.observe(EventRequest.singleClass[TestEvent](1001)).toListL.await(99.s).isEmpty)
  }

  "test" in {
    val journal = newJournal()
    import journal.eventWatch

    val observed = mutable.Buffer.empty[Stamped[KeyedEvent[TestEvent]]]
    val observing = eventWatch
      .observe(EventRequest.singleClass[TestEvent](
        after = EventId.BeforeFirst,
        timeout = Some(99.s)))
      .foreach(o => synchronized(observed += o))

    def firstUpdate(state: TestState) =
      state match {
        case TestState.empty => Right(Seq(
          "A" <-: TestEvent.Added("A"),
          "B" <-: TestEvent.Added("B")))
        case _ => Left(Problem("FAILED"))
      }

    journal.persist(firstUpdate).await(99.s).orThrow
    assert(journal.persist(firstUpdate).await(99.s) == Left(Problem("FAILED")))

    assert(eventWatch.tornEventId == EventId.BeforeFirst)
    assert(eventWatch.lastAddedEventId == 1001)
    waitForCondition(10.s, 10.ms)(synchronized(observed.size == 2))
    assert(observed == Seq(
      Stamped(1000, "A" <-: TestEvent.Added("A")),
      Stamped(1001, "B" <-: TestEvent.Added("B"))))

    journal
      .persist(_ => Right(Seq(
        "C" <-: TestEvent.Added("C"))))
      .await(99.s).orThrow

    waitForCondition(10.s, 10.ms)(synchronized(observed.size == 3))
    assert(observed == Seq(
      Stamped(1000, "A" <-: TestEvent.Added("A")),
      Stamped(1001, "B" <-: TestEvent.Added("B")),
      Stamped(1002, "C" <-: TestEvent.Added("C"))))

    observing.cancel()

    assert(eventWatch.tornEventId == EventId.BeforeFirst)
    journal.releaseEvents(1001).await(99.s).orThrow
    assert(eventWatch.tornEventId == 1001)

    assert(eventWatch
      .observe(EventRequest.singleClass[TestEvent](
        after = EventId.BeforeFirst))
      .toListL
      .materialize
      .await(99.s)
      .failed
      .exists(_.isInstanceOf[TornException]))

    assert(eventWatch
      .observe(EventRequest.singleClass[TestEvent](
        after = 1001))
      .toListL
      .await(99.s) == Seq(Stamped(1002, "C" <-: TestEvent.Added("C"))))
  }

  "size" in {
    val size = 3
    val journal = newJournal(size = size)
    import journal.eventWatch

    val observed = mutable.Buffer.empty[Stamped[KeyedEvent[TestEvent]]]
    val observing = eventWatch
      .observe(EventRequest.singleClass[TestEvent](
        after = EventId.BeforeFirst,
        timeout = Some(99.s)))
      .foreach(o => synchronized(observed += o))

    val n = 10
    val persisting = (0 until n).map(_.toString).toVector
      .traverse_(x => journal
        .persistKeyedEvent(x <-: TestEvent.Added(x))
        .map(_.orThrow))
        .runToFuture

    def lastObserved = synchronized(observed.lastOption).map(_.eventId)

    waitForCondition(10.s, 10.ms)(lastObserved.contains(1002L) && journal.queueLength == 3)
    assert(observed.last.eventId == 1002 && journal.queueLength == 3)

    for (i <- 0 until n - size) withClue(s"#$i") {
      journal.releaseEvents(untilEventId = 1000 + i).await(99.s).orThrow
      waitForCondition(10.s, 10.ms)(lastObserved.contains(1000L + size + i) && journal.queueLength == 3)
      assert(lastObserved.contains(1000L + size + i) && journal.queueLength == 3)
    }

    persisting.await(9.s)
    observing.cancel()
  }

  "persist more than size events at once" in {
    val size = 3
    val journal = newJournal(size = size)
    import journal.eventWatch

    val events = (0 until 3 * size).map(_.toString).map(x => x <-: TestEvent.Added(x))
    journal
      .persistKeyedEvents(events)
      .await(99.s)
    assert(journal.queueLength == 3 * size)
    assert(eventWatch
      .observe(EventRequest.singleClass[TestEvent](after = EventId.BeforeFirst))
      .map(_.value)
      .toListL
      .await(99.s) == events)
  }
}

object MemoryJournalTest
{
  private def newJournal(size: Int = Int.MaxValue) =
    new MemoryJournal(
      TestState.empty,
      size = size,
      infoLogEvents = Set.empty,
      eventIdGenerator = new EventIdGenerator(EventIdClock.fixed(1)))
}