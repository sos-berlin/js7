package js7.journal.watch

import cats.instances.try_._
import cats.syntax.foldable._
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.event.{EventId, EventRequest, KeyedEvent, Stamped}
import js7.journal.test.{TestAggregate, TestEvent, TestState}
import js7.journal.watch.InMemoryJournalTest._
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

final class InMemoryJournalTest extends AnyFreeSpec
{
  "Initial values" in {
    val journal = newJournal()
    assert(journal.tornEventId == EventId.BeforeFirst)
    assert(journal.lastAddedEventId == EventId.BeforeFirst)
  }

  "persist" in {
    val journal = newJournal()

    journal.persistKeyedEvent("A" <-: TestEvent.Added("a")).await(99.s).orThrow
    assert(journal.currentState == TestState(1000, keyToAggregate = Map(
      "A" -> TestAggregate("A", "a"))))
    assert(journal.tornEventId == EventId.BeforeFirst)
    assert(journal.lastAddedEventId == 1000)
    assert(journal.observe(EventRequest.singleClass[TestEvent](0)).toListL.await(99.s) == List(
      Stamped(1000, "A" <-: TestEvent.Added("a"))))

    journal.persistKeyedEvent("A" <-: TestEvent.Appended('1')).await(99.s).orThrow
    assert(journal.currentState == TestState(1001, keyToAggregate = Map(
      "A" -> TestAggregate("A", "a1"))))
    assert(journal.tornEventId == EventId.BeforeFirst)
    assert(journal.lastAddedEventId == 1001)

    assert(journal.observe(EventRequest.singleClass[TestEvent](0)).toListL.await(99.s) == List(
      Stamped(1000, "A" <-: TestEvent.Added("a")),
      Stamped(1001, "A" <-: TestEvent.Appended('1'))))
    assert(journal.observe(EventRequest.singleClass[TestEvent](1000)).toListL.await(99.s) == List(
      Stamped(1001, "A" <-: TestEvent.Appended('1'))))
    assert(journal.observe(EventRequest.singleClass[TestEvent](1001)).toListL.await(99.s).isEmpty)
  }

  "test" in {
    val journal = newJournal()

    val observed = mutable.Buffer.empty[Stamped[KeyedEvent[TestEvent]]]
    val observing = journal
      .observe(EventRequest.singleClass[TestEvent](
        after = EventId.BeforeFirst,
        timeout = Some(99.s)))
      .foreach(o => synchronized(observed += o))

    def firstUpdate(state: TestState) =
      state match {
        case TestState.empty => Right(Seq(
          "A" <-: TestEvent.Added("A"),
          "B" <-: TestEvent.Added("B")))
        case o => Left(Problem("FAILED"))
      }

    journal.persist(firstUpdate).await(99.s).orThrow
    assert(journal.persist(firstUpdate).await(99.s) == Left(Problem("FAILED")))

    assert(journal.tornEventId == EventId.BeforeFirst)
    assert(journal.lastAddedEventId == 1001)
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

    assert(journal.tornEventId == EventId.BeforeFirst)
    journal.releaseEvents(1001).await(99.s).orThrow
    assert(journal.tornEventId == 1001)

    assert(journal
      .observe(EventRequest.singleClass[TestEvent](
        after = EventId.BeforeFirst))
      .toListL
      .materialize
      .await(99.s)
      .failed
      .exists(_.isInstanceOf[TornException]))

    assert(journal
      .observe(EventRequest.singleClass[TestEvent](
        after = 1001))
      .toListL
      .await(99.s) == Seq(Stamped(1002, "C" <-: TestEvent.Added("C"))))
  }
}

object InMemoryJournalTest
{
  private def newJournal() =
    new InMemoryJournal(TestState.empty, new EventIdGenerator(EventIdClock.fixed(1)))
}
