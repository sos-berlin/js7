package js7.journal.watch

import cats.instances.try_._
import cats.syntax.foldable._
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.event.{EventId, EventRequest, KeyedEvent, Stamped}
import js7.journal.test.{TestEvent, TestState}
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

final class InMemoryJournalTest extends AnyFreeSpec
{
  "test" in {
    val journal = new InMemoryJournal(
      TestState.empty,
      new EventIdGenerator(EventIdClock.fixed(0)))
    assert(journal.tornEventId == EventId.BeforeFirst)
    assert(journal.lastAddedEventId == EventId.BeforeFirst)

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

    journal.emit(firstUpdate).await(99.s).orThrow
    assert(journal.emit(firstUpdate).await(99.s) == Left(Problem("FAILED")))

    assert(journal.tornEventId == EventId.BeforeFirst)
    assert(journal.lastAddedEventId == 2)
    waitForCondition(10.s, 10.ms)(synchronized(observed.size == 2))
    assert(observed == Seq(
      Stamped(1, "A" <-: TestEvent.Added("A")),
      Stamped(2, "B" <-: TestEvent.Added("B"))))

    journal
      .emit(_ => Right(Seq(
        "C" <-: TestEvent.Added("C"))))
      .await(99.s).orThrow

    waitForCondition(10.s, 10.ms)(synchronized(observed.size == 3))
    assert(observed == Seq(
      Stamped(1, "A" <-: TestEvent.Added("A")),
      Stamped(2, "B" <-: TestEvent.Added("B")),
      Stamped(3, "C" <-: TestEvent.Added("C"))))

    observing.cancel()

    assert(journal.tornEventId == EventId.BeforeFirst)
    journal.releaseEvents(2).await(99.s)
    assert(journal.tornEventId == 2)

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
        after = 2))
      .toListL
      .await(99.s) == Seq(Stamped(3, "C" <-: TestEvent.Added("C"))))
  }
}
