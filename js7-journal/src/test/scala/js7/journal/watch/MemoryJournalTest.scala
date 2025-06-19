package js7.journal.watch

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import cats.instances.try_.*
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import js7.base.catsutils.CatsExtensions.tryIt
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.{unsafeToCancelableFuture}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.event.{Event, EventCalc, EventId, EventRequest, KeyedEvent, Stamped}
import js7.journal.test.{TestAggregate, TestEvent, TestState}
import js7.journal.watch.MemoryJournalTest.*
import js7.journal.{CommitOptions, EventIdGenerator, MemoryJournal}
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.Assertion
import scala.collection.mutable

final class MemoryJournalTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "Initial values" in:
    journal().use: journal =>
      IO:
        import journal.eventWatch
        assert(eventWatch.tornEventId == EventId.BeforeFirst)
        assert(eventWatch.lastAddedEventId == EventId.BeforeFirst)

  "persist" in:
    journal().use: journal =>
      IO:
        import journal.eventWatch

        journal.persist("A" <-: TestEvent.Added("a")).await(99.s).orThrow
        assert(journal.unsafeAggregate() == TestState(1000, keyToAggregate = Map(
          "A" -> TestAggregate("A", "a"))))
        assert(eventWatch.tornEventId == EventId.BeforeFirst)
        assert(eventWatch.lastAddedEventId == 1000)
        assert:
          eventWatch.stream:
            EventRequest.singleClass[TestEvent](0, timeout = Some(0.s))
          .compile.toList.await(99.s) == List(
            Stamped(1000, "A" <-: TestEvent.Added("a")))

        journal.persist("A" <-: TestEvent.Appended('1')).await(99.s).orThrow
        assert(journal.unsafeAggregate() == TestState(1001, keyToAggregate = Map(
          "A" -> TestAggregate("A", "a1"))))
        assert(eventWatch.tornEventId == EventId.BeforeFirst)
        assert(eventWatch.lastAddedEventId == 1001)

        assert:
          eventWatch.stream:
            EventRequest.singleClass[TestEvent](0, timeout = Some(0.s))
          .compile.toList.await(99.s) == List(
            Stamped(1000, "A" <-: TestEvent.Added("a")),
            Stamped(1001, "A" <-: TestEvent.Appended('1')))
        assert:
          eventWatch.stream:
            EventRequest.singleClass[TestEvent](1000, timeout = Some(0.s))
          .compile.toList.await(99.s) == List(
            Stamped(1001, "A" <-: TestEvent.Appended('1')))
        assert:
          eventWatch.stream:
            EventRequest.singleClass[TestEvent](1001, timeout = Some(0.s))
          .compile.toList.await(99.s).isEmpty

  "test" in:
    journal().use: journal =>
      IO:
        import journal.eventWatch

        val observed = mutable.Buffer.empty[Stamped[KeyedEvent[TestEvent]]]
        val observing = eventWatch
          .stream(EventRequest.singleClass[TestEvent](
            after = EventId.BeforeFirst,
            timeout = Some(99.s)))
          .foreach(o => IO(synchronized:
            observed += o))
          .compile.drain.unsafeToCancelableFuture()

        def firstUpdate(state: TestState) =
          state match
            case TestState.empty => Right(Seq(
              "A" <-: TestEvent.Added("A"),
              "B" <-: TestEvent.Added("B")))
            case _ => Left(Problem("FAILED"))

        journal.persist(firstUpdate).await(99.s).orThrow
        assert(journal.persist(firstUpdate).await(99.s) == Left(Problem("FAILED")))

        assert(eventWatch.tornEventId == EventId.BeforeFirst)
        assert(eventWatch.lastAddedEventId == 1001)
        waitForCondition(10.s, 10.ms)(synchronized(observed.size == 2))
        assert(observed == Seq(
          Stamped(1000, "A" <-: TestEvent.Added("A")),
          Stamped(1001, "B" <-: TestEvent.Added("B"))))

        journal.persist:
          "C" <-: TestEvent.Added("C")
        .await(99.s).orThrow

        waitForCondition(10.s, 10.ms)(synchronized(observed.size == 3))
        assert(observed == Seq(
          Stamped(1000, "A" <-: TestEvent.Added("A")),
          Stamped(1001, "B" <-: TestEvent.Added("B")),
          Stamped(1002, "C" <-: TestEvent.Added("C"))))

        observing.cancelAndForget()

        assert(eventWatch.tornEventId == EventId.BeforeFirst)

        /// releaseEvents ///

        journal.releaseEvents(1001).await(99.s).orThrow
        assert(eventWatch.tornEventId == 1001)
        assert(eventWatch
          .stream:
            EventRequest.singleClass[TestEvent](after = 1001, timeout = Some(0.s))
          .compile.toList
          .await(99.s) == Seq(Stamped(1002, "C" <-: TestEvent.Added("C"))))

        journal.releaseEvents(1002).await(99.s).orThrow
        assert(journal.isEmpty)
        assert(eventWatch.tornEventId == 1002)
        assert:
          journal.eventWatch.stream:
            EventRequest.singleClass[Event](after = 1002, timeout = Some(0.s))
          .compile.toList.await(99.s)
          .isEmpty

        assert(eventWatch
          .stream:
            EventRequest.singleClass[TestEvent](after = 1001, timeout = Some(0.s))
          .compile.toList
          .tryIt
          .await(99.s)
          .failed
          .exists(_.isInstanceOf[TornException]))

  "size" in:
    val size = 3
    journal(size = size).use: journal =>
      IO:
        val observed = mutable.Buffer.empty[Stamped[KeyedEvent[TestEvent]]]
        val observing = journal.eventWatch
          .stream(EventRequest.singleClass[TestEvent](
            after = EventId.BeforeFirst,
            timeout = Some(99.s)))
          .foreach(o => IO(synchronized:
            observed += o))
          .compile
          .drain
          .unsafeToCancelableFuture()

        val n = 10
        val persisting = (0 until n).map(_.toString).toVector
          .traverse_(x => journal
            .persist(x <-: TestEvent.Added(x))
            .map(_.orThrow))
            .unsafeToFuture()

        def lastObserved = synchronized(observed.lastOption).map(_.eventId)

        awaitAndAssert(lastObserved.contains(1002L) && journal.queueLength == 3)

        for i <- 0 until n - size do withClue(s"#$i"):
          journal.releaseEvents(untilEventId = 1000 + i).await(99.s).orThrow
          awaitAndAssert(lastObserved.contains(1000L + size + i) && journal.queueLength == 3)

        persisting.await(9.s)
        observing.cancelAndForget(): Unit
        succeed

  "persist more than size events at once" in:
    val size = 3
    journal(size = size).use: journal =>
      val events = (0 until 3 * size).map(_.toString).map(x => x <-: TestEvent.Added(x))

      if true then
        // Until v2.6: temporary overflow of MemoryJournal's queue counts the semaphore wrongly
        journal.persist(CommitOptions.CommitLater):
          EventCalc.pure(events)
        .await(99.s)
        assert(journal.queueLength == 3 * size)
        journal.eventWatch
          .stream(EventRequest.singleClass[TestEvent](after = EventId.BeforeFirst,
            timeout = Some(0.s/*We get the whole commit immediately*/)))
          .map(_.value)
          .compile.toList
          .map: list =>
            assert(list == events)
      else
        val feed: IO[Unit] =
          journal.persist(events).map(_.orThrow).void

        val eat: IO[List[KeyedEvent[TestEvent]]] =
          journal.eventWatch
            .stream:
              EventRequest.singleClass[TestEvent](after = EventId.BeforeFirst, timeout = None)
            .take(events.size)
            .chunks
            .evalTap: chunk =>
              journal.releaseEvents(untilEventId = chunk.last.get.eventId)
            .unchunks
            .map(_.value)
            .compile.toList

        IO.both(feed, eat)
          .map(_._2)
          .map: list =>
            assert(list == events)

  "Speed test" in:
    sys.props.get("test.speed").map(_.toInt).fold(IO.pure(pending)): n =>
      // Test congestion with parallelization > availableProcessors
      val parallelization = 3 * sys.runtime.availableProcessors
      testSpeed:
        (0 until n)
          .map: i =>
            val k = i.toString
            k <-: TestEvent.Added(k)
          .toVector
          .grouped((n + parallelization - 1) / parallelization)
          .toVector

  private def testSpeed(events: Seq[Seq[KeyedEvent[TestEvent.Added]]]): IO[Assertion] =
    val n = events.view.flatten.size
    journal(size = 10).use: journal =>
      val feed: IO[Unit] =
        events.parTraverse: myEvents =>
          journal
            .persist(myEvents)
            .map(_.orThrow)
            .void
        .map(_.combineAll)

      val eat: IO[Set[KeyedEvent[TestEvent]]] =
        journal.eventWatch
          .stream:
            EventRequest.singleClass[TestEvent](after = EventId.BeforeFirst, timeout = None)
          .take(n)
          .chunks
          .evalTap: chunk =>
            journal.releaseEvents(untilEventId = chunk.last.get.eventId)
          .unchunks
          .map(_.value)
          .compile
          .fold(Set.newBuilder[KeyedEvent[TestEvent]])(_ += _)
          .map(_.result())

      IO.both(feed, eat)
        .map(_._2)
        .timed
        .flatMap: (duration, eatenEvents) =>
          IO:
            assert(eatenEvents == events.flatten.toSet)
            logger.info(Stopwatch.itemsPerSecondString(duration, n, "events"))
            //FIXME Semaphore: assert(journal.semaphoreCount.await(99.s) == 0)
            succeed

  private def journal(size: Int = Int.MaxValue): ResourceIO[MemoryJournal[TestState]] =
    MemoryJournal.resource(
      TestState.empty,
      size = size,
      infoLogEvents = Set.empty,
      eventIdGenerator = EventIdGenerator.withFixedClock(epochMilli = 1))


object MemoryJournalTest:
  private val logger = Logger[this.type]
