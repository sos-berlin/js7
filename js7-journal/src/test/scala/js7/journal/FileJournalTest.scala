package js7.journal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.CyclicBarrier
import js7.base.BuildInfo
import js7.base.catsutils.CatsEffectExtensions.{left, orThrow}
import js7.base.catsutils.Environment
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestWallClock, WallClock}
import js7.base.utils.Missing
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.event.{EventCalc, EventId, KeyedEvent, SnapshotableState, Stamped, TimeCtx}
import js7.journal.FileJournalTest.*
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.file
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.test.TestEvent.{Appended, SimpleAdded}
import js7.journal.test.{TestAggregate, TestEvent, TestState}
import org.scalatest.Assertion
import scala.collection.immutable.ArraySeq
import scala.concurrent.blocking
import scala.concurrent.duration.FiniteDuration

final class FileJournalTest extends OurAsyncTestSuite:

  override protected def testWallClock = Missing // This test itself sets the WallClock

  override protected def testTimeout: FiniteDuration =
    if sys.props.contains("test.speed") then 999.s else 99.s

  private given IORuntime = ioRuntime

  "persist" in :
    testJournal(TestWallClock(ts"1970-01-01T00:00:00.001Z")): journal =>
      for
        persisted <-
          journal.persist:
            EventCalc.pure("A" <-: TestEvent.SimpleAdded("(FIRST)"))
          .orThrow

        _ <- IO:
          // EventId 1000 is SnapshotTaken
          assert(persisted.stampedKeyedEvents == Seq(
            Stamped(1001L, "A" <-: TestEvent.SimpleAdded("(FIRST)"))))
          assert(persisted.aggregate == TestState(
            eventId = 1001L,
            SnapshotableState.Standards.empty,
            Map("A" -> TestAggregate("A", "(FIRST)"))))

          val recovered = StateRecoverer.recover[TestState](journal.journalLocation, config)
          assert(recovered.eventId == 1001L)
          assert(recovered.state == TestState(
            eventId = 1001L,
            SnapshotableState.Standards.empty,
            Map("A" -> TestAggregate("A", "(FIRST)"))))

          val normalizedJournalFileContent = journal.journalLocation.file(EventId.BeforeFirst)
            .contentString
            .replaceAll(""""totalRunningTime":[0-9.]+,""", """"totalRunningTime":0,""")
          assert(normalizedJournalFileContent ==
            s"""{"TYPE":"JS7.Journal","typeName":"TestState","journalId":"${journal.journalId}","eventId":0,"generation":1,"totalEventCount":0,"totalRunningTime":0,"timestamp":"1970-01-01T00:00:00.001Z","initiallyStartedAt":"1970-01-01T00:00:00.001Z","version":"1","js7Version":"${BuildInfo.prettyVersion}","buildId":"${BuildInfo.buildId}"}
               |"-------SNAPSHOT-------"
               |"-------END OF SNAPSHOT-------"
               |"-------EVENTS-------"
               |{"eventId":1000,"TYPE":"SnapshotTaken"}
               |{"eventId":1001,"Key":"A","TYPE":"SimpleAdded","string":"(FIRST)"}
               |""".stripMargin)

        _ <- journal.takeSnapshot
        assertion <- IO:
          val recovered = StateRecoverer.recover[TestState](journal.journalLocation, config)
          assert(recovered.eventId == 1002L)
          assert(recovered.state == TestState(
            eventId = 1002L,
            SnapshotableState.Standards.empty,
            Map("A" -> TestAggregate("A", "(FIRST)"))))

          val normalizedJournalFileContent = journal.journalLocation.file(EventId(1001L))
            .contentString
            .replaceAll(""""totalRunningTime":[0-9.]+,""", """"totalRunningTime":0,""")
          assert(normalizedJournalFileContent ==
            s"""{"TYPE":"JS7.Journal","typeName":"TestState","journalId":"${journal.journalId}","eventId":1001,"generation":2,"totalEventCount":2,"totalRunningTime":0,"timestamp":"1970-01-01T00:00:00.001Z","initiallyStartedAt":"1970-01-01T00:00:00.001Z","version":"1","js7Version":"${BuildInfo.prettyVersion}","buildId":"${BuildInfo.buildId}"}
               |"-------SNAPSHOT-------"
               |{"TYPE":"TestAggregate","key":"A","string":"(FIRST)","a":"X","b":"X","c":"X","d":"X","e":"X","f":"X","g":"X","h":"X","i":"X","j":"X","k":"X","l":"X","m":"X","n":"X","o":"X","p":"X","q":"X","r":"X"}
               |"-------END OF SNAPSHOT-------"
               |"-------EVENTS-------"
               |{"eventId":1002,"TYPE":"SnapshotTaken"}
               |""".stripMargin)
      yield
        assertion

  "persist is cancelable" in :
    testJournal(
      TestWallClock(ts"1970-01-01T00:00:00.001Z"),
      config"js7.journal.sync = off"
    ): journal =>
      journal.persist:
        // Initialize
        Seq(
          "A" <-: SimpleAdded(""),
          "B" <-: SimpleAdded(""))
      .orThrow
      .flatMap: _ =>
        IO.defer:
          val barrier1 = new CyclicBarrier(2)
          val barrier2 = new CyclicBarrier(2)
          journal.persist:
            // Block the first persist operation
            EventCalc[TestState, TestEvent, TimeCtx]: coll =>
              blocking:
                barrier1.await()
                barrier2.await()
                coll.add("A" <-: Appended('+'))
          .both:
            journal.persist("B" <-: Appended('?')).start.flatMap: fiber =>
              barrier1.await()
              // Cancel the second persist operation while the first one is still running
              fiber.cancel
                .productR:
                  fiber.joinWith(IO.left(Problem("CANCELED")))
                .productL:
                  IO(barrier2.await())
          .map: pair =>
            assert(pair._1.isInstanceOf[Right[?, ?]] && pair._2 == Left(Problem("CANCELED")))
        .replicateA(1000)
      .map(_.combineAll)

  "Big snapshot" in :
    val objectSize = 10_000
    val bigEvent = TestEvent.Added("+" * objectSize)

    def run(n: Int): IO[Assertion] =
      testJournal(WallClock): journal =>
        for
          persisted <-
            journal.persist: _ =>
              (1 to n).iterator.map: i =>
                val str = i.toString
                str <-: bigEvent
            .orThrow

          _ <- journal.aggregate.map: aggr =>
            assert(aggr.keyToAggregate.size == n)

          (writeDuration, _) <- journal.takeSnapshot.timed
          _ <- IO:
            info_(s"Snapshot write: ${itemsPerSecondString(writeDuration, n, "objects")}")

          (readDuration, recovered) <-
            IO:
              StateRecoverer.recover[TestState](journal.journalLocation, config)
            .timed
          _ <- IO:
            info_(s"Snapshot read: ${itemsPerSecondString(readDuration, n, "objects")}")
            assert(recovered.state.keyToAggregate.size == n)
            assert(recovered.state == journal.unsafeAggregate())
        yield
          succeed

    run(10_000) *>
      sys.props.get("test.speed").map(_.toInt).foldMap: n =>
        run(n)

  "Massive parallel" - {
    "test empty EventCalc" in :
      run(
        n = if isIntelliJIdea then 1_000_000 else 10_000,
        persistLimit = 512,
        _ => Nil)

    "test" in :
      run(
        n = if isIntelliJIdea then 1_000_000 else 10_000,
        persistLimit = 512,
        i =>
          Thread.sleep(0, 1000) // 1µs assumed event computation time
          (i.toString <-: TestEvent.SimpleAdded("A")) :: Nil)

    def run(n: Int, persistLimit: Int, toEvents: Int => Seq[KeyedEvent[TestEvent]]): IO[Assertion] =
      testJournal(TestWallClock(ts"1970-01-01T00:00:00.001Z"), config"""
        js7.journal.concurrent-persist-limit = $persistLimit
        js7.journal.slow-check-state = false"""
      ): journal =>
        (1 to n).to(ArraySeq).parTraverse: i =>
          journal.persist:
            EventCalc[TestState, TestEvent, TimeCtx]:
              _.add(toEvents(i))
        .timed.flatMap: (duration, _) =>
          IO:
            info_(s"$n in parallel, concurrent-persist-limit=$persistLimit • " +
              itemsPerSecondString(duration, n, "commits"))
            succeed
      .productL:
        IO(CallMeter.logAll())
  }

  private def testJournal(clock: WallClock, config: Config = ConfigFactory.empty)
    (tester: FileJournal[TestState] => IO[Assertion])
  : IO[Assertion] =
    val myConfig = config.withFallback(FileJournalTest.config)
    locally:
      for
        _ <- Environment.registerPure[WallClock](clock)
        dir <- temporaryDirectoryResource[IO]("FileJournalTest-")
        journalLocation = JournalLocation(TestState, dir / "test")
        journal <- FileJournal.service(
          Recovered.noJournalFile[TestState](journalLocation, myConfig),
          JournalConf.fromConfig(myConfig))
      yield
        journal
    .use: journal =>
      journal.failWhenStopped:
        tester(journal)

  private def info_(line: String) =
    logger.info(s"🟪🟪🟪 $line")
    if !isIntelliJIdea then info(line)


object FileJournalTest:
  private val logger = Logger[this.type]

  private val config = config"""
    js7.journal.sync = on
    js7.journal.delay = 0s
    js7.journal.sync-delay = 0s
    js7.journal.simulate-sync = 1ms
    js7.journal.concurrent-persist-limit = 512
    js7.journal.users-allowed-to-release-events = []
    js7.journal.watch.index-size = 1000
    js7.journal.watch.keep-open = 100
    js7.journal.persist-warn-durations = [ 1s ]
    js7.journal.log.ack-warn-durations = [ 1s ]
    js7.journal.log.info-events = []
    js7.journal.snapshot.period = 1h
    js7.journal.snapshot.when-bigger-than = 1G
    js7.journal.snapshot.estimate-event-threshold = 0
    js7.journal.release-events-delay = 0s
    js7.journal.remove-obsolete-files = false
  """
