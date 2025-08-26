package js7.journal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.parallel.*
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.BuildInfo
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.catsutils.Environment
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.{Stopwatch, TestWallClock, WallClock}
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.event.{AnyKeyedEvent, EventCalc, EventId, SnapshotableState, Stamped}
import js7.journal.FileJournalTest.*
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.file
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.test.{TestAggregate, TestEvent, TestState}
import org.scalatest.Assertion
import scala.concurrent.duration.Deadline

final class FileJournalTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "Persist" in:
    testJournal(): journal =>
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

  "Massive parallel" - {
    "test empty EventCalc" in:
      run(
        n = if isIntelliJIdea then 1_000_000 else 10_000,
        persistLimit = 500,
        _ => Nil)

    "test" in:
      run(
        n = if isIntelliJIdea then 1_000_000 else 10_000,
        persistLimit = 500,
        i => (i.toString <-: TestEvent.SimpleAdded("A")) :: Nil)

    def run(n: Int, persistLimit: Int, toEvents: Int => Seq[AnyKeyedEvent]): IO[Assertion] =
      testJournal(config"""
        js7.journal.concurrent-persist-limit = $persistLimit
        js7.journal.slow-check-state = false"""
      ): journal =>
        (1 to n).toVector.parTraverse: i =>
          journal.persist(toEvents(i))
        .timed.flatMap: (duration, _) =>
          IO:
            info_(s"$n in parallel, concurrent-persist-limit=$persistLimit " +
              Stopwatch.itemsPerSecondString(duration, n, "commits"))
            succeed
  }

  private def testJournal(config: Config = ConfigFactory.empty)
    (tester: FileJournal[TestState] => IO[Assertion])
  : IO[Assertion] =
    val myConfig = config.withFallback(FileJournalTest.config)
    val clock = TestWallClock(ts"1970-01-01T00:00:00.001Z")
    locally:
      for
        _ <- Environment.registerPure[WallClock](clock)
        dir <- temporaryDirectoryResource[IO]("FileJournalTest-")
        journalLocation = JournalLocation(TestState, dir / "test")
        journal <- FileJournal.service(
          Recovered.noJournalFile[TestState](
            journalLocation,
            Deadline.now,
            myConfig),
          JournalConf.fromConfig(myConfig))
      yield
        journal
    .use: journal =>
      journal.failWhenStopped:
        tester(journal)

  private def info_(line: String) =
    logger.debug(s"🔵🔵🔵 $line")
    info(line)

object FileJournalTest:
  private val logger = Logger[this.type]

  private val config = config"""
    js7.journal.sync = on
    js7.journal.delay = 0s
    js7.journal.sync-delay = 0s
    js7.journal.simulate-sync = 1ms
    js7.journal.concurrent-persist-limit = 1024
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
