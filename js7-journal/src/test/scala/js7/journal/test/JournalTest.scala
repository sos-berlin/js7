package js7.journal.test

import akka.pattern.ask
import java.nio.file.Files.{delete, deleteIfExists}
import js7.base.BuildInfo
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.blockingThreadFuture
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.data.event.JournalHeader
import js7.journal.JournalActor
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.test.JournalTest.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers.*
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class JournalTest extends OurTestSuite with BeforeAndAfterAll with TestJournalMixin
{
  "First run" in {
    withTestActor() { (actorSystem, actor) =>
      for ((key, cmd) <- testCommands("TEST")) execute(actorSystem, actor, key, cmd) await 99.s
      assert(journalAggregates.isEmpty)
      assert(journalKeyedTestEvents == testEvents("TEST"))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C", "(C.Add)"))
    }
    assert(journalJsons == FirstJournal)
    assert(journalFileNames == Vector("test--0.journal"))
  }

  "Second run, recovering from journal, then taking snapshot" in {
    withTestActor() { (actorSystem, actor) =>
      assert(journalAggregates.isEmpty)
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C", "(C.Add)"))

      execute(actorSystem, actor, "TEST-D", TestAggregateActor.Command.Add("DDD")) await 99.s  // 1000068

      (actor ? TestActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] await 99.s
      assert({
          val jsons = journalJsons
          jsons.slice(0, 2) ++
            jsons.slice(2, 5).sortBy(_.asObject.map(_("key").map(_.asString))) ++  // Make snapshots comparable
            jsons.drop(5)
        } == SecondJournal)
      assert(journalAggregates == Set(
        TestAggregate("TEST-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C", "(C.Add)"),
        TestAggregate("TEST-D", "DDD")))
      assert(journalKeyedTestEvents.isEmpty)

      assert(journalJsons(directory / "test--0.journal") == FirstJournal)  // Archived journal for history

      execute(actorSystem, actor, "TEST-A", TestAggregateActor.Command.Remove) await 99.s
    }
    assert(journalFileNames == Vector("test--0.journal", "test--1000066.journal", "test--1000068.journal"))
  }

  "Third run, recovering from journal, no events" in {
    withTestActor() { (_, actor) =>
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "(C.Add)"),
        TestAggregate("TEST-D", "DDD"))
    }
    assert(journalFileNames.length == 4)
  }

  "With each snapshot a JournalWritten event is written to increment the EventId and force a new journal file" in {
    withTestActor() { (_, _) => }
    assert(journalFileNames.length == 5)
  }

  "acceptEarly" in {
    withTestActor() { (actorSystem, actor) =>
      def journalState = (actor ? TestActor.Input.GetJournalState).mapTo[JournalActor.Output.JournalActorState] await 99.s

      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Add("A")) await 99.s
      assert(journalState == JournalActor.Output.JournalActorState(isFlushed = true, isSynced = true, isRequiringClusterAcknowledgement = false))

      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.AcceptEarly) await 99.s
      //assert(journalState == JournalActor.Output.JournalActorState(isFlushed = true, isSynced = true))

      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Append("Cc")) await 99.s
      assert(journalState == JournalActor.Output.JournalActorState(isFlushed = true, isSynced = true, isRequiringClusterAcknowledgement = false))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "(C.Add)"),
        TestAggregate("TEST-D", "DDD"),
        TestAggregate("TEST-E", "ACc"))
    }
    assert(journalFileNames.length == 6)
  }

  "persist empty event list" in {
    withTestActor() { (actorSystem, actor) =>
      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.AppendEmpty) await 99.s
      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Append("!")) await 99.s
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "(C.Add)"),
        TestAggregate("TEST-D", "DDD"),
        TestAggregate("TEST-E", "ACc!"))
    }
    assert(journalFileNames.length == 7)
  }

  "Massive parallel" - {
    def run(n: Int, coalesceEventLimit: Int): Unit = {
      journalMeta.listJournalFiles.map(_.file) foreach delete
      val config = config"""
       js7.journal.coalesce-event-limit = $coalesceEventLimit
       js7.journal.slow-check-state = false"""
      withTestActor(config) { (_, actor) =>
        val prefixes = for (i <- 1 to n) yield i.toString
        val stopwatch = new Stopwatch
        // Add "$p-A"
        (for (p <- prefixes) yield {
          val (key, cmd) = testCommands(p).head
          simpleExecute(actor, key, cmd)
        }) await 99.s
        // Start executing remaining commands ...
        val executed = for (p <- prefixes) yield blockingThreadFuture { for ((key, cmd) <- testCommands(p).tail) simpleExecute(actor, key, cmd) await 99.s }
        // ... while disturbing form a different Actor to test persistAsync()
        // DisturbAndRespond responds with String, not Done. See TestActor
        val disturbed = for (p <- prefixes) yield simpleExecute(actor, s"$p-A", TestAggregateActor.Command.DisturbAndRespond)
        (executed ++ disturbed) await 99.s
        info(s"$n actors, coalesce-event-limit=$coalesceEventLimit " + stopwatch.itemsPerSecondString(n, "commands"))
        assert(journalAggregates.isEmpty)
        val prefixToKeyedEvents = journalKeyedTestEvents.groupBy(_.key.split("-").head)
        assert(prefixToKeyedEvents.keySet == prefixes.toSet)
        for (p <- prefixes) assert(prefixToKeyedEvents(p) == testEvents(p))
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual
          prefixes.flatMap(p => Set(
            TestAggregate(s"$p-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
            TestAggregate(s"$p-C", "(C.Add)"))
          ).toSet
      }
      assert(journalFileNames.length == 1)
    }

    for ((n, coalesceEventLimit) <- Vector(1000 -> 1000) ++ (if (sys.props.contains("test.speed")) Array(1000 -> 300, 100 -> 100, 100 -> 30, 100 -> 10) else Nil)) {
      s"$n actors, coalesce-event-limit=$coalesceEventLimit" in {
        run(n = n, coalesceEventLimit = coalesceEventLimit)
      }
    }
  }

  if (sys.props contains "test.speed")
  "Speed test" in {
    deleteIfExists(journalMeta.fileBase)
    val keys = for (i <- 1 to 100000) yield s"TEST-$i"
    withTestActor() { (_, actor) =>
      val stopwatch = new Stopwatch
      (for (key <- keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Add(s"CONTENT-FOR-$key"))) ++
        (for (key <- keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-a"))) ++
        (for (key <- keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-b"))) await 999.s
      info("Stored    " + stopwatch.itemsPerSecondString(3 * keys.size, "events"))
    }
    val stopwatch = new Stopwatch
    withTestActor() { (_, actor) =>
      (actor ? TestActor.Input.WaitUntilReady) await 999.s
      info("Recovered " + stopwatch.itemsPerSecondString(keys.size, "objects"))  // Including initial snapshots
      assertResult((for (key <- keys) yield TestAggregate(key, s"CONTENT-FOR-$key-a-b")).toSet) {
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet
      }
    }
  }

  private def journalFileNames =
    journalMeta.listJournalFiles.map(_.file.getFileName.toString)
}

object JournalTest
{
  private val FirstJournal = Vector(
    json"""{
      "TYPE": "JS7.Journal",
      "typeName": "TestState",
      "journalId" : "ABEiM0RVZneImaq7zN3u_w",
      "eventId": 0,
      "generation": 1,
      "totalEventCount": 0,
      "totalRunningTime": 3600,
      "initiallyStartedAt": "STARTED-AT",
      "timestamp": "TIMESTAMP",
      "version": "${JournalHeader.Version}",
      "js7Version": "${BuildInfo.longVersion}",
      "buildId": "${BuildInfo.buildId}"
    }""",
    json""""-------SNAPSHOT-------"""",
    json""""-------END OF SNAPSHOT-------"""",
    json""""-------EVENTS-------"""",
    json"""{ "eventId": 1000000, "TYPE": "SnapshotTaken" }""",
    json"""{ "eventId": 1000001, "Key": "TEST-A", "TYPE": "Added", "string": "(A.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "eventId": 1000002, "Key": "TEST-B", "TYPE": "Added", "string": "(B.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "eventId": 1000003, "Key": "TEST-C", "TYPE": "Added", "string": "(C.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json""""TRANSACTION"""",
    json"""{ "eventId": 1000004, "Key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000005, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000006, "Key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000007, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000008, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000009, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000010, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000011, "Key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000012, "Key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000013, "Key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json""""COMMIT"""",
    json"""{ "eventId": 1000014, "Key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000015, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000016, "Key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000017, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000018, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000019, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000020, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000021, "Key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000022, "Key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000023, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000024, "Key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000025, "Key": "TEST-A", "TYPE": "Appended", "char": "y" }""",
    json"""{ "eventId": 1000026, "Key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000027, "Key": "TEST-A", "TYPE": "Appended", "char": "c" }""",
    json"""{ "eventId": 1000028, "Key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json"""{ "eventId": 1000029, "Key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000030, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000031, "Key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000032, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000033, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000034, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000035, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000036, "Key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000037, "Key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000038, "Key": "TEST-A", "TYPE": "Appended", "char": "N" }""",
    json"""{ "eventId": 1000039, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000040, "Key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000041, "Key": "TEST-A", "TYPE": "Appended", "char": "t" }""",
    json"""{ "eventId": 1000042, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000043, "Key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000044, "Key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json"""{ "eventId": 1000045, "Key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000046, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000047, "Key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000048, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000049, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000050, "Key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000051, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000052, "Key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000053, "Key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000054, "Key": "TEST-A", "TYPE": "Appended", "char": "N" }""",
    json"""{ "eventId": 1000055, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000056, "Key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000057, "Key": "TEST-A", "TYPE": "Appended", "char": "t" }""",
    json"""{ "eventId": 1000058, "Key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000059, "Key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000060, "Key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000061, "Key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000062, "Key": "TEST-A", "TYPE": "Appended", "char": "y" }""",
    json"""{ "eventId": 1000063, "Key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000064, "Key": "TEST-A", "TYPE": "Appended", "char": "c" }""",
    json"""{ "eventId": 1000065, "Key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json"""{ "eventId": 1000066, "Key": "TEST-B", "TYPE": "Removed" }""",
  )

  private val SecondJournal = Vector(
    json"""{
      "TYPE": "JS7.Journal",
      "typeName": "TestState",
      "version": "${JournalHeader.Version}",
      "js7Version": "${BuildInfo.longVersion}",
      "buildId": "${BuildInfo.buildId}",
      "journalId": "ABEiM0RVZneImaq7zN3u_w",
      "eventId": 1000068,
      "totalEventCount": 69,
      "initiallyStartedAt" : "STARTED-AT",
      "totalRunningTime" : 3600,
      "generation": 3,
      "timestamp": "TIMESTAMP"
    }""",
    json""""-------SNAPSHOT-------"""",
    json"""{ "TYPE": "TestAggregate", "key": "TEST-A", "string": "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "TYPE": "TestAggregate", "key": "TEST-C", "string": "(C.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "TYPE": "TestAggregate", "key": "TEST-D", "string": "DDD",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json""""-------END OF SNAPSHOT-------"""",
    json""""-------EVENTS-------"""",
    json"""{ "eventId": 1000069, "TYPE": "SnapshotTaken" }""",
  )
}
