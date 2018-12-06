package com.sos.jobscheduler.core.event.journal.test

import akka.pattern.ask
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.blockingFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.core.event.journal.test.JournalTest._
import com.typesafe.config.ConfigFactory
import java.nio.file.Files.{delete, deleteIfExists}
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class JournalTest extends FreeSpec with BeforeAndAfterAll with TestJournalMixin {

  "First run" in {
    withTestActor() { (actorSystem, actor) ⇒
      for ((key, cmd) ← testCommands("TEST")) execute(actorSystem, actor, key, cmd) await 99.s
      assert(journalAggregates.isEmpty)
      assert(journalKeyedEvents == testEvents("TEST"))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C", "(C.Add)"))
    }
    assert(journalJsons == FirstJournal)
    assert(journalFileNames == Vector("test--0.journal"))
  }

  "Second run, recovering from journal, then taking snapshot" in {
    withTestActor() { (actorSystem, actor) ⇒
      assert(journalAggregates.isEmpty)
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C", "(C.Add)"))

      execute(actorSystem, actor, "TEST-D", TestAggregateActor.Command.Add("DDD")) await 99.s  // 1000066

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
      assert(journalKeyedEvents.isEmpty)

      assert(journalJsons(directory / "test--0.journal") == FirstJournal)  // Archived journal for history

      execute(actorSystem, actor, "TEST-A", TestAggregateActor.Command.Remove) await 99.s
    }
    assert(journalFileNames.length == 3)
  }

  "Third run, recovering from journal, no events" in {
    withTestActor() { (_, actor) ⇒
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "(C.Add)"),
        TestAggregate("TEST-D", "DDD"))
    }
    assert(journalFileNames.length == 4)
  }

  "After adding no events, journal file is reused (rewritten) because of unchanged after-EventId" in {
    withTestActor() { (_, _) ⇒ }
    assert(journalFileNames.length == 4)  // Unchanged
  }

  "acceptEarly" in {
    withTestActor() { (actorSystem, actor) ⇒
      def journalState = (actor ? TestActor.Input.GetJournalState).mapTo[JournalActor.Output.State] await 99.s

      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Add("A")) await 99.s
      assert(journalState == JournalActor.Output.State(isFlushed = true, isSynced = true))

      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.AcceptEarly) await 99.s
      //assert(journalState == JournalActor.Output.State(isFlushed = true, isSynced = true))

      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Append("Cc")) await 99.s
      assert(journalState == JournalActor.Output.State(isFlushed = true, isSynced = true))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "(C.Add)"),
        TestAggregate("TEST-D", "DDD"),
        TestAggregate("TEST-E", "ACc"))
    }
    assert(journalFileNames.length == 4)  // Unchanged
  }

  "Massive parallel" - {
    def run(n: Int, eventBufferSize: Int): Unit = {
      listJournalFiles(journalMeta.fileBase) map (_.file) foreach delete
      withTestActor(ConfigFactory.parseString(s"""jobscheduler.journal.event-buffer-size = $eventBufferSize""")) { (_, actor) ⇒
        val prefixes = for (i ← 1 to n) yield i.toString
        val stopwatch = new Stopwatch
        // Add "$p-A"
        (for (p ← prefixes) yield {
          val (key, cmd) = testCommands(p).head
          simpleExecute(actor, key, cmd)
        }) await 99.s
        // Start executing remaining commands ...
        val executed = for (p ← prefixes) yield blockingFuture { for ((key, cmd) ← testCommands(p).tail) simpleExecute(actor, key, cmd) await 99.s }
        // ... while disturbing form a different Actor to test persistAsync()
        // DisturbAndRespond responds with String, not Done. See TestActor
        val disturbed = for (p ← prefixes) yield simpleExecute(actor, s"$p-A", TestAggregateActor.Command.DisturbAndRespond)
        (executed ++ disturbed) await 99.s
        info(s"$n actors, event-buffer-size=$eventBufferSize " + stopwatch.itemsPerSecondString(n, "commands"))
        assert(journalAggregates.isEmpty)
        val prefixToKeyedEvents = journalKeyedEvents groupBy { _.key.split("-").head }
        assert(prefixToKeyedEvents.keySet == prefixes.toSet)
        for (p ← prefixes) assert(prefixToKeyedEvents(p) == testEvents(p))
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual
          prefixes.flatMap(p ⇒ Set(
            TestAggregate(s"$p-A", "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
            TestAggregate(s"$p-C", "(C.Add)"))
          ).toSet
      }
      assert(journalFileNames.length == 1)
    }

    for ((n, eventBufferSize) ← Array(1000 → 1000) ++ (if (sys.props.contains("test.speed")) Array(1000 → 300, 100 → 100, 100 → 30, 100 → 10) else Nil)) {
      s"$n actors, event-buffer-size=$eventBufferSize" in {
        run(n = n, eventBufferSize = eventBufferSize)
      }
    }
  }

  if (sys.props contains "test.speed")
  "Speed test" in {
    deleteIfExists(journalMeta.fileBase)
    val keys = for (i ← 1 to 100000) yield s"TEST-$i"
    withTestActor() { (_, actor) ⇒
      val stopwatch = new Stopwatch
      (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Add(s"CONTENT-FOR-$key"))) ++
        (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-a"))) ++
        (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-b"))) await 999.s
      info("Stored    " + stopwatch.itemsPerSecondString(3 * keys.size, "events"))
    }
    val stopwatch = new Stopwatch
    withTestActor() { (_, actor) ⇒
      (actor ? TestActor.Input.WaitUntilReady) await 999.s
      info("Recovered " + stopwatch.itemsPerSecondString(keys.size, "objects"))  // Including initial snapshots
      assertResult((for (key ← keys) yield TestAggregate(key, s"CONTENT-FOR-$key-a-b")).toSet) {
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet
      }
    }
  }

  private def journalFileNames =
    listJournalFiles(journalMeta.fileBase).map(_.file.getFileName.toString)
}

object JournalTest {
  private val FirstJournal = Vector(
    json"""{
      "TYPE": "JobScheduler.Journal",
      "version": "0.19",
      "softwareVersion": "2.0.0-SNAPSHOT",
      "buildId": "${BuildInfo.buildId}",
      "eventId": 0,
      "totalEventCount": 0,
      "timestamp": "TIMESTAMP"
    }""",
    json""""-------SNAPSHOTS-------"""",
    json""""-------END OF SNAPSHOTS-------"""",
    json""""-------EVENTS-------"""",
    json"""{ "eventId": 1000000, "key": "TEST-A", "TYPE": "Added", "string": "(A.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "eventId": 1000001, "key": "TEST-B", "TYPE": "Added", "string": "(B.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "eventId": 1000002, "key": "TEST-C", "TYPE": "Added", "string": "(C.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json""""TRANSACTION"""",
    json"""{ "eventId": 1000003, "key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000004, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000005, "key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000006, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000007, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000008, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000009, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000010, "key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000011, "key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000012, "key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json""""COMMIT"""",
    json"""{ "eventId": 1000013, "key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000014, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000015, "key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000016, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000017, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000018, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000019, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000020, "key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000021, "key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000022, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000023, "key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000024, "key": "TEST-A", "TYPE": "Appended", "char": "y" }""",
    json"""{ "eventId": 1000025, "key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000026, "key": "TEST-A", "TYPE": "Appended", "char": "c" }""",
    json"""{ "eventId": 1000027, "key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json"""{ "eventId": 1000028, "key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000029, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000030, "key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000031, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000032, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000033, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000034, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000035, "key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000036, "key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000037, "key": "TEST-A", "TYPE": "Appended", "char": "N" }""",
    json"""{ "eventId": 1000038, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000039, "key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000040, "key": "TEST-A", "TYPE": "Appended", "char": "t" }""",
    json"""{ "eventId": 1000041, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000042, "key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000043, "key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json"""{ "eventId": 1000044, "key": "TEST-A", "TYPE": "Appended", "char": "(" }""",
    json"""{ "eventId": 1000045, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000046, "key": "TEST-A", "TYPE": "Appended", "char": "." }""",
    json"""{ "eventId": 1000047, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000048, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000049, "key": "TEST-A", "TYPE": "Appended", "char": "p" }""",
    json"""{ "eventId": 1000050, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000051, "key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000052, "key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000053, "key": "TEST-A", "TYPE": "Appended", "char": "N" }""",
    json"""{ "eventId": 1000054, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000055, "key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000056, "key": "TEST-A", "TYPE": "Appended", "char": "t" }""",
    json"""{ "eventId": 1000057, "key": "TEST-A", "TYPE": "Appended", "char": "e" }""",
    json"""{ "eventId": 1000058, "key": "TEST-A", "TYPE": "Appended", "char": "d" }""",
    json"""{ "eventId": 1000059, "key": "TEST-A", "TYPE": "Appended", "char": "A" }""",
    json"""{ "eventId": 1000060, "key": "TEST-A", "TYPE": "Appended", "char": "s" }""",
    json"""{ "eventId": 1000061, "key": "TEST-A", "TYPE": "Appended", "char": "y" }""",
    json"""{ "eventId": 1000062, "key": "TEST-A", "TYPE": "Appended", "char": "n" }""",
    json"""{ "eventId": 1000063, "key": "TEST-A", "TYPE": "Appended", "char": "c" }""",
    json"""{ "eventId": 1000064, "key": "TEST-A", "TYPE": "Appended", "char": ")" }""",
    json"""{ "eventId": 1000065, "key": "TEST-B", "TYPE": "Removed" }""",
    json""""-------END OF EVENTS-------"""",
  )

  private val SecondJournal = Vector(
    json"""{
      "TYPE": "JobScheduler.Journal",
      "version": "0.19",
      "softwareVersion": "2.0.0-SNAPSHOT",
      "buildId": "${BuildInfo.buildId}",
      "eventId": 1000066,
      "totalEventCount": 67,
      "timestamp": "TIMESTAMP"
    }""",
    json""""-------SNAPSHOTS-------"""",
    json"""{ "TYPE": "TestAggregate", "key": "TEST-A", "string": "(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "TYPE": "TestAggregate", "key": "TEST-C", "string": "(C.Add)",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json"""{ "TYPE": "TestAggregate", "key": "TEST-D", "string": "DDD",
      "a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X", "h": "X", "i": "X", "j": "X", "k": "X", "l": "X", "m": "X", "n": "X", "o": "X", "p": "X", "q": "X", "r": "X" }""",
    json""""-------END OF SNAPSHOTS-------"""",
    json""""-------EVENTS-------"""")
}
