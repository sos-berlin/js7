package com.sos.jobscheduler.shared.event.journal.tests

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.utils.Collections.RichGenericCompanion
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.blockingFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.common.jsonseq.InputStreamJsonSeqIterator
import com.sos.jobscheduler.shared.event.journal.JsonJournalActor
import com.sos.jobscheduler.shared.event.journal.tests.JsonJournalTest._
import com.sos.jobscheduler.shared.event.journal.tests.TestJsonFormats.TestKeyedEventJsonFormat
import java.io.{EOFException, FileInputStream}
import java.nio.file.Files.{createTempDirectory, delete, deleteIfExists}
import java.util.zip.GZIPInputStream
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import spray.json.{JsObject, JsString, JsValue}

/**
  * @author Joacim Zschimmer
  */
final class JsonJournalTest extends FreeSpec with BeforeAndAfterAll {

  private implicit val askTimeout = Timeout(999.seconds)
  private lazy val directory = createTempDirectory("JsonJournalTest-")
  private lazy val journalFile = directory / "journal"

  override def afterAll() = {
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  "First run" in {
    withTestActor { actor ⇒
      for ((key, cmd) ← testCommands("TEST")) execute(actor, key, cmd) await 99.s
      assert(journalAggregates.isEmpty)
      assert(journalKeyedEvents == testEvents("TEST"))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "AAAabcdefghijkl"),
        TestAggregate("TEST-C", "CCC"))
    }
  }

  "Second run, recovering from journal, then taking snapshot" in {
    withTestActor { actor ⇒
      assert(journalAggregates.isEmpty)
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "AAAabcdefghijkl"),
        TestAggregate("TEST-C", "CCC"))

      execute(actor, "TEST-D", TestAggregateActor.Command.Add("DDD")) await 99.s

      (actor ? TestActor.Input.TakeSnapshot).mapTo[JsonJournalActor.Output.SnapshotTaken.type] await 99.s
      assert(journalAggregates == Set(
        TestAggregate("TEST-A", "AAAabcdefghijkl"),
        TestAggregate("TEST-C", "CCC"),
        TestAggregate("TEST-D", "DDD")))
      assert(journalKeyedEvents.isEmpty)

      execute(actor, "TEST-A", TestAggregateActor.Command.Remove) await 99.s
    }
  }

  "Third run, recovering from snapshot and journal" in {
    withTestActor { actor ⇒
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "CCC"),
        TestAggregate("TEST-D", "DDD"))
    }
  }

  "noSync" in {
    withTestActor { actor ⇒
      def journalState = (actor ? TestActor.Input.GetJournalState).mapTo[JsonJournalActor.Output.State] await 99.s
      execute(actor, "TEST-E", TestAggregateActor.Command.Add("A"))  await 99.s
      assert(journalState == JsonJournalActor.Output.State(isFlushed = true, isSynced = true))
      execute(actor, "TEST-E", TestAggregateActor.Command.AppendNoSync("Bb")) await 99.s
      assert(journalState == JsonJournalActor.Output.State(isFlushed = true, isSynced = false))
      execute(actor, "TEST-E", TestAggregateActor.Command.Append("C")) await 99.s
      assert(journalState == JsonJournalActor.Output.State(isFlushed = true, isSynced = true))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "CCC"),
        TestAggregate("TEST-D", "DDD"),
        TestAggregate("TEST-E", "ABbC"))
    }
  }

  "Massive parallel" in {
    for (_ ← 1 to 10) {
      delete(journalFile)
      withTestActor { actor ⇒
        val prefixes = for (i ← 1 to 1000) yield i.toString
        // Add "$p-A"
        (for (p ← prefixes) yield {
          val (key, cmd) = testCommands(p).head
          execute(actor, key, cmd)
        }) await 99.s
        // Start executing remaining commands ...
        val executed = for (p ← prefixes) yield blockingFuture { for ((key, cmd) ← testCommands(p).tail) execute(actor, key, cmd) await 99.s }
        // ... while disturbing form a different Actor to test persistAsync()
        // Disturb responds with String, not Done. See TestActor
        val disturbed = for (p ← prefixes) yield execute(actor, s"$p-A", TestAggregateActor.Command.Disturb)
        (executed ++ disturbed) await 99.s
        assert(journalAggregates.isEmpty)
        val prefixToKeyedEvents = journalKeyedEvents groupBy { _.key.split("-").head }
        assert(prefixToKeyedEvents.keySet == prefixes.toSet)
        for (p ← prefixes) assert(prefixToKeyedEvents(p) == testEvents(p))
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual
          (prefixes flatMap { p ⇒ Set(
            TestAggregate(s"$p-A", "AAAabcdefghijkl"),
            TestAggregate(s"$p-C", "CCC")) }
          ).toSet
      }
    }
  }

  if (sys.props contains "test.speed") "Speed test" in {
    deleteIfExists(journalFile)
    val keys = for (i ← 1 to 100000) yield s"TEST-$i"
    withTestActor { actor ⇒
      val stopwatch = new Stopwatch
      (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Add(s"CONTENT-FOR-$key"))) ++
        (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-a"))) ++
        (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-b"))) await 999.s
      info("Stored    " + stopwatch.itemsPerSecondString(3 * keys.size, "events"))
    }
    val stopwatch = new Stopwatch
    withTestActor { actor ⇒
      (actor ? TestActor.Input.WaitUntilReady) await 999.s
      info("Recovered " + stopwatch.itemsPerSecondString(keys.size, "objects"))  // Including initial snapshots
      assertResult((for (key ← keys) yield TestAggregate(key, s"CONTENT-FOR-$key-a-b")).toSet) {
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet
      }
    }
  }

  private def withTestActor(body: ActorRef ⇒ Unit): Unit = {
    val actorSystem = ActorSystem("JsonJournalTest")
    try {
      DeadLetterActor.subscribe(actorSystem, o ⇒ logger.warn(o))
      val actor = actorSystem.actorOf(Props { new TestActor(journalFile) }, "TestActor")
      body(actor)
    }
    finally actorSystem.terminate() await 99.s
  }

  private def execute(actor: ActorRef, key: String, command: TestAggregateActor.Command) =
    actor ? TestActor.Input.Forward(key, command)

  private def journalKeyedEvents =
    journalJsValues collect {
      case o: JsObject if TestKeyedEventJsonFormat canDeserialize o ⇒
        o.convertTo[Stamped[KeyedEvent[TestEvent]]].value
    }

  private def journalAggregates =
    (journalJsValues collect {
      case o: JsObject if TestActor.SnapshotJsonFormat canDeserialize o ⇒
        o.convertTo[TestAggregate]
    }).toSet

  private def journalJsValues: Vector[JsValue] =
    autoClosing(new FileInputStream(journalFile)) { in ⇒
      val iterator = new InputStreamJsonSeqIterator(new GZIPInputStream(in))
      if (iterator.hasNext) {
        val header = iterator.next()
        assert(header == JsObject(
          "TYPE" → JsString("JobScheduler.Journal"),
          "version" → JsString("0.1"),
          "softwareVersion" → JsString(BuildInfo.version)))
      }
      Vector.build[JsValue] { builder ⇒
        try iterator foreach builder.+=
        catch {
          case _: EOFException ⇒ None
          case _: java.util.zip.ZipException ⇒ None
        }
      }
    }
}

object JsonJournalTest {
  private val logger = Logger(getClass)

  private def testCommands(prefix: String) = Vector(
    s"$prefix-A" → TestAggregateActor.Command.Add("AAA"),
    s"$prefix-B" → TestAggregateActor.Command.Add("BBB"),
    s"$prefix-C" → TestAggregateActor.Command.Add("CCC"),
    s"$prefix-A" → TestAggregateActor.Command.Append("abc"),
    s"$prefix-A" → TestAggregateActor.Command.Append(""),
    s"$prefix-A" → TestAggregateActor.Command.AppendAsync("def"),
    s"$prefix-A" → TestAggregateActor.Command.AppendNested("ghi"),
    s"$prefix-A" → TestAggregateActor.Command.AppendNestedAsync("jkl"),
    s"$prefix-B" → TestAggregateActor.Command.Remove)

  private def testEvents(prefix: String) = Vector(
    KeyedEvent(TestEvent.Added("AAA"))(s"$prefix-A"),
    KeyedEvent(TestEvent.Added("BBB"))(s"$prefix-B"),
    KeyedEvent(TestEvent.Added("CCC"))(s"$prefix-C"),
    KeyedEvent(TestEvent.Appended('a'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('b'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('c'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('d'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('e'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('f'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('g'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('h'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('i'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('j'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('k'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Appended('l'))(s"$prefix-A"),
    KeyedEvent(TestEvent.Removed)(s"$prefix-B"))
}
