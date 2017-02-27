package com.sos.jobscheduler.shared.event.journal.tests

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Collections.RichGenericCompanion
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, KeyedEvent, Snapshot}
import com.sos.jobscheduler.shared.common.jsonseq.InputStreamJsonSeqIterator
import com.sos.jobscheduler.shared.event.journal.Journal
import com.sos.jobscheduler.shared.event.journal.tests.JsonJournalTest._
import com.sos.jobscheduler.shared.event.journal.tests.TestJsonFormats.TestKeyedEventJsonFormat
import java.io.{EOFException, FileInputStream}
import java.nio.file.Files.{createTempDirectory, deleteIfExists}
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
      execute(actor, "A", TestAggregateActor.Command.Add("AAA"))
      execute(actor, "B", TestAggregateActor.Command.Add("BBB"))
      execute(actor, "C", TestAggregateActor.Command.Add("CCC"))
      execute(actor, "A", TestAggregateActor.Command.Append("aaa"))
      execute(actor, "B", TestAggregateActor.Command.Remove)
      assert(journalAggregates.isEmpty)
      assert(journalKeyedEvents == Vector(
        KeyedEvent(TestEvent.Added("AAA"))("A"),
        KeyedEvent(TestEvent.Added("BBB"))("B"),
        KeyedEvent(TestEvent.Added("CCC"))("C"),
        KeyedEvent(TestEvent.Appended("aaa"))("A"),
        KeyedEvent(TestEvent.Removed)("B")))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("A", "AAAaaa"),
        TestAggregate("C", "CCC"))
    }
  }

  "Second run, recovering from journal, then taking snapshot" in {
    withTestActor { actor ⇒
      assert(journalAggregates.isEmpty)
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("A", "AAAaaa"),
        TestAggregate("C", "CCC"))

      execute(actor, "D", TestAggregateActor.Command.Add("DDD"))

      (actor ? Journal.Input.TakeSnapshot).mapTo[Journal.Output.SnapshotTaken.type] await 99.s
      assert(journalAggregates == Set(
        TestAggregate("A", "AAAaaa"),
        TestAggregate("C", "CCC"),
        TestAggregate("D", "DDD")))
      assert(journalKeyedEvents.isEmpty)

      execute(actor, "A", TestAggregateActor.Command.Remove)
    }
  }

  "Third run, recovering from snapshot and journal" in {
    withTestActor { actor ⇒
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("C", "CCC"),
        TestAggregate("D", "DDD"))
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

  private def execute(actor: ActorRef, key: String, command: TestAggregateActor.Command): Unit = {
    (actor ? TestActor.Input.Forward(key, command)) await 99.s
  }

  private def journalKeyedEvents =
    journalJsValues collect {
      case o: JsObject if TestKeyedEventJsonFormat canDeserialize o ⇒
        o.convertTo[Snapshot[AnyKeyedEvent]].value
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
          "version" → JsString("1"),
          "softwareVersion" → JsString(BuildInfo.version)))
      }
      Vector.build[JsValue] { builder ⇒
        try iterator foreach builder.+=
        catch {
          case _: EOFException ⇒ None
          case t: java.util.zip.ZipException ⇒ None
        }
      }
    }
}

object JsonJournalTest {
  private val logger = Logger(getClass)
}
