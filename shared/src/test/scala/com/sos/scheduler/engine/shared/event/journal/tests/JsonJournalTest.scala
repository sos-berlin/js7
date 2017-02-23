package com.sos.scheduler.engine.shared.event.journal.tests

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.common.BuildInfo
import com.sos.scheduler.engine.common.akkautils.DeadLetterActor
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, KeyedEvent, Snapshot}
import com.sos.scheduler.engine.shared.common.jsonseq.InputStreamJsonSeqIterator
import com.sos.scheduler.engine.shared.event.journal.Journal
import com.sos.scheduler.engine.shared.event.journal.tests.JsonJournalTest._
import com.sos.scheduler.engine.shared.event.journal.tests.TestJsonFormats.TestKeyedEventJsonFormat
import java.io.FileInputStream
import java.nio.file.Files.createTempDirectory
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration.DurationInt
import spray.json.{JsObject, JsString, JsValue}

/**
  * @author Joacim Zschimmer
  */
final class JsonJournalTest extends FreeSpec with BeforeAndAfterAll {

  private implicit val askTimeout = Timeout(15.seconds)
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
    (actor ? (key → command)) await 99.s
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
      val iterator = new InputStreamJsonSeqIterator(in)
      if (iterator.hasNext) {
        val header = iterator.next()
        assert(header == JsObject("TYPE" → JsString("JobScheduler.Journal"), "version" → JsString(BuildInfo.version)))
      }
      iterator.toVector
    }
}

object JsonJournalTest {
  private val logger = Logger(getClass)
}
