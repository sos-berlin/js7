package com.sos.jobscheduler.core.event.journal.tests

import akka.Done
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.utils.Collections.RichGenericCompanion
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.blockingFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqIterator
import com.sos.jobscheduler.core.event.journal.tests.JournalTest._
import com.sos.jobscheduler.core.event.journal.tests.TestJsonCodecs.TestKeyedEventJsonCodec
import com.sos.jobscheduler.core.event.journal.{JournalActor, JournalMeta}
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import io.circe.Json
import java.io.{EOFException, FileInputStream}
import java.nio.file.Files.{createTempDirectory, delete, deleteIfExists}
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.GZIPInputStream
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalTest extends FreeSpec with BeforeAndAfterAll {

  private implicit val askTimeout = Timeout(999.seconds)
  private lazy val directory = createTempDirectory("JournalTest-")
  private lazy val journalFile = directory / "journal"

  override def afterAll() = {
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  "First run" in {
    withTestActor { (actorSystem, actor) ⇒
      for ((key, cmd) ← testCommands("TEST")) execute(actorSystem, actor, key, cmd) await 99.s
      assert(journalAggregates.isEmpty)
      assert(journalKeyedEvents == testEvents("TEST"))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "AAAabcdefghijkl"),
        TestAggregate("TEST-C", "CCC"))
    }
  }

  "Second run, recovering from journal, then taking snapshot" in {
    withTestActor { (actorSystem, actor) ⇒
      assert(journalAggregates.isEmpty)
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-A", "AAAabcdefghijkl"),
        TestAggregate("TEST-C", "CCC"))

      execute(actorSystem, actor, "TEST-D", TestAggregateActor.Command.Add("DDD")) await 99.s

      (actor ? TestActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] await 99.s
      assert(journalAggregates == Set(
        TestAggregate("TEST-A", "AAAabcdefghijkl"),
        TestAggregate("TEST-C", "CCC"),
        TestAggregate("TEST-D", "DDD")))
      assert(journalKeyedEvents.isEmpty)

      execute(actorSystem, actor, "TEST-A", TestAggregateActor.Command.Remove) await 99.s
    }
  }

  "Third run, recovering from snapshot and journal" in {
    withTestActor { (_, actor) ⇒
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "CCC"),
        TestAggregate("TEST-D", "DDD"))
    }
  }

  "noSync" in {
    withTestActor { (actorSystem, actor) ⇒
      def journalState = (actor ? TestActor.Input.GetJournalState).mapTo[JournalActor.Output.State] await 99.s
      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Add("A"))  await 99.s
      assert(journalState == JournalActor.Output.State(isFlushed = true, isSynced = true))
      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.AppendNoSync("Bb")) await 99.s
      assert(journalState == JournalActor.Output.State(isFlushed = true, isSynced = false))
      execute(actorSystem, actor, "TEST-E", TestAggregateActor.Command.Append("C")) await 99.s
      assert(journalState == JournalActor.Output.State(isFlushed = true, isSynced = true))
      ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet shouldEqual Set(
        TestAggregate("TEST-C", "CCC"),
        TestAggregate("TEST-D", "DDD"),
        TestAggregate("TEST-E", "ABbC"))
    }
  }

  "Massive parallel" - {
    val n = 1  // TODO More iterations for long-running test (tagged test or system property?)
    for (runIndex ← 1 to n) s"#$runIndex" in {
      delete(journalFile)
      withTestActor { (_, actor) ⇒
        val prefixes = for (i ← 1 to 1000) yield i.toString
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

  if (sys.props contains "test.speed")
  "Speed test" in {
    deleteIfExists(journalFile)
    val keys = for (i ← 1 to 100000) yield s"TEST-$i"
    withTestActor { (_, actor) ⇒
      val stopwatch = new Stopwatch
      (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Add(s"CONTENT-FOR-$key"))) ++
        (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-a"))) ++
        (for (key ← keys) yield actor ? TestActor.Input.Forward(key, TestAggregateActor.Command.Append(s"-b"))) await 999.s
      info("Stored    " + stopwatch.itemsPerSecondString(3 * keys.size, "events"))
    }
    val stopwatch = new Stopwatch
    withTestActor { (_, actor) ⇒
      (actor ? TestActor.Input.WaitUntilReady) await 999.s
      info("Recovered " + stopwatch.itemsPerSecondString(keys.size, "objects"))  // Including initial snapshots
      assertResult((for (key ← keys) yield TestAggregate(key, s"CONTENT-FOR-$key-a-b")).toSet) {
        ((actor ? TestActor.Input.GetAll).mapTo[Vector[TestAggregate]] await 99.s).toSet
      }
    }
  }

  private def withTestActor(body: (ActorSystem, ActorRef) ⇒ Unit): Unit = {
    val actorSystem = newActorSystem("JournalTest")
    try {
      DeadLetterActor.subscribe(actorSystem, o ⇒ logger.warn(o))
      val whenJournalStopped = Promise[JournalActor.Stopped]()
      val actor = actorSystem.actorOf(Props { new TestActor(journalFile, whenJournalStopped) }, "TestActor")
      body(actorSystem, actor)
      (actor ? TestActor.Input.Terminate) await 99.s
      actorSystem.stop(actor)
      assert(whenJournalStopped.future.await(99.s) == JournalActor.Stopped(keyedEventJournalingActorCount = 0))  // No memory leak
    }
    finally actorSystem.terminate() await 99.s
  }

  private def simpleExecute(actor: ActorRef, key: String, command: TestAggregateActor.Command) =
    actor ? TestActor.Input.Forward(key, command)

  private def execute(actorSystem: ActorSystem, actor: ActorRef, key: String, command: TestAggregateActor.Command): Future[Any] = {
    val promise = Promise[Any]()
    actorSystem.actorOf(Props {
      new Actor {
        val before = disturbanceCounter.getAndAdd(2)
        command match {
          case _: TestAggregateActor.Command.Add ⇒
            actor ! TestActor.Input.Forward(key, command)

          case TestAggregateActor.Command.Remove ⇒
            actor ! TestActor.Input.Forward(key, command)

          case _ ⇒
            actor ! TestActor.Input.Forward(key, TestAggregateActor.Command.Disturb(before))  // Value disturbance is expected as Command.Response
            actor ! TestActor.Input.Forward(key, command)
            actor ! TestActor.Input.Forward(key, TestAggregateActor.Command.Disturb(before + 1))  // This message must be delayed until event has been journaled
        }

        def receive = {
          case msg @ TestAggregateActor.Response.Completed(disturbance) ⇒
            try {
              command match {
                case _: TestAggregateActor.Command.Add ⇒
                case TestAggregateActor.Command.Remove ⇒
                case _: TestAggregateActor.Command.IsAsync ⇒ assert(disturbance == before + 1, s" - Disturbance expected: $command -> $msg")
                case _ ⇒ assert(disturbance == before, s" - persist operation has been disturbed: $command -> $msg")
              }
              promise.success(())
            }
            catch {
              case NonFatal(t) ⇒ promise.failure(t)
            }

          case msg @ ("OK" | Done) ⇒
            promise.success(msg)
            context.stop(self)
        }
      }
    })
    promise.future
  }

  private def journalKeyedEvents =
    journalJsons collect {
      case o if TestKeyedEventJsonCodec canDeserialize o ⇒
        o.as[Stamped[KeyedEvent[TestEvent]]].map(_.value).force
    }

  private def journalAggregates =
    (journalJsons collect {
      case o if TestActor.SnapshotJsonFormat canDeserialize o ⇒
        o.as[TestAggregate].force
    }).toSet

  private def journalJsons: Vector[Json] =
    autoClosing(new FileInputStream(journalFile)) { in ⇒
      val iterator = new InputStreamJsonSeqIterator(new GZIPInputStream(in))
      if (iterator.hasNext) {
        val header = iterator.next()
        assert(header == Json.obj(
          "TYPE" → Json.fromString("JobScheduler.Journal"),
          "version" → Json.fromString(JournalMeta.Header.version),
          "softwareVersion" → Json.fromString(BuildInfo.version)))
      }
      Vector.build[Json] { builder ⇒
        try iterator foreach builder.+=
        catch {
          case _: EOFException ⇒ None
          case _: java.util.zip.ZipException ⇒ None
        }
      }
    }
}

object JournalTest {
  private val logger = Logger(getClass)
  private val disturbanceCounter = new AtomicInteger

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
    s"$prefix-A" <-: TestEvent.Added("AAA"),
    s"$prefix-B" <-: TestEvent.Added("BBB"),
    s"$prefix-C" <-: TestEvent.Added("CCC"),
    s"$prefix-A" <-: TestEvent.Appended('a'),
    s"$prefix-A" <-: TestEvent.Appended('b'),
    s"$prefix-A" <-: TestEvent.Appended('c'),
    s"$prefix-A" <-: TestEvent.Appended('d'),
    s"$prefix-A" <-: TestEvent.Appended('e'),
    s"$prefix-A" <-: TestEvent.Appended('f'),
    s"$prefix-A" <-: TestEvent.Appended('g'),
    s"$prefix-A" <-: TestEvent.Appended('h'),
    s"$prefix-A" <-: TestEvent.Appended('i'),
    s"$prefix-A" <-: TestEvent.Appended('j'),
    s"$prefix-A" <-: TestEvent.Appended('k'),
    s"$prefix-A" <-: TestEvent.Appended('l'),
    s"$prefix-B" <-: TestEvent.Removed)
}
