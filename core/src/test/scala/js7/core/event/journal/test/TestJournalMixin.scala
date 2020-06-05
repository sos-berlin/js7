package js7.core.event.journal.test

import akka.Done
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.{RichJavaClass, RichThrowableEither}
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.akkautils.DeadLetterActor
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.core.common.jsonseq.InputStreamJsonSeqReader
import js7.core.event.journal.JournalActor
import js7.core.event.journal.files.JournalFiles
import js7.core.event.journal.test.TestData.testJournalMeta
import js7.core.event.journal.test.TestJournalMixin._
import js7.core.event.journal.test.TestJsonCodecs.TestKeyedEventJsonCodec
import js7.data.event.{Event, KeyedEvent, Stamped}
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.io.EOFException
import java.nio.file.Files.createTempDirectory
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] trait TestJournalMixin extends BeforeAndAfterAll { this: Suite =>

  protected implicit val askTimeout = Timeout(99.seconds)
  protected lazy val directory = createTempDirectory("JournalTest-")
  private val disturbanceCounter = new AtomicInteger

  protected val journalMeta = testJournalMeta(directory / "test")

  override def afterAll() = {
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  protected def withTestActor(config: Config = ConfigFactory.empty)(body: (ActorSystem, ActorRef) => Unit): Unit = {
    val actorSystem = newActorSystem(getClass.simpleScalaName, TestConfig)
    try {
      DeadLetterActor.subscribe(actorSystem, o => logger.warn(o))
      val whenJournalStopped = Promise[JournalActor.Stopped]()
      val actor = actorSystem.actorOf(Props { new TestActor(config, journalMeta, whenJournalStopped) }, "TestActor")
      body(actorSystem, actor)
      sleep(100.ms)  // Wait to let Terminated message of aggregate actors arrive at JournalActor (???)
      (actor ? TestActor.Input.Terminate) await 99.s
      assert(whenJournalStopped.future.await(99.s) == JournalActor.Stopped(keyedEventJournalingActorCount = 0))  // No memory leak
    }
    finally actorSystem.terminate() await 99.s
  }

  protected final def simpleExecute(actor: ActorRef, key: String, command: TestAggregateActor.Command) =
    actor ? TestActor.Input.Forward(key, command)

  protected final def execute(actorSystem: ActorSystem, actor: ActorRef, key: String, command: TestAggregateActor.Command): Future[Any] = {
    val promise = Promise[Any]()
    actorSystem.actorOf(Props {
      new Actor {
        val before = disturbanceCounter.getAndAdd(2)
        command match {
          case _: TestAggregateActor.Command.Add | TestAggregateActor.Command.Remove | TestAggregateActor.Command.AcceptEarly =>
            actor ! TestActor.Input.Forward(key, command)

          case _ =>
            actor ! TestActor.Input.Forward(key, TestAggregateActor.Command.Disturb(before))  // Value disturbance is expected as Command.Response
            actor ! TestActor.Input.Forward(key, command)
            actor ! TestActor.Input.Forward(key, TestAggregateActor.Command.Disturb(before + 1))  // This message must be delayed until event has been journaled
        }

        def receive = {
          case msg @ TestAggregateActor.Response.Completed(disturbance) =>
            try {
              command match {
                case _: TestAggregateActor.Command.Add =>
                case TestAggregateActor.Command.Remove =>
                case TestAggregateActor.Command.AcceptEarly =>

                case _: TestAggregateActor.Command.IsAsync =>
                  // Async persist may be disturbed or not.
                  // This test does not ensure arrival of message `Command.Disturb` before message `JournalActor.Output.Stored`
                  if (disturbance != before) {
                    assert(disturbance == before + 1, s" - Disturbance expected: $command -> $msg")
                  }

                case _ =>
                  assert(disturbance == before, s" - persist operation has been disturbed: $command -> $msg")
              }
              promise.success(())
            }
            catch {
              case NonFatal(t) => promise.failure(t)
            }

          case msg @ ("OK" | Done) =>
            promise.success(msg)
            context.stop(self)
        }
      }
    })
    promise.future
  }

  protected final def journalKeyedTestEvents: Vector[KeyedEvent[TestEvent]] =
    journalJsons
      .collect {
        case o if TestKeyedEventJsonCodec canDeserialize o =>
          o.as[Stamped[KeyedEvent[Event]]].map(_.value).orThrow
      }
      .collect { case KeyedEvent(k: String, e: TestEvent) => k <-: e }  // Ignore JournalEvent.SnapshotTaken

  protected final def journalAggregates =
    (journalJsons collect {
      case o if TestData.SnapshotJsonFormat canDeserialize o =>
        o.as[TestAggregate].orThrow
    }).toSet

  protected final def journalJsons: Vector[Json] = journalJsons(JournalFiles.currentFile(journalMeta.fileBase).orThrow)

  protected final def journalJsons(file: Path): Vector[Json] =
    autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
      val builder = new VectorBuilder[Json]
      try reader.iterator foreach (o => builder += normalizeValues(o.value))
      catch {
        case _: EOFException => None
      }
      builder.result()
    }

  final def testCommands(prefix: String) = Vector(
    s"$prefix-A" -> TestAggregateActor.Command.Add("(A.Add)"),
    s"$prefix-B" -> TestAggregateActor.Command.Add("(B.Add)"),
    s"$prefix-C" -> TestAggregateActor.Command.Add("(C.Add)"),
    s"$prefix-A" -> TestAggregateActor.Command.Append("(A.Append)"),
    s"$prefix-A" -> TestAggregateActor.Command.Append(""),
    s"$prefix-A" -> TestAggregateActor.Command.AppendAsync("(A.AppendAsync)"),
    s"$prefix-A" -> TestAggregateActor.Command.AppendNested("(A.AppendNested)"),
    s"$prefix-A" -> TestAggregateActor.Command.AppendNestedAsync("(A.AppendNestedAsync)"),
    s"$prefix-B" -> TestAggregateActor.Command.Remove)

  final def testEvents(prefix: String) =
    Vector(
      s"$prefix-A" <-: TestEvent.Added("(A.Add)"),
      s"$prefix-B" <-: TestEvent.Added("(B.Add)"),
      s"$prefix-C" <-: TestEvent.Added("(C.Add)")) ++
    "(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)".map(ch => s"$prefix-A" <-: TestEvent.Appended(ch)) :+
    (s"$prefix-B" <-: TestEvent.Removed)
}

private[journal] object TestJournalMixin
{
  private val logger = Logger(getClass)
  private val TestConfig = ConfigFactory.parseString("""
     |jobscheduler.akka.actor-message-log-level = Trace
     |jobscheduler.journal.dispatcher {
     |  type = PinnedDispatcher
     |}
     |""".stripMargin)

  private def normalizeValues(json: Json): Json = json.asObject match {
    case Some(jsonObject) =>
      var o = jsonObject
      for (_ <- o("startedAt")) o = o.add("startedAt", "STARTED-AT".asJson)
      for (_ <- o("timestamp")) o = o.add("timestamp", "TIMESTAMP".asJson)
      for (_ <- o("totalRunningTime")) o = o.add("totalRunningTime", 3600.asJson)
      o.asJson
    case None => json
  }
}
