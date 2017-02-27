package com.sos.jobscheduler.shared.event.journal.tests

import akka.Done
import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props, Stash, SupervisorStrategy, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.jobscheduler.data.event.{KeyedEvent, Snapshot}
import com.sos.jobscheduler.shared.event.SnapshotKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.tests.TestActor._
import com.sos.jobscheduler.shared.event.journal.tests.TestJsonFormats.TestKeyedEventJsonFormat
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, Journal, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer}
import java.nio.file.Path
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
private[tests] final class TestActor(journalFile: Path) extends Actor with Stash {

  private implicit val executionContext = context.dispatcher

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0) {
    case _ ⇒ SupervisorStrategy.Stop
  }
  private implicit val askTimeout = Timeout(999.seconds)
  private val journalActor = context.actorOf(
    Props { new JsonJournalActor(TestJsonJournalMeta, journalFile, syncOnCommit = true, new EventIdGenerator, new SnapshotKeyedEventBus) },
    "Journal")
  private val keyToAggregate = mutable.Map[String, ActorRef]()

  override def preStart() = {
    super.preStart()
    recover()
  }

  private def recover(): Unit = {
    val recovered =
      autoClosing(new JsonJournalRecoverer(TestJsonJournalMeta, journalFile)) { journal ⇒
        import JsonJournalRecoverer._
        for (recovered ← journal) (recovered: @unchecked) match {
          case RecoveringSnapshot(snapshot: TestAggregate) ⇒
            journal.addActorForSnapshot(snapshot, newAggregateActor(snapshot.key))

          case RecoveringForUnknownKey(eventSnapshot @ Snapshot(_, KeyedEvent(key: String, _: TestEvent.Added))) ⇒
            journal.addActorForFirstEvent(eventSnapshot, newAggregateActor(key))

          case _: RecoveringForKnownKey ⇒
        }
        journal.recoveredJournalingActors
      }
    keyToAggregate ++= recovered.keyToJournalingActor map { case (k: String, a) ⇒ k → a }
    journalActor ! Journal.Input.Start(recovered)
  }

  def receive = {
    case Journal.Output.Ready ⇒
      context.become(ready)
      unstashAll()
      logger.info("Ready")

    case _ ⇒
      stash()
  }

  private def ready: Receive = {
    case Input.WaitUntilReady ⇒
      sender() ! Done

    case Input.Forward(key: String, command: TestAggregateActor.Command.Add) ⇒
      assert(!keyToAggregate.contains(key))
      val actor = context.actorOf(Props { new TestAggregateActor(key, journalActor) })
      context.watch(actor)
      keyToAggregate += key → actor
      (actor ? command).mapTo[Done] pipeTo sender()

    case Input.Forward(key: String, command: TestAggregateActor.Command) ⇒
      (keyToAggregate(key) ? command).mapTo[Done] pipeTo sender()

    case Input.GetAll ⇒
      sender() ! (keyToAggregate.values map { a ⇒ (a ? TestAggregateActor.Input.Get).mapTo[TestAggregate] await 99.s }).toVector

    case Journal.Input.TakeSnapshot ⇒
      (journalActor ? Journal.Input.TakeSnapshot).mapTo[Journal.Output.SnapshotTaken.type] pipeTo sender()

    case Terminated(actorRef) ⇒  // ???
      keyToAggregate --= keyToAggregate collectFirst { case (key, `actorRef`) ⇒ key }
  }

  private def newAggregateActor(key: String): ActorRef =
    context.actorOf(
      Props { new TestAggregateActor(key, journalActor) },
      s"Test-$key")
}

private[tests] object TestActor {
  intelliJuseImports(TestKeyedEventJsonFormat)

  val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[TestAggregate])
  private val TestJsonJournalMeta = new JsonJournalMeta(
      snapshotJsonFormat = SnapshotJsonFormat,
      eventJsonFormat = TestKeyedEventJsonFormat,
      snapshotToKey = {
        case a: TestAggregate ⇒ a.key
      },
      isDeletedEvent = Set(TestEvent.Removed))
    with GzipCompression
  private val logger = Logger(getClass)

  object Input {
    final case object WaitUntilReady
    final case class Forward(key: String, command: TestAggregateActor.Command)
    final case object GetAll
  }
}
