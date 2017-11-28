package com.sos.jobscheduler.shared.event.journal.tests

import akka.Done
import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.tests.TestActor._
import com.sos.jobscheduler.shared.event.journal.tests.TestJsonCodecs.TestKeyedEventJsonCodec
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalActorRecoverer, JsonJournalMeta, JsonJournalRecoverer}
import java.nio.file.Path
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
private[tests] final class TestActor(journalFile: Path) extends Actor with Stash {

  private implicit val executionContext = context.dispatcher

  override val supervisorStrategy = SupervisorStrategies.escalate
  private implicit val askTimeout = Timeout(999.seconds)
  private val journalActor = context.actorOf(
    Props { new JsonJournalActor(TestJsonJournalMeta, journalFile, syncOnCommit = true, new EventIdGenerator, new StampedKeyedEventBus) },
    "Journal")
  private val keyToAggregate = mutable.Map[String, ActorRef]()

  override def preStart() = {
    super.preStart()
    val recoverer = new MyJournalRecoverer()
    recoverer.recoverAllAndTransferTo(journalActor = journalActor)
    keyToAggregate ++= recoverer.recoveredJournalingActors.keyToJournalingActor map { case (k: String @unchecked, a) ⇒ k → a }
  }

  private class MyJournalRecoverer extends JsonJournalActorRecoverer[TestEvent] {
    protected val sender = TestActor.this.sender()
    protected val jsonJournalMeta = TestJsonJournalMeta
    protected val journalFile = TestActor.this.journalFile

    protected def snapshotToKey = {
      case a: TestAggregate ⇒ a.key
    }

    protected def isDeletedEvent = Set(TestEvent.Removed)

    def recoverSnapshot = {
      case snapshot: TestAggregate ⇒
        recoverActorForSnapshot(snapshot, newAggregateActor(snapshot.key))
    }

    def recoverNewKey = {
      case stamped @ Stamped(_, KeyedEvent(key: String, _: TestEvent.Added)) ⇒
        recoverActorForNewKey(stamped, newAggregateActor(key))

      case _ ⇒
    }
  }

  def receive = {
    case JsonJournalRecoverer.Output.JournalIsReady ⇒
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

    case Input.Forward(key: String, command: TestAggregateActor.Command.Disturb.type) ⇒
      (keyToAggregate(key) ? command).mapTo[String] pipeTo sender()

    case Input.Forward(key: String, command: TestAggregateActor.Command) ⇒
      (keyToAggregate(key) ? command).mapTo[Done] pipeTo sender()

    case Input.GetAll ⇒
      sender() ! (keyToAggregate.values map { a ⇒ (a ? TestAggregateActor.Input.Get).mapTo[TestAggregate] await 99.s }).toVector

    case Input.TakeSnapshot ⇒
      (journalActor ? JsonJournalActor.Input.TakeSnapshot).mapTo[JsonJournalActor.Output.SnapshotTaken.type] pipeTo sender()

    case Input.GetJournalState ⇒
      journalActor.forward(JsonJournalActor.Input.GetState)

    case Terminated(actorRef) ⇒  // ???
      keyToAggregate --= keyToAggregate collectFirst { case (key, `actorRef`) ⇒ key }
  }

  private def newAggregateActor(key: String): ActorRef =
    context.actorOf(
      Props { new TestAggregateActor(key, journalActor) },
      s"Test-$key")
}

private[tests] object TestActor {
  intelliJuseImport(TestKeyedEventJsonCodec)

  val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[TestAggregate])
  private val TestJsonJournalMeta = new JsonJournalMeta[TestEvent](SnapshotJsonFormat, TestKeyedEventJsonCodec) with GzipCompression
  private val logger = Logger(getClass)

  object Input {
    final case object WaitUntilReady
    final case object TakeSnapshot
    final case class Forward(key: String, command: TestAggregateActor.Command)
    final case object GetJournalState
    final case object GetAll
  }
}
