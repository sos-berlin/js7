package js7.journal.test

import akka.Done
import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.softwaremill.diffx.generic.auto._
import com.typesafe.config.Config
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.common.akkautils.SupervisorStrategies
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.recover.StateRecoverer
import js7.journal.state.FileStatePersistence
import js7.journal.test.TestActor._
import js7.journal.{EventIdClock, EventIdGenerator, JournalActor}
import monix.execution.Scheduler.Implicits.global
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
private[journal] final class TestActor(config: Config, journalMeta: JournalMeta, journalStopped: Promise[Unit])
extends Actor with Stash
{
  override val supervisorStrategy = SupervisorStrategies.escalate
  private implicit val askTimeout = Timeout(99.seconds)
  private val journalConf = JournalConf.fromConfig(config)
  private val keyToAggregate = mutable.Map[String, ActorRef]()
  private var terminator: ActorRef = null
  private var persistence: FileStatePersistence[TestState] = null

  private def journalActor = persistence.journalActor

  override def preStart() = {
    super.preStart()
    StateRecoverer.recover[TestState](journalMeta, config)
    val recovered = StateRecoverer.recover[TestState](journalMeta, config)
    persistence = FileStatePersistence
      .start(recovered, journalConf,
        new EventIdGenerator(EventIdClock.fixed(epochMilli = 1000/*EventIds start at 1000000*/)))
      .runToFuture
      .await(99.s)

    val state = recovered.state
    for (aggregate <- state.keyToAggregate.values) {
      val actor = newAggregateActor(aggregate.key)
      actor ! TestAggregateActor.Input.RecoverFromSnapshot(aggregate)
      keyToAggregate += aggregate.key -> actor
    }
  }

  override def postStop() = {
    persistence.stop await 99.s
    journalStopped.success(())
    super.postStop()
  }

  def receive = {
    case Input.WaitUntilReady =>
      sender() ! Done

    case Input.Forward(key: String, command: TestAggregateActor.Command.Add) =>
      assert(!keyToAggregate.contains(key))
      val actor = newAggregateActor(key)
      keyToAggregate += key -> actor
      (actor ? command).mapTo[Done] pipeTo sender()

    case Input.Forward(key: String, TestAggregateActor.Command.Remove) =>
      val aggregateActor = keyToAggregate(key)
      val respondTo = this.sender()
      (aggregateActor ? TestAggregateActor.Command.Remove).mapTo[TestAggregateActor.Response.Completed] foreach { response =>
        // Respond first when actor TestAggregateActor has been terminated
        context.actorOf(Props {
          new Actor {
            context.watch(aggregateActor)
            def receive = {
              case Terminated(`aggregateActor`) =>
                respondTo ! response
                context.stop(self)
            }
          }
        })
      }
      keyToAggregate -= key

    case Input.Forward(key: String, disturb: TestAggregateActor.Command.Disturb) =>
      keyToAggregate(key) ! disturb

    case Input.Forward(key: String, command: TestAggregateActor.Command.DisturbAndRespond.type) =>
      (keyToAggregate(key) ? command).mapTo[String] pipeTo sender()

    case Input.Forward(key: String, command: TestAggregateActor.Command) =>
      (keyToAggregate(key) ? command).mapTo[TestAggregateActor.Response.Completed] pipeTo sender()

    case Input.GetAll =>
      sender() ! (keyToAggregate.values map { a => (a ? TestAggregateActor.Input.Get).mapTo[TestAggregate] await 99.s }).toVector

    case Input.TakeSnapshot =>
      (journalActor ? JournalActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] pipeTo sender()

    case Input.GetJournalState =>
      journalActor.forward(JournalActor.Input.GetJournalActorState)

    case Input.Terminate =>
      terminator = sender()
      keyToAggregate.values foreach context.stop
      if (keyToAggregate.isEmpty) {
        context.stop(self)
        terminator ! Done
      }

    case Terminated(actorRef) =>
      val key = keyToAggregate collectFirst { case (k, `actorRef`) => k }
      keyToAggregate --= key
      if (terminator != null && keyToAggregate.isEmpty) {
        context.stop(self)
        terminator ! Done
      }
  }

  private def newAggregateActor(key: String): ActorRef =
    context.watch(context.actorOf(
      Props { new TestAggregateActor(key, journalActor, journalConf) },
      s"Test-$key"))
}

private[journal] object TestActor
{
  object Input {
    final case object WaitUntilReady
    final case object TakeSnapshot
    final case class Forward(key: String, command: TestAggregateActor.Command)
    final case object GetJournalState
    final case object GetAll
    final case object Terminate
  }
}
