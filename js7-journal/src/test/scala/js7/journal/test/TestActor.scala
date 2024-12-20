package js7.journal.test

import cats.effect.unsafe.IORuntime
import org.apache.pekko.Done
import org.apache.pekko.actor.{Actor, ActorRef, Props, Stash, Terminated}
import org.apache.pekko.pattern.{ask, pipe}
import org.apache.pekko.util.Timeout
import scala.concurrent.ExecutionContext
import cats.effect.IO
import com.typesafe.config.Config
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.common.pekkoutils.SupervisorStrategies
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.recover.StateRecoverer
import js7.journal.state.FileJournal
import js7.journal.test.TestActor.*
import js7.journal.{EventIdGenerator, JournalActor}
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import scala.language.unsafeNulls

/**
  * @author Joacim Zschimmer
  */
private[journal] final class TestActor(
  config: Config,
  journalLocation: JournalLocation,
  journalStopped: Promise[Unit])
  (using IORuntime)
extends Actor, Stash:

  override val supervisorStrategy = SupervisorStrategies.escalate
  private implicit val askTimeout: Timeout = Timeout(99.seconds)
  private val journalConf = JournalConf.fromConfig(config)
  private val keyToAggregate = mutable.Map[String, ActorRef]()
  private var terminator: ActorRef = null
  private var journalAllocated: Allocated[IO, FileJournal[TestState]] = null
  private def journal = journalAllocated.allocatedThing

  private def journalActor = journal.journalActor

  private given ExecutionContext = summon[IORuntime].compute

  override def preStart() =
    super.preStart()
    StateRecoverer.recover[TestState](journalLocation, config)
    val recovered = StateRecoverer.recover[TestState](journalLocation, config)
    journalAllocated = FileJournal
      .resource(recovered, journalConf,
        EventIdGenerator.withFixedClock(epochMilli = 1000/*EventIds start at 1000000*/))
      .toAllocated
      .await(99.s)

    val state = recovered.state
    for aggregate <- state.keyToAggregate.values do
      val actor = newAggregateActor(aggregate.key)
      actor ! TestAggregateActor.Input.RecoverFromSnapshot(aggregate)
      keyToAggregate += aggregate.key -> actor

  override def postStop() =
    journalAllocated.release.await(99.s)
    journalStopped.success(())
    super.postStop()

  def receive =
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
      sender() ! (keyToAggregate.values map { a => (a ? TestAggregateActor.Input.Get).mapTo[TestAggregate].await(99.s) }).toVector

    case Input.TakeSnapshot =>
      (journalActor ? JournalActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] pipeTo sender()

    case Input.GetJournalState =>
      journalActor.forward(JournalActor.Input.GetJournalActorState)

    case Input.Terminate =>
      terminator = sender()
      keyToAggregate.values foreach context.stop
      if keyToAggregate.isEmpty then
        context.stop(self)
        terminator ! Done

    case Terminated(actorRef) =>
      val key = keyToAggregate collectFirst { case (k, `actorRef`) => k }
      keyToAggregate --= key
      if terminator != null && keyToAggregate.isEmpty then
        context.stop(self)
        terminator ! Done

  private def newAggregateActor(key: String): ActorRef =
    context.watch(context.actorOf(
      Props { new TestAggregateActor(key, journalActor, journalConf) },
      s"Test-$key"))

private[journal] object TestActor:
  object Input:
    case object WaitUntilReady
    case object TakeSnapshot
    final case class Forward(key: String, command: TestAggregateActor.Command)
    case object GetJournalState
    case object GetAll
    case object Terminate
