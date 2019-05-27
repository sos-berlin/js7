package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.SetOnce
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.core.workflow.Recovering.followUpRecoveredSnapshots
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCanceled, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.MasterJournalRecoverer._
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterJournalKeyedEventJsonCodec
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterSnapshots.SnapshotJsonCodec
import com.sos.jobscheduler.master.data.agent.{AgentEventIdEvent, AgentSnapshot}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster
import com.sos.jobscheduler.master.data.events.MasterEvent.{MasterStarted, MasterTestEvent}
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.typesafe.config.Config
import scala.collection.mutable
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
private final class MasterJournalRecoverer(masterConfiguration: MasterConfiguration)
extends JournalRecoverer[Event]
{
  private val resumedAt = Timestamp.now
  protected val journalMeta = JournalMeta(SnapshotJsonCodec, MasterJournalKeyedEventJsonCodec, masterConfiguration.journalFileBase)

  protected def expectedJournalId = None

  private val masterStarted = new SetOnce[MasterStarted]
  private var repo = Repo(MasterFileBaseds.jsonCodec)
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
  private val pathToAgent = mutable.Map[AgentRefPath, AgentSnapshot]()

  protected def recoverSnapshot = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case event: RepoEvent =>
      repo = repo.applyEvent(event).orThrow

    case snapshot: AgentSnapshot =>
      pathToAgent.insert(snapshot.agentRefPath -> snapshot)

    case o: MasterStarted =>
      masterStarted := o
  }

  override protected def onAllSnapshotRecovered() = {
    val (added, removed) = followUpRecoveredSnapshots(repo.idTo[Workflow], idToOrder.toMap)
    idToOrder ++= added.map(o => o.id -> o)
    idToOrder --= removed
  }

  protected def recoverEvent = {
    case Stamped(_, _, keyedEvent) =>
      keyedEvent match {
        case KeyedEvent(_: NoKey, _: MasterEvent.MasterReady) =>

        case KeyedEvent(_: NoKey, event: RepoEvent) =>
          repo = repo.applyEvent(event).orThrow

        case KeyedEvent(agentRefPath: AgentRefPath, event: MasterAgentEvent) =>
          event match {
            case AgentRegisteredMaster(agentRunId) =>
              pathToAgent.insert(agentRefPath -> AgentSnapshot(agentRefPath, Some(agentRunId), eventId = EventId.BeforeFirst))
            case _ =>
          }

        case KeyedEvent(a: AgentRefPath, AgentEventIdEvent(agentEventId)) =>
          // Preceding AgentSnapshot is required (see recoverSnapshot)
          pathToAgent(a) = pathToAgent(a).copy(eventId = agentEventId)

        case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
          event match {
            case event: OrderAdded =>
              idToOrder.insert(orderId -> Order.fromOrderAdded(orderId, event))

            case OrderFinished | OrderCanceled =>
              idToOrder -= orderId

            case event: OrderCoreEvent =>
              handleForkJoinEvent(orderId, event)
              idToOrder(orderId) = idToOrder(orderId).update(event).orThrow  // 🔥 ProblemException

            case _: OrderStdWritten =>
          }

        case KeyedEvent(_: NoKey, o: MasterStarted) =>
          // The very first journal file "master--0.journal" contains a (premature) MasterStarted snapshot and a MasterStarted event
          require(o == masterStarted(), s"Error in journal: recovered $o event differs from snapshot ${masterStarted()}")

        case KeyedEvent(_, MasterTestEvent) =>

        case _ => sys.error(s"Unknown event recovered from journal: $keyedEvent")
      }
  }

  private def handleForkJoinEvent(orderId: OrderId, event: OrderCoreEvent): Unit =  // TODO Duplicate with Agent's OrderJournalRecoverer
    event match {
      case event: OrderForked =>
        for (childOrder <- idToOrder(orderId).newForkedOrders(event)) {
          idToOrder.insert(childOrder.id -> childOrder)
        }

      case event: OrderJoined =>
        idToOrder(orderId).state match {
          case forked: Order.Forked =>
            idToOrder --= forked.childOrderIds

          case state =>
            sys.error(s"Event $event recovered, but $orderId is in state $state")
        }

      case _ =>
    }

  private def masterState: Option[MasterState] = {
    hasJournal ? MasterState(
      eventId = lastRecoveredEventId,
      masterStarted(),
      repo,
      pathToAgent.values.toImmutableSeq, idToOrder.values.toImmutableSeq)
  }

  private def result(config: Config) = new Recovered(this, config)
}

private[master] object MasterJournalRecoverer
{
  def recover(masterConfiguration: MasterConfiguration): Recovered = {
    val recoverer = new MasterJournalRecoverer(masterConfiguration)
    recoverer.recoverAll()
    recoverer.result(masterConfiguration.config)
  }

  final class Recovered private[MasterJournalRecoverer](recoverer: MasterJournalRecoverer, config: Config)
  {
    val eventWatch = new JournalEventWatch(recoverer.journalMeta, Some(recoverer.journalHeader.journalId), config)
    def journalMeta = recoverer.journalMeta
    def masterState = recoverer.masterState
    def totalRunningTime = recoverer.journalHeader.totalRunningTime

    def startJournalAndFinishRecovery(journalActor: ActorRef @@ JournalActor.type)(implicit arf: ActorRefFactory) =
      recoverer.startJournalAndFinishRecovery(journalActor = journalActor, journalingObserver = Some(eventWatch))
  }
}
