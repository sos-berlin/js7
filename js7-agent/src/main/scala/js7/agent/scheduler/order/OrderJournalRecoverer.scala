package js7.agent.scheduler.order

import akka.actor.{ActorRef, ActorRefFactory}
import js7.agent.AgentState
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.event.AgentMasterEvent
import js7.agent.scheduler.order.OrderJournalRecoverer._
import js7.base.problem.Checked.Ops
import js7.base.utils.Closer.syntax._
import js7.base.utils.Collections.implicits.{InsertableMutableMap, RichTraversable}
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.RichPartialFunction
import js7.core.event.journal.JournalActor
import js7.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import js7.core.event.journal.recover.JournalRecoverer
import js7.core.event.journal.watch.JournalEventWatch
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{JournalEvent, JournalId, JournalState, JournaledState, KeyedEvent, Stamped}
import js7.data.execution.workflow.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowEvent}
import com.typesafe.config.Config
import scala.collection.mutable
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
private final class OrderJournalRecoverer(
  protected val journalMeta: JournalMeta,
  journalId: JournalId)
extends JournalRecoverer[AgentState]
{
  protected val expectedJournalId = Some(journalId)

  private var journalState = JournalState.empty
  private val workflowRegister = new WorkflowRegister
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()

  protected def recoverSnapshot = {
    case o: JournalState =>
      journalState = o

    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case workflow: Workflow =>
      workflowRegister.recover(workflow)
  }

  override protected def onAllSnapshotRecovered() = {
    val (added, removed) = followUpRecoveredWorkflowsAndOrders(workflowRegister.idToWorkflow.checked, idToOrder.toMap)
    idToOrder ++= added.map(o => o.id -> o)
    idToOrder --= removed
  }

  protected def recoverEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, event: WorkflowEvent.WorkflowAttached)) =>
      workflowRegister.handleEvent(NoKey <-: event)

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      handleEvent(orderId, event)

    case Stamped(_, _, KeyedEvent(_, _: AgentMasterEvent.AgentReadyForMaster)) =>

    case Stamped(_, _, KeyedEvent(_, event: JournalEvent)) =>
      journalState = journalState.applyEvent(event)
  }

  private def handleEvent(orderId: OrderId, event: OrderEvent) =
    event match {
      case event: OrderEvent.OrderAttached =>
        idToOrder.insert(orderId -> Order.fromOrderAttached(orderId, event))

      case OrderEvent.OrderDetached =>
        idToOrder -= orderId

      case event: OrderEvent =>
        // See also OrderActor#update
        event match {
          case event: OrderCoreEvent =>
            handleForkJoinEvent(orderId, event)
            idToOrder(orderId) = idToOrder(orderId).update(event).orThrow
          case _: OrderStdWritten =>
            // OrderStdWritten is not handled (but forwarded to Master)
        }
    }

  private def handleForkJoinEvent(orderId: OrderId, event: OrderCoreEvent): Unit =  // TODO Duplicate with JournaledStateRecoverer
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

  private def agentState = AgentState(
    lastRecoveredEventId,
    JournaledState.Standards.empty.copy(journalState = journalState),
    idToOrder.toMap,
    workflowRegister.workflows toKeyedMap (_.id))

  private def result(config: Config) = new Recovered(this, config)
}

private[agent] object OrderJournalRecoverer
{
  def recover(journalMeta: JournalMeta, journalId: JournalId, agentConfiguration: AgentConfiguration): Recovered = {
    val recoverer = new OrderJournalRecoverer(journalMeta, journalId)
    recoverer.recoverAll()
    recoverer.result(agentConfiguration.config)
  }

  final class Recovered private[OrderJournalRecoverer](recoverer: OrderJournalRecoverer, config: Config)
  extends HasCloser
  {
    lazy val eventWatch = new JournalEventWatch(recoverer.journalMeta, config)
      .closeWithCloser

    def eventId = recoverer.lastRecoveredEventId

    def journalMeta: JournalMeta =
      recoverer.journalMeta

    def agentState: AgentState =
      recoverer.agentState

    def startJournalAndFinishRecovery(
      journalActor: ActorRef @@ JournalActor.type,
      journaledState: AgentState,
      actors: RecoveredJournalingActors)
      (implicit arf: ActorRefFactory)
    =
      recoverer.startJournalAndFinishRecovery(journalActor = journalActor, journaledState, actors, Some(eventWatch))
  }
}
