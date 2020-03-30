package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.agent.AgentState
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.agent.scheduler.order.OrderJournalRecoverer._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.base.utils.Collections.implicits.{InsertableMutableMap, RichTraversable}
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.workflow.Recovering.followUpRecoveredSnapshots
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{JournalEvent, JournalId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent}
import com.typesafe.config.Config
import scala.collection.mutable
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
private final class OrderJournalRecoverer(
  protected val journalMeta: JournalMeta,
  journalId: JournalId,
  agentConfiguration: AgentConfiguration)
extends JournalRecoverer {

  protected val expectedJournalId = Some(journalId)

  private val workflowRegister = new WorkflowRegister
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()

  protected def recoverSnapshot = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case workflow: Workflow =>
      workflowRegister.recover(workflow)
  }

  override protected def onAllSnapshotRecovered() = {
    val (added, removed) = followUpRecoveredSnapshots(workflowRegister.idToWorkflow.checked, idToOrder.toMap)
    idToOrder ++= added.map(o => o.id -> o)
    idToOrder --= removed
  }

  protected def recoverEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, event: WorkflowEvent.WorkflowAttached)) =>
      workflowRegister.handleEvent(NoKey <-: event)

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      handleEvent(orderId, event)

    case Stamped(_, _, KeyedEvent(_, _: AgentMasterEvent.AgentReadyForMaster)) =>

    case Stamped(_, _, KeyedEvent(_, _: JournalEvent)) =>
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

  private def agentState = AgentState(lastRecoveredEventId, idToOrder.toMap, workflowRegister.workflows toKeyedMap (_.id))

  private def result(config: Config) = new Recovered(this, config)
}

private[agent] object OrderJournalRecoverer
{
  def recover(journalMeta: JournalMeta, journalId: JournalId, agentConfiguration: AgentConfiguration): Recovered = {
    val recoverer = new OrderJournalRecoverer(journalMeta, journalId, agentConfiguration)
    recoverer.recoverAll()
    recoverer.result(agentConfiguration.config)
  }

  final class Recovered private[OrderJournalRecoverer](recoverer: OrderJournalRecoverer, config: Config)
  extends HasCloser
  {
    lazy val eventWatch = new JournalEventWatch(recoverer.journalMeta, config)
      .closeWithCloser

    def journalMeta: JournalMeta =
      recoverer.journalMeta

    def agentState: AgentState =
      recoverer.agentState

    def startJournalAndFinishRecovery(journalActor: ActorRef @@ JournalActor.type, actors: RecoveredJournalingActors)(implicit arf: ActorRefFactory) =
      recoverer.startJournalAndFinishRecovery(journalActor = journalActor, actors, Some(eventWatch))
  }
}
