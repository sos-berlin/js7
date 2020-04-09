package com.sos.jobscheduler.master.web.master.api.fatevent

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.fatevent.MasterFatEvent.MasterReadyFat
import com.sos.jobscheduler.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderCanceledFat, OrderFailedFat, OrderFinishedFat, OrderForkedFat, OrderJoinedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import com.sos.jobscheduler.data.fatevent.{AgentFatEvent, FatEvent, MasterFatEvent, OrderFatEvent}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCanceled, OrderCoreEvent, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}

/**
  * @author Joacim Zschimmer
  */
private[fatevent] final case class FatState(masterId: MasterId, eventId: EventId, repo: Repo, idToOrder: Map[OrderId, Order[Order.State]])
{
  def toFatEvents(stamped: Stamped[KeyedEvent[Event]]): (FatState, Option[Stamped[KeyedEvent[FatEvent]]]) = {
    if (stamped.eventId <= eventId) throw new IllegalArgumentException(s"Duplicate stamped.eventId ${EventId.toString(stamped.eventId)} <= eventId ${EventId.toString(eventId)}")
    stamped.value match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        handleOrderEvent(stamped.copy(value = orderId <-: event))

      case KeyedEvent(_: NoKey, event: RepoEvent) =>
        val updatedConverter = copy(
          eventId = stamped.eventId,
          repo = repo.applyEvent(event).orThrow)
        (updatedConverter, None)

      case KeyedEvent(agentRefPath: AgentRefPath, event: MasterAgentEvent) =>
        (this, toMasterAgentFatEvent(event) map (e => stamped.copy(value = agentRefPath <-: e)))

      case KeyedEvent(_: NoKey, event: MasterEvent) =>
        (this, toMasterFatEvent(event) map (e => stamped.copy(value = NoKey <-: e)))

      case _ =>
        (this, None)
    }
  }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): (FatState, Option[Stamped[KeyedEvent[OrderFatEvent]]]) = {
    val Stamped(eventId, timestamp, KeyedEvent(orderId, event)) = stamped
    val order = event match {
      case event: OrderAdded => Order.fromOrderAdded(orderId, event)
      case event: OrderCoreEvent => idToOrder(orderId).update(event).orThrow  // 🔥 ProblemException
      case _ => idToOrder(orderId)
    }
    val updatedFatState = event match {
      case _: OrderAdded      => copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id -> order))
      case _: OrderFinished   => copy(eventId = stamped.eventId, idToOrder = idToOrder - order.id)
      case event: OrderForked => copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id -> order) ++ (order.newForkedOrders(event) :+ order).map(o => o.id -> o))
      case _: OrderJoined     => copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id -> order) -- idToOrder(order.id).castState[Order.Forked].state.childOrderIds)
      case _: OrderCoreEvent  => copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id -> order))
      case _                  => copy(eventId = stamped.eventId)
    }
    val fatEvents = toOrderFatEvent(order, event) map (e => Stamped(eventId, timestamp, order.id <-: e))
    (updatedFatState, fatEvents)
  }

  private def toOrderFatEvent(order: Order[Order.State], event: OrderEvent): Option[OrderFatEvent] =
    event match {
      case added: OrderAdded =>
        Some(OrderAddedFat(added.workflowId /: Position(0), added.scheduledFor, order.keyValues))

      case _: OrderProcessingStarted =>
        val jobName = repo.idTo[Workflow](order.workflowId).flatMap(_.checkedExecute(order.position)).orThrow match {
          case named: Execute.Named => Some(named.name)
          case _ => None
        }
        val agentRef = order.attached.flatMap(a => repo.pathTo[AgentRef](a)).orThrow
        Some(OrderProcessingStartedFat(order.workflowPosition, agentRef.path, agentRef.uri, jobName, order.keyValues))

      case OrderStdWritten(stdoutOrStderr, chunk) =>
        Some(OrderStdWrittenFat(order.id, stdoutOrStderr)(chunk))

      case event: OrderProcessed =>
        Some(OrderProcessedFat(event.outcome, order.keyValues))

      case OrderFinished =>
        Some(OrderFinishedFat(order.workflowPosition))

      case OrderFailed(outcome) =>
        Some(OrderFailedFat(order.workflowPosition, outcome))

      case OrderCanceled =>
        Some(OrderCanceledFat(order.workflowPosition))

      case OrderForked(children) =>
        Some(OrderForkedFat(
          order.workflowId /: order.position,
          for (ch <- children) yield
            OrderForkedFat.Child(ch.branchId, ch.orderId, order.arguments)))

      case OrderJoined(outcome) =>
        Some(OrderJoinedFat(
          childOrderIds = idToOrder(order.id).ifState[Order.Forked] map (_.state.childOrderIds) getOrElse Nil/*failure*/,
          outcome))

      case _ =>
        None
    }

  private def toMasterFatEvent(event: MasterEvent): Option[MasterFatEvent] =
    event match {
      case masterReady: MasterEvent.MasterReady =>
        Some(MasterReadyFat(masterId, masterReady.timezone))

      case _ =>
        None
    }

  private def toMasterAgentFatEvent(event: MasterAgentEvent): Option[AgentFatEvent] =
    event match {
      case MasterAgentEvent.AgentReady(zoneId) =>
        Some(AgentFatEvent.AgentReadyFat(zoneId))

      case _ =>
        None
    }
}
