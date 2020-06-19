package js7.controller.web.controller.api.fatevent

import js7.base.problem.Checked.Ops
import js7.controller.data.events.{ControllerAgentEvent, ControllerEvent}
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.data.fatevent.ControllerFatEvent.ControllerReadyFat
import js7.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderCancelledFat, OrderFailedFat, OrderFinishedFat, OrderForkedFat, OrderJoinedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import js7.data.fatevent.{AgentFatEvent, ControllerFatEvent, FatEvent, OrderFatEvent}
import js7.data.filebased.{Repo, RepoEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderCoreEvent, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Execute
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
private[fatevent] final case class FatState(controllerId: ControllerId, eventId: EventId, repo: Repo, idToOrder: Map[OrderId, Order[Order.State]])
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

      case KeyedEvent(agentRefPath: AgentRefPath, event: ControllerAgentEvent) =>
        (this, toControllerAgentFatEvent(event) map (e => stamped.copy(value = agentRefPath <-: e)))

      case KeyedEvent(_: NoKey, event: ControllerEvent) =>
        (this, toControllerFatEvent(event) map (e => stamped.copy(value = NoKey <-: e)))

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

      case OrderCancelled =>
        Some(OrderCancelledFat(order.workflowPosition))

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

  private def toControllerFatEvent(event: ControllerEvent): Option[ControllerFatEvent] =
    event match {
      case controllerReady: ControllerEvent.ControllerReady =>
        Some(ControllerReadyFat(controllerId, controllerReady.timezone))

      case _ =>
        None
    }

  private def toControllerAgentFatEvent(event: ControllerAgentEvent): Option[AgentFatEvent] =
    event match {
      case ControllerAgentEvent.AgentReady(zoneId) =>
        Some(AgentFatEvent.AgentReadyFat(zoneId))

      case _ =>
        None
    }
}
