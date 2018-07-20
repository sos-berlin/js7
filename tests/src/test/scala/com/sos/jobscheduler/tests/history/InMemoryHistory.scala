package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.data.event.{<-:, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderForkedFat, OrderJoinedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import com.sos.jobscheduler.data.fatevent.{FatEvent, OrderFatEvent}
import com.sos.jobscheduler.data.order.OrderId
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class InMemoryHistory {
  private val idToOrderEntry = mutable.LinkedHashMap[OrderId, OrderEntry]()

  def handleFatEvent(stampedEvent: Stamped[KeyedEvent[FatEvent]]): Unit = {
    stampedEvent.value match {
      case (orderId: OrderId) <-: (event: OrderFatEvent) ⇒ handleOrderFatEvent(stampedEvent.copy(value = orderId <-: event))
      case _ ⇒
    }
  }

  private def handleOrderFatEvent(stampedEvent: Stamped[KeyedEvent[OrderFatEvent]]): Unit = {
    val Stamped(_, timestamp, KeyedEvent(orderId, event)) = stampedEvent
    event match {
      case OrderAddedFat(workflowPosition, scheduledAt, variables) ⇒
        idToOrderEntry.get(orderId) match {
          case None ⇒
            idToOrderEntry(orderId) = OrderEntry(orderId, None, variables, OrderEntry.Cause.Added, Some(workflowPosition), scheduledAt = scheduledAt)

          case Some(existing) ⇒
            idToOrderEntry(orderId) = existing.copy(
              parent = None,
              startWorkflowPosition = Some(workflowPosition),
              scheduledAt = scheduledAt,
              finishedAt = None,
              endWorkflowPosition = None)
        }

      case OrderForkedFat(workflowPosition, children) ⇒
        for (child ← children) {
          idToOrderEntry.get(child.orderId) match {
            case None ⇒
              idToOrderEntry(child.orderId) = OrderEntry(child.orderId, Some(orderId), child.variables, OrderEntry.Cause.Forked, Some(workflowPosition), None)

            case Some(existing) ⇒
              idToOrderEntry(child.orderId) = existing.copy(
                parent = Some(orderId),
                startWorkflowPosition = Some(workflowPosition),
                scheduledAt = None,
                finishedAt = None,
                endWorkflowPosition = None)
          }
        }

      case OrderJoinedFat(childOrderIds, variables, outcome) ⇒
        for (id ← childOrderIds) {
          idToOrderEntry(id) = idToOrderEntry(id).copy(finishedAt = Some(timestamp))
        }

      case OrderFinishedFat(workflowPosition) ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).copy(
          finishedAt = Some(timestamp),
          endWorkflowPosition = Some(workflowPosition))

      case OrderProcessingStartedFat(workflowPosition, agentUri, jobPath, variables) ⇒
        var entry = idToOrderEntry(orderId)
        if (entry.startedAt.isEmpty) {
          entry = entry.copy(
            startWorkflowPosition = Some(workflowPosition),
            startedAt = Some(timestamp))
        }
        idToOrderEntry(orderId) = entry.copy(
          steps = entry.steps :+ OrderStepEntry(orderId, workflowPosition, agentUri, jobPath, variables, timestamp))

      case OrderProcessedFat(outcome, variables) ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).updateLastStep(timestamp, outcome, variables)

      case OrderStdWrittenFat(t, chunk) ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).addToLog(t, chunk)
    }
  }

  def orderEntries: Seq[OrderEntry] = idToOrderEntry.values.toVector
}
