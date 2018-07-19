package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderForkedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import com.sos.jobscheduler.data.order.{OrderFatEvent, OrderId}
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class InMemoryHistory {
  private val idToOrderEntry = mutable.LinkedHashMap[OrderId, OrderEntry]()

  def handleHistoryEvent(stampedEvent: Stamped[KeyedEvent[OrderFatEvent]]): Unit = {
    val Stamped(_, timestamp, KeyedEvent(orderId, event)) = stampedEvent
    event match {
      case OrderAddedFat(workflowPosition, scheduledAt, variables) ⇒
        idToOrderEntry.get(orderId) match {
          case None ⇒
            idToOrderEntry(orderId) = OrderEntry(orderId, None, OrderEntry.Cause.Added, Some(workflowPosition), scheduledAt)

          case Some(existing) ⇒
            idToOrderEntry(orderId) = existing.copy(
              parent = None,
              startWorkflowPosition = Some(workflowPosition),
              scheduledAt = scheduledAt,
              endedAt = None,
              endWorkflowPosition = None)
        }

      case OrderForkedFat(workflowPosition, children) ⇒
        for (child ← children) {
          idToOrderEntry.get(child.orderId) match {
            case None ⇒
              idToOrderEntry(child.orderId) = OrderEntry(child.orderId, Some(orderId), OrderEntry.Cause.Forked, Some(workflowPosition), None)

            case Some(existing) ⇒
              idToOrderEntry(child.orderId) = existing.copy(
                parent = Some(orderId),
                startWorkflowPosition = Some(workflowPosition),
                scheduledAt = None,
                endedAt = None,
                endWorkflowPosition = None)
          }
        }

      case OrderFinishedFat(workflowPosition) ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).copy(
          endedAt = Some(timestamp),
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
        idToOrderEntry(orderId) = idToOrderEntry(orderId).updateLastStep(timestamp, outcome, variables = variables)

      case OrderStdWrittenFat(t, chunk) ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).addToLog(t, chunk)
    }
  }

  def orderEntries: Seq[OrderEntry] = idToOrderEntry.values.toVector
}
