package com.sos.jobscheduler.tests.history

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.Agent
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.tests.history.TestHistory._
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private class TestHistory {
  private var repo = Repo.empty
  private val idToOrderEntry = mutable.LinkedHashMap[OrderId, OrderEntry]()
  private val idToOrder = mutable.LinkedHashMap[OrderId, Order[Order.State]]()

  def handleStampedKeyedEvent(stamped: Stamped[KeyedEvent[Event]]): Unit =
    stamped.value match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
        handleStampedKeyedOrderEvent(stamped.copy(value = orderId <-: event))

      case KeyedEvent(_: NoKey, event: RepoEvent) ⇒
        repo = repo.applyEvent(event).orThrow

      case _ ⇒
    }

  def handleStampedKeyedOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): Unit = {
    val Stamped(_, timestamp, KeyedEvent(orderId, event)) = stamped
    lazy val previousOrder = idToOrder(orderId)
    event match {
      case added: OrderAdded ⇒
        idToOrderEntry -= orderId  // In case the OrderId is reused (not recommended)
        idToOrderEntry.insert(orderId → OrderEntry(
          orderId,
          cause = OrderEntry.Cause.UNKNOWN,
          scheduledAt = added.scheduledAt,
          steps = Vector.empty))

      case _: OrderProcessingStarted ⇒
        val order = idToOrder(orderId)
        val jobPath = repo.idTo[Workflow](order.workflowId) map (_.instruction(order.position)) match {
          case Valid(job: Job) ⇒ job.jobPath
          case Invalid(problem) ⇒
            logger.warn(s"$stamped: $problem)")
            JobPath("/?")
        }
        val agentUri = order.attachedToAgent flatMap (a ⇒ repo.idTo[Agent](a)) map (_.uri) onProblem (o ⇒ logger.warn(s"$stamped: $o")) getOrElse ""
        var entry = idToOrderEntry(orderId)
        if (entry.startedAt.isEmpty) {
          entry = entry.copy(
            startWorkflowPosition = Some(previousOrder.workflowPosition),
            startedAt = Some(timestamp))
        }
        idToOrderEntry(orderId) = entry.copy(
          steps = entry.steps :+ OrderStepEntry(orderId, previousOrder.workflowPosition, agentUri, jobPath, previousOrder.variables, timestamp))

      case OrderStdWritten(stdoutOrStderr, chunk) ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).addToLog(stdoutOrStderr, chunk)

      case event: OrderProcessed ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).updateLastStep(
          returnCode = Some(event.outcome) collect { case o: Outcome.Undisrupted ⇒ o.returnCode },
          endVariables = previousOrder.variables)

      case OrderFinished ⇒
        idToOrderEntry(orderId) = idToOrderEntry(orderId).copy(
          endedAt = Some(timestamp),
          endWorkflowPosition = Some(previousOrder.workflowPosition))
        idToOrder -= orderId

      case event: OrderForked ⇒
        for (childOrder ← previousOrder.newForkedOrders(event)) {
          // Forked child OrderId may be reused
          idToOrder += childOrder.id → childOrder
          idToOrderEntry.get(childOrder.id) match {
            case None ⇒
              idToOrderEntry += childOrder.id → OrderEntry(
                childOrder.id,
                startWorkflowPosition = Some(childOrder.workflowPosition),
                cause = OrderEntry.Cause.Forked,
                startedAt = Some(timestamp),
                steps = Vector.empty)

            case Some(entry) ⇒  // Reused child OrderId
              idToOrderEntry(childOrder.id) = entry.copy(
                endedAt = None,
                endWorkflowPosition = None)
          }
        }

      case _: OrderJoined ⇒
        idToOrder --= previousOrder.castState[Order.Join].state.joinOrderIds

      case _ ⇒
    }
    event match {
      case event: OrderAdded ⇒
        idToOrder.insert(orderId → Order.fromOrderAdded(orderId, event))
      case _: OrderFinished ⇒
      case event: OrderCoreEvent ⇒
        idToOrder(orderId) = idToOrder(orderId).update(event)
      case _ ⇒
    }
  }

  def orderEntries: Seq[OrderEntry] = idToOrderEntry.values.toVector
}

private object TestHistory {
  private val logger = Logger(getClass)
}
