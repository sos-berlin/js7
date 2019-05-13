package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent, WorkflowId}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class WorkflowRegister {

  private val _idToWorkflow = mutable.Map[WorkflowId, Workflow]()
    .withDefault { workflowPath => throw new NoSuchElementException(s"No such $workflowPath") }

  def idToWorkflow: PartialFunction[WorkflowId, Workflow] = _idToWorkflow

  def recover(workflow: Workflow): Unit = {
    _idToWorkflow.insert(workflow.id -> workflow)
  }

  def handleEvent(keyedEvent: KeyedEvent[WorkflowEvent]): Unit = {
    keyedEvent.event match {
      case WorkflowEvent.WorkflowAttached(workflow) =>
        _idToWorkflow += workflow.id -> workflow   // Multiple orders with same Workflow may occur. TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed.
    }
  }

  /** Reuses string from workflow to avoid duplicate strings */
  def reuseMemory[S <: Order.State](order: Order[S]): Order[S] = {
    // A more general place for object identification may be the JSON deserializer: it needs access to an reusable object pool
    val reusedId = _idToWorkflow(order.workflowId).id
    val wp = reusedId /: order.position
    if (order.workflowPosition eq wp)
      order
    else {
      assert(order.workflowPosition == wp)
      order.copy(workflowPosition = wp)
    }
  }

  def get(id: WorkflowId): Option[Workflow] =
    _idToWorkflow.get(id)

  def apply(id: WorkflowId): Workflow =
    _idToWorkflow(id)

  def workflows: Vector[Workflow] =
    _idToWorkflow.values.toVector

  def size: Int =
    _idToWorkflow.size
}
