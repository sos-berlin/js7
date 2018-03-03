package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent, WorkflowPath}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class WorkflowRegister {

  private val _pathToWorkflow = mutable.Map[WorkflowPath, Workflow]()
    .withDefault { workflowPath ⇒ throw new NoSuchElementException(s"Unknown $workflowPath") }

  def pathToWorkflow: PartialFunction[WorkflowPath, Workflow] = _pathToWorkflow

  def recover(workflow: Workflow): Unit = {
    _pathToWorkflow.insert(workflow.path → workflow)
  }

  def handleEvent(keyedEvent: KeyedEvent[WorkflowEvent]): Unit = {
    keyedEvent.event match {
      case WorkflowEvent.WorkflowAttached(workflow) ⇒
        _pathToWorkflow += workflow.path → workflow   // Multiple orders with same Workflow may occur. TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed.
    }
  }

  /** Reuses string from workflow to avoid duplicate strings */
  def reuseMemory[S <: Order.State](order: Order[S]): Order[S] = {
    // A more general place for object identification may be the JSON deserializer: it needs access to an reusable object pool
    val reusedPath = _pathToWorkflow(order.workflowPath).path
    val wp = reusedPath /: order.position
    if (order.workflowPosition eq wp)
      order
    else {
      assert(order.workflowPosition == wp)
      order.copy(workflowPosition = wp)
    }
  }

  def get(path: WorkflowPath): Option[Workflow] =
    _pathToWorkflow.get(path)

  def apply(path: WorkflowPath): Workflow =
    _pathToWorkflow(path)

  def workflows: Vector[Workflow] =
    _pathToWorkflow.values.toVector

  def size: Int =
    _pathToWorkflow.size
}
