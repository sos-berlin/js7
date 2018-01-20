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

  private val pathToNamedWorkflow = mutable.Map[WorkflowPath, Workflow.Named]()
    .withDefault { workflowPath ⇒ throw new NoSuchElementException(s"Unknown $workflowPath") }

  def recover(namedWorkflow: Workflow.Named): Unit = {
    pathToNamedWorkflow.insert(namedWorkflow.path → namedWorkflow)
  }

  def handleEvent(keyedEvent: KeyedEvent[WorkflowEvent]): Unit = {
    val path = keyedEvent.key
    keyedEvent.event match {
      case WorkflowEvent.WorkflowAttached(workflow) ⇒
        pathToNamedWorkflow += path → Workflow.Named(path, workflow)   // Multiple orders with same Workflow may occur. TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed.
    }
  }

  /** Reuses string from workflow to avoid duplicate strings */
  def reuseMemory[S <: Order.State](order: Order[S]): Order[S] = {
    // A more general place for object identification may be the JSON deserializer: it needs access to an reusable object pool
    val reusedPath = pathToNamedWorkflow(order.workflowPath).path
    val wp = reusedPath /: order.position
    if (order.workflowPosition eq wp)
      order
    else {
      assert(order.workflowPosition == wp)
      order.copy(workflowPosition = wp)
    }
  }

  def pathToWorkflow: PartialFunction[WorkflowPath, Workflow] = {
    case workflowPath if pathToNamedWorkflow contains workflowPath ⇒
      pathToNamedWorkflow(workflowPath).workflow
  }

  def get(path: WorkflowPath): Option[Workflow.Named] =
    pathToNamedWorkflow.get(path)

  def apply(path: WorkflowPath): Workflow.Named =
    pathToNamedWorkflow(path)

  def namedWorkflowScripts: Vector[Workflow.Named] =
    pathToNamedWorkflow.values.toVector

  def size: Int =
    pathToNamedWorkflow.size
}
