package js7.agent.scheduler.order

import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.workflow.{Workflow, WorkflowEvent, WorkflowId}
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
        // Multiple orders with same Workflow may occur
        // TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed
        _idToWorkflow += workflow.id -> workflow
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
      assertThat(order.workflowPosition == wp)
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
