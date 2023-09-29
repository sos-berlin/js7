package js7.agent.scheduler.order

import js7.base.crypt.Signed
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.BasicItemEvent
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, SignedItemAttachedToMe}
import js7.data.order.Order
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class WorkflowRegister(agentPath: AgentPath) {

  // COMPATIBLE with v2.1
  private val _idToWorkflow = mutable.Map.empty[WorkflowId, Workflow]
    .withDefault { workflowPath => throw new NoSuchElementException(s"No such $workflowPath") }

  def idToWorkflow: PartialFunction[WorkflowId, Workflow] = _idToWorkflow

  def recover(workflow: Workflow): Unit = {
    _idToWorkflow.insert(workflow.id, workflow)
  }

  def recover(signed: Signed[Workflow]): Unit = {
    val workflow = signed.value
    _idToWorkflow.insert(workflow.id, workflow.reduceForAgent(agentPath))
  }

  def handleEvent(keyedEvent: KeyedEvent[BasicItemEvent], reducedWorkflow: Workflow): Unit = {
    keyedEvent.event match {
      case SignedItemAttachedToMe(Signed(_: Workflow, _)) =>
        _idToWorkflow.update(reducedWorkflow.id, reducedWorkflow.reduceForAgent(agentPath))

      // COMPATIBLE with v2.1
      case ItemAttachedToMe(workflow: Workflow) =>
        _idToWorkflow.update(workflow.id, reducedWorkflow)

      //case ItemDetached(workflowId: WorkflowId, _) =>
      //  _idToWorkflow -= workflowId

      case _ => sys.error(s"WorkflowRegister: Unexpected event: $keyedEvent")
    }
  }

  /** Reuses string from workflow to avoid duplicate strings */
  def reuseMemory[S <: Order.State](order: Order[S]): Order[S] = {
    // A more general place for object identification may be the JSON deserializer: it needs access to an reusable object pool
    val reusedId = _idToWorkflow(order.workflowId).id
    val wp = reusedId /: order.position
    if order.workflowPosition eq wp then
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
