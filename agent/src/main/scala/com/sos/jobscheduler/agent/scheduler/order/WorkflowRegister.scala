package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.{NodeKey, WorkflowEvent, WorkflowGraph, WorkflowPath}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class WorkflowRegister {

  private val pathToWorkflow = mutable.Map[WorkflowPath, WorkflowGraph.Named]()
    .withDefault { workflowPath ⇒ throw new NoSuchElementException(s"Unknown $workflowPath") }

  def recover(workflow: WorkflowGraph.Named): Unit = {
    pathToWorkflow.insert(workflow.path → workflow)
  }

  def handleEvent(keyedEvent: KeyedEvent[WorkflowEvent]): Unit = {
    val path = keyedEvent.key
    keyedEvent.event match {
      case WorkflowEvent.WorkflowAttached(graph) ⇒
        pathToWorkflow += path → WorkflowGraph.Named(path, graph)   // Multiple orders with same Workflow may occur. TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed.
    }
  }

  /** Reuses string from workflow to avoid duplicate strings */
  def reuseMemory[S <: Order.State](order: Order[S]): Order[S] = {
    // A more general place for object identification may be the JSON deserializer: it needs access to an reusable object pool
    val workflow = pathToWorkflow(order.workflowPath)
    val nk = workflow.graph.idToNode.get(order.nodeId) match {  // Agent gets a Workflow fragment without node IDs for other agents
      case Some(node) ⇒ NodeKey(workflow.path, node.id)
      case None ⇒ NodeKey(workflow.path, order.nodeId)
    }
    if (order.nodeKey eq nk)
      order
    else {
      assert(order.nodeKey == nk)
      order.copy(nodeKey = nk)
    }
  }

  def nodeKeyToJobNodeOption(nodeKey: NodeKey): Option[WorkflowGraph.JobNode] =
    nodeKeyToNodeOption(nodeKey) collect { case o: WorkflowGraph.JobNode ⇒ o }

  def nodeKeyToNodeOption(nodeKey: NodeKey): Option[WorkflowGraph.Node] =
    pathToWorkflow(nodeKey.workflowPath).graph.idToNode.get(nodeKey.nodeId)

  def get(path: WorkflowPath): Option[WorkflowGraph.Named] =
    pathToWorkflow.get(path)

  def apply(path: WorkflowPath): WorkflowGraph.Named =
    pathToWorkflow(path)

  def namedWorkflowGraphs: Vector[WorkflowGraph.Named] =
    pathToWorkflow.values.toVector

  def size: Int =
    pathToWorkflow.size
}
