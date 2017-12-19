package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.workflow.Workflow.{JobNode, Node}
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import scala.collection.immutable.ListMap
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowBuilder private(start: JobNode) {

  private val nodes = mutable.Buffer[Node]()
  private val transitions = mutable.Buffer[Transition]()

  nodes += start

  def fork(join: Node, idToRoute: ListMap[WorkflowRoute.Id, WorkflowRoute]): this.type = {
    val (f, j) =
      Transition.forkJoin(
        forkNodeId = nodes.last.id,
        joinNodeId = join.id,
        idToRoute = idToRoute,
        ForkTransition,
        JoinTransition)
    transitions += f
    transitions += j
    this
  }

  def forwardTo(node: Node): this.type = {
    transitions += Transition(from = nodes.last.id, to = node.id, ForwardTransition)
    addNode(node)
    this
  }

  private def addNode(node: Node) = {
    require(!nodes.exists(_.id == node.id))
    nodes += node
  }

  def toRoute: WorkflowRoute =
    WorkflowRoute(start = nodes.head.id, nodes.toVector, transitions.toVector)

  def toWorkflow(path: WorkflowPath): Workflow =
    Workflow(path, toRoute)
}

object WorkflowBuilder {
  def startWith(start: JobNode) = new WorkflowBuilder(start)
}
