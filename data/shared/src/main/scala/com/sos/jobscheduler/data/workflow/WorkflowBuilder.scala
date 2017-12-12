package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.workflow.Workflow.{JobNode, Node}
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowBuilder private(start: JobNode) {

  private val nodes = mutable.Buffer[Node]()
  private val transitions = mutable.Buffer[Transition]()

  nodes += start

  def fork(join: Node, routes: IndexedSeq[WorkflowRoute]): this.type = {
    val (f, j) =
      Transition.forkJoin(
        forkNodeId = nodes.last.id,
        joinNodeId = join.id,
        routes = routes,
        childEndNodes = routes.map(_.end),
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

  def toRoute(id: WorkflowRoute.Id = WorkflowRoute.Id.empty): WorkflowRoute =
    WorkflowRoute(id,
      start = nodes.head.id,
      end = nodes.last.id,
      nodes.toVector,
      transitions.toVector)

  def toWorkflow(path: WorkflowPath): Workflow =
    Workflow(path, toRoute())
}

object WorkflowBuilder {
  def startWith(start: JobNode) = new WorkflowBuilder(start)
}
