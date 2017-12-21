package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.DuplicateKeyException
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow._
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.TransitionRegister
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class Workflow private(path: WorkflowPath, graph: WorkflowGraph) {

  val forkNodeToJoiningTransition = graph.allTransitions.map(o ⇒ o.forkNodeId → o).collect { case (Some(forkNodeId), t) ⇒ forkNodeId → t }
    .toMap withNoSuchKey (k ⇒ new NoSuchElementException(s"No joining transition for forking node '$k'"))
  val nodeToOutputTransition = (for (t ← graph.allTransitions; nodeId ← t.fromProcessedNodeIds) yield nodeId → t)
    .uniqueToMap(duplicates ⇒ new DuplicateKeyException(s"Nodes with duplicate transitions: ${duplicates.mkString(", ")}"))
    .withNoSuchKey(k ⇒ new NoSuchElementException(s"No output transition for Workflow.Node '$k'"))

  def requireCompleteness: this.type = {
    graph.requireCompleteness
    this
  }

  def reduceForAgent(agentPath: AgentPath) = copy(graph = graph.reduceForAgent(agentPath))

  def apply(nodeId: NodeId) = idToNode(nodeId)

  def isDefinedAt(nodeId: NodeId) = idToNode isDefinedAt nodeId

  def jobNodeOption(nodeId: NodeId): Option[JobNode] =
    idToNode.get(nodeId) collect { case o: JobNode ⇒ o }

  def jobNode(nodeId: NodeId) = idToNode(nodeId).asInstanceOf[JobNode]

  def jobNodeCount = idToNode.values count { _.isInstanceOf[JobNode] }

  def agentPathOption(nodeId: NodeId): Option[AgentPath] =
    idToNode.get(nodeId) collect { case o: Workflow.JobNode ⇒ o.agentPath }

  def idToNode: Map[NodeId, Node] = graph.idToNode

  def startNodeKey = NodeKey(path, graph.start)

  def start: NodeId = graph.start
}

object Workflow {
  def apply(path: WorkflowPath, graph: WorkflowGraph): Workflow =
    new Workflow(path, graph)

  def apply(path: WorkflowPath, start: NodeId, nodes: Seq[Node], transitions: Seq[Transition]): Workflow =
    Workflow(path, WorkflowGraph(start, nodes, transitions))

  def fromWorkflowAttached(path: WorkflowPath, event: WorkflowEvent.WorkflowAttached): Workflow =
    event.workflow

  sealed trait Node {
    def id: NodeId
  }

  object Node {
    implicit val JsonCodec: CirceCodec[Node] = TypedJsonCodec[Node](
      Subtype[JobNode],
      Subtype[EndNode])
  }

  @JsonCodec
  final case class EndNode(id: NodeId) extends Node

  @JsonCodec
  final case class JobNode(id: NodeId, job: AgentJobPath) extends Node {
    def agentPath = job.agentPath
    def jobPath = job.jobPath
  }
  object JobNode {
    def apply(id: NodeId, agentPath: AgentPath, jobPath: JobPath) =
      new JobNode(id, AgentJobPath(agentPath, jobPath))
  }

  implicit val JsonCodec = {
    implicit val TransitionRegisterCodec = TransitionRegister.JsonCodec
    deriveCirceCodec[Workflow]
  }
}
