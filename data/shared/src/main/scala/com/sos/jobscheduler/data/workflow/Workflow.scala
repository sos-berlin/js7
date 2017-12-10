package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.RichMap
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichPairTraversable, RichTraversable}
import com.sos.jobscheduler.base.utils.DuplicateKeyException
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow._
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.TransitionRegister
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
final case class Workflow(path: WorkflowPath, inputNodeId: NodeId, transitions: Seq[Transition], unconnectedNodes: Seq[Node] = Nil) {

  val nodes = (transitions.flatMap(_.nodes) ++ unconnectedNodes).toVector.distinct
  val idToNode = nodes toKeyedMap { _.id }
  val forkNodeToJoiningTransition = transitions.map(o ⇒ o.forkNodeId → o).collect { case (Some(forkNodeId), t) ⇒ forkNodeId → t } .toMap
    .withNoSuchKey(k ⇒ new NoSuchElementException(s"No joining transition for forking node '$k'"))
  //private[workflow] val nodeToInputTransition = (for (t ← transitions; nodeId ← t.toNodeIds) yield nodeId → t)
  //  .uniqueToMap(duplicates ⇒ new DuplicateKeyException(s"Nodes with duplicate transitions: ${duplicates.mkString(", ")}"))
  //  .withNoSuchKey(k ⇒ new NoSuchElementException(s"No input transition for Workflow.Node '$k'"))
  val nodeToOutputTransition = (for (t ← transitions; nodeId ← t.fromProcessedNodeIds) yield nodeId → t)
    .uniqueToMap(duplicates ⇒ new DuplicateKeyException(s"Nodes with duplicate transitions: ${duplicates.mkString(", ")}"))
    .withNoSuchKey(k ⇒ new NoSuchElementException(s"No output transition for Workflow.Node '$k'"))

  require(idToNode.size == nodes.size, s"$path contains duplicate NodeIds")
  (for (t ← transitions; to ← t.toNodeIds) yield to → t).uniqueToMap  // throws if not unique

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt inputNodeId)
    this
  }

  def reduceForAgent(agentPath: AgentPath): Workflow = {
    val agentNodes = nodes.collect { case JobNode(id, `agentPath`, _) ⇒ id }.toSet
    val agentTransitions = transitions filter (_.nodeIds forall agentNodes)
    val transitionNodes = agentTransitions flatMap (_.nodeIds)
    copy(
      transitions = agentTransitions,
      unconnectedNodes = nodes.filter(o ⇒ agentNodes.contains(o.id) && !transitionNodes.contains(o.id)).distinct)
  }

  def apply(nodeId: NodeId) = idToNode(nodeId)

  def isDefinedAt(nodeId: NodeId) = idToNode isDefinedAt nodeId

  def jobNodeOption(nodeId: NodeId): Option[JobNode] =
    idToNode.get(nodeId) collect { case o: JobNode ⇒ o }

  def jobNode(nodeId: NodeId) = idToNode(nodeId).asInstanceOf[JobNode]

  def jobNodeCount = idToNode.values count { _.isInstanceOf[JobNode] }

  def agentPathOption(nodeId: NodeId): Option[AgentPath] =
    idToNode.get(nodeId) collect { case o: Workflow.JobNode ⇒ o.agentPath }

  def inputNodeKey = NodeKey(path, inputNodeId)
}

object Workflow {

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
  final case class JobNode(id: NodeId, agentPath: AgentPath, jobPath: JobPath) extends Node

  implicit val JsonCodec = {
    implicit val TransitionRegisterCodec = TransitionRegister.JsonCodec
    deriveCirceCodec[Workflow]
  }
}
