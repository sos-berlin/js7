package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow._
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}

/**
  * @author Joacim Zschimmer
  */
final case class Workflow(path: WorkflowPath, inputNodeId: NodeId, idToNode: Map[NodeId, Node]) {
  require(idToNode forall { case (k, v) ⇒ k == v.id })

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt inputNodeId)
    require(idToNode.values forall {
      case n: JobNode ⇒ idToNode.contains(n.onSuccess) && idToNode.contains(n.onFailure)
      case _: EndNode ⇒ true
    })
    this
  }

  def reduceForAgent(agentPath: AgentPath): Workflow =
    copy(idToNode = idToNode filter {
      case (_, JobNode(_, `agentPath`, _, _, _))  ⇒ true
      case _ ⇒ false
    })

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

  def apply(path: WorkflowPath, inputNodeId: NodeId, nodes: Iterable[Node]): Workflow = {
    val map = (for (n ← nodes) yield n.id → n).toMap
    require(map.size == nodes.size, "Some nodes does not have unique NodeIds")
    Workflow(path, inputNodeId, map)
  }

  def fromWorkflowAttached(path: WorkflowPath, event: WorkflowEvent.WorkflowAttached): Workflow =
    Workflow(path, event.inputNodeId, event.nodes.toKeyedMap(_.id))

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
  final case class JobNode(id: NodeId, agentPath: AgentPath, jobPath: JobPath, onSuccess: NodeId, onFailure: NodeId) extends Node

  implicit val JsonEncoder: Encoder[Workflow] =
    workflow ⇒ Json.fromJsonObject(JsonObject.fromMap(Map(
      "path" → workflow.path.asJson,
      "inputNodeId" → workflow.inputNodeId.asJson,
      "nodes" → workflow.idToNode.values.asJson)))

  implicit val JsonDecoder: Decoder[Workflow] =
    cursor ⇒
      for {
        path ← cursor.get[WorkflowPath]("path")
        inputNodeId ← cursor.get[NodeId]("inputNodeId")
        nodes ← cursor.get[Vector[Node]]("nodes")
      } yield Workflow(path, inputNodeId, nodes toKeyedMap (_.id))
}
