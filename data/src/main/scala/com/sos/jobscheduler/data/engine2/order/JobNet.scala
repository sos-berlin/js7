package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import com.sos.jobscheduler.data.engine2.order.JobNet._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class JobNet(path: JobChainPath, inputNodeId: NodeId, idToNode: Map[NodeId, Node]) {
  require(idToNode forall { case (k, v) ⇒ k == v.id })

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt inputNodeId)
    require(idToNode.values forall {
      case n: JobNode ⇒ idToNode.contains(n.onSuccess) && idToNode.contains(n.onFailure)
      case _: EndNode ⇒ true
    })
    this
  }

  def reduceForAgent(agentPath: AgentPath): JobNet =
    copy(idToNode = idToNode filter {
      case (_, JobNode(_, `agentPath`, _, _, _))  ⇒ true
      case _ ⇒ false
    })

  def apply(nodeId: NodeId) = idToNode(nodeId)

  def isDefinedAt(nodeId: NodeId) = idToNode isDefinedAt nodeId
}

object JobNet {

  def apply(path: JobChainPath, inputNodeId: NodeId, nodes: Iterable[Node]): JobNet = {
    val map = (for (n ← nodes) yield n.id → n).toMap
    require(map.size == nodes.size, "Some nodes does not have unique NodeIds")
    JobNet(path, inputNodeId, map)
  }

  def fromJobnetAttached(path: JobChainPath, event: JobnetEvent.JobnetAttached): JobNet =
    JobNet(path, event.inputNodeId, event.idToNode)

  sealed trait Node {
    def id: NodeId
  }

  object Node {
    implicit val jsonFormat = TypedJsonFormat[Node](
      Subtype(jsonFormat5(JobNode)),
      Subtype(jsonFormat1(EndNode)))
  }

  final case class EndNode(id: NodeId) extends Node

  final case class JobNode(id: NodeId, agentPath: AgentPath, jobPath: JobPath, onSuccess: NodeId, onFailure: NodeId) extends Node

  implicit val jsonFormat = jsonFormat3 { (path: JobChainPath, input: NodeId, o: Map[NodeId, Node]) ⇒ JobNet(path, input, o) }
}
