package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import com.sos.jobscheduler.data.engine2.order.Jobnet._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class Jobnet(path: JobnetPath, inputNodeId: NodeId, idToNode: Map[NodeId, Node]) {
  require(idToNode forall { case (k, v) ⇒ k == v.id })

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt inputNodeId)
    require(idToNode.values forall {
      case n: JobNode ⇒ idToNode.contains(n.onSuccess) && idToNode.contains(n.onFailure)
      case _: EndNode ⇒ true
    })
    this
  }

  def reduceForAgent(agentPath: AgentPath): Jobnet =
    copy(idToNode = idToNode filter {
      case (_, JobNode(_, `agentPath`, _, _, _))  ⇒ true
      case _ ⇒ false
    })

  def apply(nodeId: NodeId) = idToNode(nodeId)

  def isDefinedAt(nodeId: NodeId) = idToNode isDefinedAt nodeId
}

object Jobnet {

  def apply(path: JobnetPath, inputNodeId: NodeId, nodes: Iterable[Node]): Jobnet = {
    val map = (for (n ← nodes) yield n.id → n).toMap
    require(map.size == nodes.size, "Some nodes does not have unique NodeIds")
    Jobnet(path, inputNodeId, map)
  }

  def fromJobnetAttached(path: JobnetPath, event: JobnetEvent.JobnetAttached): Jobnet =
    Jobnet(path, event.inputNodeId, event.idToNode)

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

  implicit val jsonFormat = jsonFormat3 { (path: JobnetPath, input: NodeId, o: Map[NodeId, Node]) ⇒ Jobnet(path, input, o) }
}
