package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.jobchain.NodeId
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderDetailed(
  overview: OrderOverview,
  priority: Int,
  initialNodeId: Option[NodeId] = None,
  endNodeId: Option[NodeId] = None,
  title: String,
  variables: Map[String, String] = Map())
extends OrderView {

  def path = orderKey

  def orderKey = overview.orderKey

  private[engine] def occupyingClusterMemberId = overview.occupyingClusterMemberId

  private[engine] def orderProcessingState = overview.orderProcessingState

  private[engine] def nodeKey = overview.nodeKey
}

object OrderDetailed extends OrderView.Companion[OrderDetailed] {
  implicit val jsonFormat = jsonFormat6(apply)
}
