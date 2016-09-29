package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import spray.json.DefaultJsonProtocol._

final case class SinkNodeOverview(
  nodeKey: NodeKey,
  nextNodeId: NodeId,
  errorNodeId: NodeId,
  jobPath: JobPath,
  action: JobChainNodeAction,
  orderCount: Int,
  obstacles: Set[NodeObstacle])
extends JobNodeOverview

object SinkNodeOverview {
  private implicit val OrderStateJsonFormat = NodeId.MyJsonFormat
  private implicit val JobChainNodeActionJsonFormat = JobChainNodeAction.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat7(apply)
}
