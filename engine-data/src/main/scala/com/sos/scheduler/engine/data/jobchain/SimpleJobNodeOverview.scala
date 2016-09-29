package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import spray.json.DefaultJsonProtocol._

final case class SimpleJobNodeOverview(
  jobChainPath: JobChainPath,
  nodeId: NodeId,
  nextNodeId: NodeId,
  errorNodeId: NodeId,
  jobPath: JobPath,
  action: JobChainNodeAction = JobChainNodeAction.process,
  orderCount: Int,
  obstacles: Set[NodeObstacle] = Set())
extends JobNodeOverview

object SimpleJobNodeOverview {
  private implicit val OrderStateJsonFormat = NodeId.MyJsonFormat
  private implicit val JobChainNodeActionJsonFormat = JobChainNodeAction.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat8(apply)
}
