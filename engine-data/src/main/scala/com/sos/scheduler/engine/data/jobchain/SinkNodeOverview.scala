package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import spray.json.DefaultJsonProtocol._

final case class SinkNodeOverview(
  nodeKey: NodeKey,
  nextNodeId: NodeId,
  errorNodeId: NodeId,
  action: JobChainNodeAction,
  jobPath: JobPath,
  orderCount: Int)
extends JobNodeOverview

object SinkNodeOverview {
  private implicit val OrderStateJsonFormat = NodeId.MyJsonFormat
  private implicit val JobChainNodeActionJsonFormat = JobChainNodeAction.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat6(apply)
}
