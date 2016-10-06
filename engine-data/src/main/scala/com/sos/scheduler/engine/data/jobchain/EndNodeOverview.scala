package com.sos.scheduler.engine.data.jobchain

import spray.json.DefaultJsonProtocol._

final case class EndNodeOverview(
  jobChainPath: JobChainPath,
  nodeId: NodeId)
extends NodeOverview

object EndNodeOverview {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
