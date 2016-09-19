package com.sos.scheduler.engine.data.jobchain

import spray.json.DefaultJsonProtocol._

final case class EndNodeOverview(nodeKey: NodeKey)
extends NodeOverview

object EndNodeOverview {
  implicit val MyJsonFormat = jsonFormat1(apply)
}
