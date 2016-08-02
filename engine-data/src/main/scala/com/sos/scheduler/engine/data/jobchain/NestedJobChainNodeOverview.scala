package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState
import spray.json.DefaultJsonProtocol._

final case class NestedJobChainNodeOverview(
  nodeKey: NodeKey,
  nextState: OrderState,
  errorState: OrderState,
  nestedJobChainPath: JobChainPath)
extends NodeOverview

object NestedJobChainNodeOverview {
  implicit val MyJsonFormat = jsonFormat4(apply)
}
