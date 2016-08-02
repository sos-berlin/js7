package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class NodeKey(jobChainPath: JobChainPath, state: OrderState) {
  override def toString = s"${jobChainPath.string}:${state.string}"
}

object NodeKey {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
