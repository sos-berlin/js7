package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState
import spray.json._

trait NodeOverview {
  val orderState: OrderState
}


object NodeOverview {
  implicit object MyJsonFormat extends RootJsonFormat[NodeOverview]{
    override def write(o: NodeOverview) =
      o match {
        case o: SimpleJobNodeOverview ⇒ o.toJson
        case o: SinkNodeOverview ⇒ o.toJson
        case o: NestedJobChainNodeOverview ⇒ o.toJson
        case o: EndNodeOverview ⇒ o.toJson
      }

    override def read(json: JsValue) = throw new UnsupportedOperationException
  }
}
