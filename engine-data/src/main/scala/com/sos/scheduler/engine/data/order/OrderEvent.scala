package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat.Subtype
import com.sos.scheduler.engine.data.event.KeyedEvent
import spray.json.DefaultJsonProtocol._

trait OrderEvent extends KeyedEvent {

  type Key = OrderKey

  final def key = orderKey

  def orderKey: OrderKey
}

object OrderEvent {
  implicit val OrderEventJsonFormat = TypedJsonFormat[OrderEvent](
    Subtype(jsonFormat2(OrderFinished)),
    Subtype(jsonFormat1(OrderNestedFinished)),
    Subtype(jsonFormat1(OrderNestedStarted)),
    Subtype(jsonFormat1(OrderResumed)),
    Subtype(jsonFormat2(OrderSetBack)),
    Subtype(jsonFormat3(OrderNodeChanged)),
    Subtype(jsonFormat2(OrderStepEnded)),
    Subtype(jsonFormat3(OrderStepStarted)),
    Subtype(jsonFormat1(OrderSuspended)),
    Subtype(jsonFormat1(OrderStarted)))
}
