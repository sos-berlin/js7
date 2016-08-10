package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat.Subtype
import com.sos.scheduler.engine.data.event.KeyedEvent
import spray.json.DefaultJsonProtocol._

trait OrderEvent extends KeyedEvent {

  type Key = OrderKey

  final def key =
    orderKey

  def orderKey: OrderKey
}

object OrderEvent {
  implicit val OrderEventJsonFormat = TypedJsonFormat[OrderEvent](
    Subtype(jsonFormat2(OrderFinishedEvent      ), "OrderFinished"),
    Subtype(jsonFormat1(OrderNestedFinishedEvent), "OrderNestedFinished"),
    Subtype(jsonFormat1(OrderNestedTouchedEvent ), "OrderNestedTouched"),
    Subtype(jsonFormat1(OrderResumedEvent       ), "OrderResumed"),
    Subtype(jsonFormat2(OrderSetBackEvent       ), "OrderSetBack"),
    Subtype(jsonFormat3(OrderStateChangedEvent  ), "OrderStateChanged"),
    Subtype(jsonFormat2(OrderStepEndedEvent     ), "OrderStepEnded"),
    Subtype(jsonFormat3(OrderStepStartedEvent   ), "OrderStepStarted"),
    Subtype(jsonFormat1(OrderSuspendedEvent     ), "OrderSuspended"),
    Subtype(jsonFormat1(OrderTouchedEvent       ), "OrderTouched"))
}
