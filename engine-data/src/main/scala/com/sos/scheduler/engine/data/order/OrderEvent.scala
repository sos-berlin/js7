package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.KeyedEvent

trait OrderEvent extends KeyedEvent {

  type Key = OrderKey

  final def key =
    orderKey

  def orderKey: OrderKey
}
