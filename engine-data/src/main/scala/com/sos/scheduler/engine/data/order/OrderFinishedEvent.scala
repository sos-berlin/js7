package com.sos.scheduler.engine.data.order

final case class OrderFinishedEvent(orderKey: OrderKey)
extends OrderEvent
