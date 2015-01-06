package com.sos.scheduler.engine.data.order

final case class OrderResumedEvent(orderKey: OrderKey)
extends OrderEvent
