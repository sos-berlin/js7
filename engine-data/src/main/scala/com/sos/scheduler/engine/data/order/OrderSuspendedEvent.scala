package com.sos.scheduler.engine.data.order

final case class OrderSuspendedEvent(orderKey: OrderKey)
extends OrderEvent