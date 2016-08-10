package com.sos.scheduler.engine.data.order

final case class OrderSuspended(orderKey: OrderKey)
extends OrderEvent
