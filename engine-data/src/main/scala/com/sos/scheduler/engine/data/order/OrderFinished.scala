package com.sos.scheduler.engine.data.order

final case class OrderFinished(orderKey: OrderKey, state: OrderState)
extends OrderEvent
