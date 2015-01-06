package com.sos.scheduler.engine.data.order

final case class OrderSetBackEvent(orderKey: OrderKey, state: OrderState)
extends OrderEvent
