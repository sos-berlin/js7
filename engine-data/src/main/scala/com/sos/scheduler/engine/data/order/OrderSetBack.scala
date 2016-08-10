package com.sos.scheduler.engine.data.order

final case class OrderSetBack(orderKey: OrderKey, state: OrderState)
extends OrderEvent
