package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.ModifiableSourceEvent

final case class OrderStepStartedEvent(orderKey: OrderKey, state: OrderState)
extends OrderEvent with ModifiableSourceEvent
