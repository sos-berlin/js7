package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.ModifiableSourceEvent
import com.fasterxml.jackson.annotation.JsonTypeName

@JsonTypeName("OrderStarted")
final case class OrderStarted(orderKey: OrderKey)
extends OrderEvent with ModifiableSourceEvent
