package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.ModifiableSourceEvent
import com.fasterxml.jackson.annotation.JsonTypeName

@JsonTypeName("OrderTouchedEvent")
final case class OrderTouchedEvent(orderKey: OrderKey)
extends OrderEvent with ModifiableSourceEvent
