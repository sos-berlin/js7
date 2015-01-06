package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.{AbstractEvent, ModifiableSourceEvent}

final case class OrderStepEndedEvent(orderKey: OrderKey, stateTransition: OrderStateTransition)
extends AbstractEvent  // Für @ForCpp: Funktionen müssen eine Klasse, kein Interface liefern
with OrderEvent with ModifiableSourceEvent
