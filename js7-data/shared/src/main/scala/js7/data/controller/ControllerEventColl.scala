package js7.data.controller

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.event.{Event, EventColl, EventCollCtx, KeyedEvent}

object ControllerEventColl:

  inline def keyedEvents[E <: Event](controllerState: ControllerState)
    (body: EventCollCtx[ControllerState, E, Any] => Checked[EventCollCtx[ControllerState, E, Any]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(controllerState)(body)

  inline def keyedEvents[E <: Event](controllerState: ControllerState, now: Timestamp)
    (body: EventColl[ControllerState, E] => Checked[EventColl[ControllerState, E]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(controllerState, now)(body)
