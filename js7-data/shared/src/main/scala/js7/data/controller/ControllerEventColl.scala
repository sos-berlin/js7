package js7.data.controller

import js7.base.problem.Checked
import js7.data.event.{Event, EventColl, KeyedEvent}

object ControllerEventColl:

  inline def keyedEvents[E <: Event](controllerState: ControllerState)
    (body: EventColl[ControllerState, E, Any] => Checked[EventColl[ControllerState, E, Any]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(controllerState)(body)

  //inline def keyedEvents[E <: Event](controllerState: ControllerState, now: => Timestamp)
  //  (body: EventColl[ControllerState, E, TimeCtx] => Checked[EventColl[ControllerState, E, TimeCtx]])
  //: Checked[Vector[KeyedEvent[E]]] =
  //  EventColl.keyedEvents(controllerState, TimeCtx(now))(body)
