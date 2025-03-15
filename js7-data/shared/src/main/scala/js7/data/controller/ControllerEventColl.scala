package js7.data.controller

import js7.base.problem.Checked
import js7.data.event.{Event, EventColl, KeyedEvent}

object ControllerEventColl:

  //private def apply[E <: Event](aggregate: ControllerState): EventColl[ControllerState, E, Any] =
  //  EventColl(aggregate, ())
  //
  //private def apply[E <: Event](aggregate: ControllerState, now: => Timestamp)
  //: EventColl[ControllerState, E, TimeCtx] =
  //  EventColl(aggregate, TimeCtx(now))

  def tryEvents[E <: Event](controllerState: ControllerState)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    ControllerEventColl.keyedEvents(controllerState)(_.add(keyedEvents))

  inline def keyedEvents[E <: Event](controllerState: ControllerState)
    (body: EventColl[ControllerState, E, Any] => Checked[EventColl[ControllerState, E, Any]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(controllerState)(body)

  //inline def keyedEvents[E <: Event](controllerState: ControllerState, now: => Timestamp)
  //  (body: EventColl[ControllerState, E, TimeCtx] => Checked[EventColl[ControllerState, E, TimeCtx]])
  //: Checked[Vector[KeyedEvent[E]]] =
  //  EventColl.keyedEvents(controllerState, TimeCtx(now))(body)
