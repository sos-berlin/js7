package js7.data.controller

import js7.base.time.Timestamp
import js7.data.event.{Event, EventColl}
import js7.data.state.{EngineEventCalc_, EngineEventColl_}

type ControllerEventCalc =
  ControllerEventCalc_[Event]

type ControllerEventCalc_[E <: Event] =
  EngineEventCalc_[ControllerState, E]

type ControllerEventColl =
  ControllerEventColl_[Event]

type ControllerEventColl_[E <: Event] =
  EngineEventColl_[ControllerState, E]

object ControllerEventColl:

  def apply(aggregate: ControllerState, now: Timestamp)
  : EventColl[ControllerState, Event] =
    EventColl(aggregate, now)

  //inline def keyedEvents[E <: Event](controllerState: ControllerState)
  //  (body: EventCollCtx[ControllerState, E, Any] => Checked[EventCollCtx[ControllerState, E, Any]])
  //: Checked[Vector[KeyedEvent[E]]] =
  //  EventColl.keyedEvents(controllerState)(body)
  //
  //inline def keyedEvents[E <: Event](controllerState: ControllerState, now: Timestamp)
  //  (body: EventColl[ControllerState, E] => Checked[EventColl[ControllerState, E]])
  //: Checked[Vector[KeyedEvent[E]]] =
  //  EventColl.keyedEvents(controllerState, now)(body)
