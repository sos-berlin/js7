package js7.data.controller

import js7.base.problem.Checked
import js7.data.event.{Event, EventColl, KeyedEvent}

type ControllerEventColl[E <: Event] = EventColl[ControllerState, E]

object ControllerEventColl:

  def apply[E <: Event](aggregate: ControllerState): ControllerEventColl[E] =
    EventColl[ControllerState, E](aggregate)

  def keyedEvents[E <: Event](aggregate: ControllerState)
    (body: ControllerEventColl[E] => Checked[ControllerEventColl[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(apply[E](aggregate)).map(_.keyedEvents)
