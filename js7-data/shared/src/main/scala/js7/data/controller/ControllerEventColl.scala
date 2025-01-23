package js7.data.controller

import js7.base.problem.Checked
import js7.data.event.{Event, EventColl, KeyedEvent}

type ControllerEventColl[E <: Event] = EventColl[ControllerState, E]

object ControllerEventColl:

  def apply[E <: Event](aggregate: ControllerState): ControllerEventColl[E] =
    EventColl[ControllerState, E](aggregate)

  def tryEvents[E <: Event](controllerState: ControllerState)(keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    ControllerEventColl.keyedEvents(controllerState)(_.add(keyedEvents))

  def keyedEvents[E <: Event](controllerState: ControllerState)
    (body: ControllerEventColl[E] => Checked[ControllerEventColl[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(apply[E](controllerState)).map(_.keyedEvents)
