package js7.data.controller

import js7.data.event.{Event, EventColl}

type ControllerEventColl = EventColl[ControllerState, Event]

object ControllerEventColl:

  def apply(aggregate: ControllerState): ControllerEventColl =
    EventColl[ControllerState, Event](aggregate)
