package js7.data.controller

import js7.base.problem.Checked
import js7.data.event.{Event, EventCalc, EventColl, TimeCtx}

type ControllerEventCalc[E <: Event] = EventCalc[ControllerState, E, TimeCtx]


object ControllerEventCalc:
  def apply[E <: Event](
    function: EventColl[ControllerState, E, TimeCtx] => Checked[EventColl[ControllerState, E, TimeCtx]]
  ): EventCalc[ControllerState, E, TimeCtx] =
    EventCalc(function)