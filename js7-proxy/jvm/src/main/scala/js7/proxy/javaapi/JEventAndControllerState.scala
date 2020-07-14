package js7.proxy.javaapi

import js7.controller.data.ControllerState
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.proxy.EventAndState
import js7.proxy.javaapi.data.JControllerState

final case class JEventAndControllerState[E <: Event](
  stampedEvent: Stamped[KeyedEvent[E]],
  state: JControllerState)
{
  def toScala = EventAndState(stampedEvent, state.underlying)
}

object JEventAndControllerState
{
  def fromScala[E <: Event](eventAndState: EventAndState[E, ControllerState]) =
    new JEventAndControllerState(eventAndState.stampedEvent, JControllerState(eventAndState.state))
}
