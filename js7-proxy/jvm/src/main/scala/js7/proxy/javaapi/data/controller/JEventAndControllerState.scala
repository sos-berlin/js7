package js7.proxy.javaapi.data.controller

import js7.base.annotation.javaApi
import js7.controller.data.ControllerState
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.proxy.data.event.EventAndState
import js7.proxy.javaapi.data.common.JavaWrapper

@javaApi
final case class JEventAndControllerState[E <: Event](underlying: EventAndState[E, ControllerState])
extends JavaWrapper
{
  protected type Underlying = EventAndState[E, ControllerState]

  def stampedEvent: Stamped[KeyedEvent[E]] =
    underlying.stampedEvent

  def state: JControllerState =
    JControllerState(underlying.state)

  def previousState: JControllerState =
    JControllerState(underlying.previousState)
}
