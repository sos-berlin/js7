package js7.proxy.javaapi.data.controller

import js7.base.annotation.javaApi
import js7.controller.data.ControllerState
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.proxy.data.event.EventAndState
import js7.proxy.javaapi.data.common.JavaWrapper

@javaApi
final case class JEventAndControllerState[E <: Event](asScala: EventAndState[E, ControllerState])
extends JavaWrapper
{
  protected type AsScala = EventAndState[E, ControllerState]

  def stampedEvent: Stamped[KeyedEvent[E]] =
    asScala.stampedEvent

  def state: JControllerState =
    JControllerState(asScala.state)

  def previousState: JControllerState =
    JControllerState(asScala.previousState)
}
