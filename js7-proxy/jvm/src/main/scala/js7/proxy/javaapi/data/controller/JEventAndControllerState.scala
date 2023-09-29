package js7.proxy.javaapi.data.controller

import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.data.controller.ControllerState
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.controller.JControllerState
import js7.proxy.data.event.EventAndState

@javaApi
final case class JEventAndControllerState[E <: Event](asScala: EventAndState[E, ControllerState])
extends JavaWrapper:
  type AsScala = EventAndState[E, ControllerState]

  @Nonnull
  def stampedEvent: Stamped[KeyedEvent[E]] =
    asScala.stampedEvent

  @Nonnull
  def state: JControllerState =
    JControllerState(asScala.state)

  @Nonnull
  def previousState: JControllerState =
    JControllerState(asScala.previousState)
