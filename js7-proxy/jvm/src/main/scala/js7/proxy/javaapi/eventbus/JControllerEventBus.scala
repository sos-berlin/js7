package js7.proxy.javaapi.eventbus

import js7.base.annotation.javaApi
import js7.controller.data.ControllerState
import js7.proxy.JournaledStateEventBus
import js7.proxy.javaapi.data.controller.JControllerState

@javaApi
final class JControllerEventBus
extends JJournaledStateEventBus[JControllerState, ControllerState](new JournaledStateEventBus[ControllerState])
