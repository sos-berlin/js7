package js7.proxy.javaapi.eventbus

import js7.base.annotation.javaApi
import js7.data.controller.ControllerState
import js7.data_for_java.controller.JControllerState
import js7.proxy.JournaledStateEventBus

@javaApi
final class JControllerEventBus
extends JJournaledStateEventBus[JControllerState, ControllerState](new JournaledStateEventBus[ControllerState])
