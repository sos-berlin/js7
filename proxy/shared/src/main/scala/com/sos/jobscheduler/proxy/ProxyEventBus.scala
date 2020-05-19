package com.sos.jobscheduler.proxy

import com.sos.jobscheduler.base.eventbus.ClassEventBus
import com.sos.jobscheduler.data.event.{Event, JournaledState}

final class ProxyEventBus[S <: JournaledState[S]]
extends ClassEventBus[EventAndState[Event, S]]
{
  protected type Classifier = Event
  protected type ClassifierToEvent[E <: Event] = EventAndState[E, S]

  protected def classifierSuperclass = classOf[Event]

  protected def classify(eventAndState: EventAndState[Event, S]) =
    eventAndState.stampedEvent.value.event.getClass.asInstanceOf[Class[Event]]
}
