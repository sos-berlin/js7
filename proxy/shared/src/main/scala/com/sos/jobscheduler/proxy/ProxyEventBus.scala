package js7.proxy

import js7.base.eventbus.ClassEventBus
import js7.data.event.{Event, JournaledState}

final class ProxyEventBus[S <: JournaledState[S]]
extends ClassEventBus[EventAndState[Event, S]]
{
  protected type Classifier = Event
  protected type ClassifierToEvent[E <: Event] = EventAndState[E, S]

  protected def classifierSuperclass = classOf[Event]

  protected def classify(eventAndState: EventAndState[Event, S]) =
    eventAndState.stampedEvent.value.event.getClass.asInstanceOf[Class[Event]]
}
