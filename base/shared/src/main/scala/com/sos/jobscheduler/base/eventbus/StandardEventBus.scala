package com.sos.jobscheduler.base.eventbus

final class StandardEventBus[E] extends ClassEventBus[E]
{
  protected type Classifier = E
  protected type ClassifierToEvent[E1 <: E] = E1

  protected def classify(event: E) =
    event.getClass
}
