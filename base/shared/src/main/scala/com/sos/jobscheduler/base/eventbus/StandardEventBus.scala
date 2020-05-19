package com.sos.jobscheduler.base.eventbus

import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

final class StandardEventBus[E: ClassTag] extends ClassEventBus[E]
{
  protected type Classifier = E
  protected type ClassifierToEvent[E1 <: E] = E1

  protected def classifierSuperclass = implicitClass[E]

  protected def classify(event: E) =
    event.getClass
}
