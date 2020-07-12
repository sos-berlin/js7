package js7.base.eventbus

import scala.reflect.ClassTag

final class StandardEventBus[E](eventClass: Class[E]) extends ClassEventBus[E]
{
  def this()(implicit e: ClassTag[E]) =
    this(e.runtimeClass.asInstanceOf[Class[E]])

  protected type Classifier = E
  protected type ClassifierToEvent[E1 <: E] = E1

  protected def classifierSuperclass = eventClass

  protected def classify(event: E) = event.getClass
}
