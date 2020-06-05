package js7.base.eventbus

import js7.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

final class StandardEventBus[E: ClassTag] extends ClassEventBus[E]
{
  protected type Classifier = E
  protected type ClassifierToEvent[E1 <: E] = E1

  protected def classifierSuperclass = implicitClass[E]

  protected def classify(event: E) =
    event.getClass
}
