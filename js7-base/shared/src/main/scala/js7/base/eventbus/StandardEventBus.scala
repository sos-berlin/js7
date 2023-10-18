package js7.base.eventbus

import js7.base.eventbus.StandardEventBus.logger
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.reflect.ClassTag

final class StandardEventBus[E](eventClass: Class[E]) extends ClassEventBus[E]:
  def this()(implicit e: ClassTag[E]) =
    this(e.runtimeClass.asInstanceOf[Class[E]])

  logger.trace(s"new $toString[${eventClass.simpleScalaName}]")

  protected type Classifier = E
  protected type ClassifierToEvent[E1 <: E] = E1

  protected def classifierSuperclass = eventClass

  protected def classify(event: E) = event.getClass


object StandardEventBus:
  private val logger = Logger[this.type]
