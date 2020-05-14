package com.sos.jobscheduler.base.eventbus

import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import java.util.concurrent.ConcurrentHashMap
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.Promise
import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait ClassEventBus[E] extends EventPublisher[E]
{
  protected type Classifier
  protected type ClassifierToEvent[C <: Classifier] <: E

  protected def classify(event: E): Class[_ <: Classifier]

  private val register = new ConcurrentHashMap[Class[_ <: Classifier], Vector[EventSubscription]]

  private[eventbus] def isEmpty = register.isEmpty

  final def publish(event: E): Unit =
    register.get(classify(event)) match {
      case null =>
      case subscriptions =>
        for (subscription <- subscriptions) {
          try subscription.call(event)
          catch { case NonFatal(t) =>
            scribe.warn(s"Error in event handler ignored: $t", t)
          }
        }
    }

  final def addSubscription(subscription: EventSubscription): Unit =
    register.synchronized {
      for (cls <- subscription.classes) {
        register.put(
          cls,
          register.getOrDefault(cls, Vector.empty) :+ subscription)
      }
    }

  final def removeSubscription(subscription: EventSubscription): Unit =
    register.synchronized {
      for (cls <- subscription.classes) {
        register.get(cls) match {
          case null =>
          case list =>
            val removed = list.filterNot(_ eq subscription)
            if (removed.nonEmpty) {
              register.put(cls.asInstanceOf[Class[Classifier]], removed)
            } else {
              register.remove(cls)
            }
        }
      }
    }

  final def removeAllSubscriptions(): Unit =
    synchronized {
      register.clear()
    }

  final def when[C <: Classifier: ClassTag]: Task[ClassifierToEvent[C]] =
    Task.deferFuture {
      val promise = Promise[ClassifierToEvent[C]]()
      oneShot[C](promise.success)
      promise.future
    }

  final def oneShot[C <: Classifier: ClassTag](handle: ClassifierToEvent[C] => Unit): Unit = {
    val used = AtomicBoolean(false)
    lazy val subscription: EventSubscription =
      toSubscription { event =>
        if (!used.getAndSet(true)) {
          subscription.close()
          handle(event.asInstanceOf[ClassifierToEvent[C]])
        }
      }
    addSubscription(subscription)
  }

  final def subscribe[C <: Classifier: ClassTag](handle: ClassifierToEvent[C] => Unit): EventSubscription =
    subscribeToClass(implicitClass[C])(handle)

  final def subscribeToClass[C <: Classifier](eventClass: Class[C])(handle: ClassifierToEvent[C] => Unit): EventSubscription = {
    val subscription = classToSubscription(eventClass)(handle)
    addSubscription(subscription)
    subscription
  }

  final def toSubscription[C <: Classifier: ClassTag](handle: E => Unit): EventSubscription =
    classToSubscription(implicitClass[C])(handle)

  private def classToSubscription[C <: Classifier](eventClass: Class[C])(handle: ClassifierToEvent[C] => Unit): EventSubscription =
    new EventSubscription(Set(eventClass), handle.asInstanceOf[E => Unit])

  final class EventSubscription(
    private[ClassEventBus] val classes: Set[Class[_ <: Classifier]],
    private[ClassEventBus] val call: E => Unit)
  extends AutoCloseable
  {
    def close() = removeSubscription(this)
  }
}
