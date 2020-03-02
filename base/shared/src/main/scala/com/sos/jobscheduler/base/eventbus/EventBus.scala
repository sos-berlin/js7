package com.sos.jobscheduler.base.eventbus

import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import java.util.concurrent.ConcurrentHashMap
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

final class EventBus
{
  private val register = new ConcurrentHashMap[Class[_], Vector[EventSubscription[_]]]

  private[eventbus] def isEmpty = register.isEmpty

  def publish(event: AnyRef): Unit =
    register.get(event.getClass) match {
      case null =>
      case subscriptions =>
        for (subscription <- subscriptions) {
          try subscription.call(event)
          catch { case NonFatal(t) =>
            scribe.warn(s"Error in event handler ignored: $t", t)
          }
        }
    }

  def when[E <: AnyRef: ClassTag]: Future[E] = {
    val promise = Promise[E]()
    oneShot[E](promise.success)
    promise.future
  }

  def oneShot[E <: AnyRef: ClassTag](handle: E => Unit): Unit = {
    val used = AtomicBoolean(false)
    lazy val subscription: EventSubscription[E] =
      EventSubscription { event =>
        if (!used.getAndSet(true)) {
          remove(subscription)
          handle(event)
        }
      }
    subscribe(subscription)
  }

  def subscribe[E <: AnyRef: ClassTag](handle: E => Unit): EventSubscription[E] =
    subscribeToClass(implicitClass[E])(handle)

  def subscribeToClass[E <: AnyRef](eventClass: Class[E])(handle: E => Unit): EventSubscription[E] = {
    val subscription = EventSubscription.forClass(eventClass)(handle)
    subscribe(subscription)
    subscription
  }

  private def subscribe[E <: AnyRef](subscription: EventSubscription[E]): Unit =
    register.synchronized {
      register.put(
        subscription.eventClass,
        register.getOrDefault(subscription.eventClass, Vector.empty) :+ subscription)
    }

  private def remove[E <: AnyRef](subscription: EventSubscription[E]): Unit =
    register.synchronized {
      register.get(subscription.eventClass) match {
        case null =>
        case list =>
          val removed = list.filterNot(_ eq subscription)
          if (removed.nonEmpty) {
            register.put(subscription.eventClass, removed)
          } else {
            register.remove(subscription.eventClass)
          }
      }
    }

  final class EventSubscription[E <: AnyRef](
    private[EventBus] val eventClass: Class[E],
    private[EventBus] val call: AnyRef => Unit)
  extends AutoCloseable
  {
    def close() = remove(this)
  }
  private object EventSubscription {
    def apply[E <: AnyRef: ClassTag](handle: E => Unit): EventSubscription[E] =
      forClass(implicitClass[E])(handle)

    def forClass[E <: AnyRef](eventClass: Class[E])(handle: E => Unit): EventSubscription[E] =
      new EventSubscription(eventClass, handle.asInstanceOf[AnyRef => Unit])
  }
}
