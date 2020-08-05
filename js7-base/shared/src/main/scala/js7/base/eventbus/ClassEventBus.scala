package js7.base.eventbus

import java.util.concurrent.ConcurrentHashMap
import js7.base.utils.ScalaUtils.implicitClass
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.Promise
import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait ClassEventBus[E] extends EventPublisher[E] with AutoCloseable
{
  protected type Classifier
  protected type ClassifierToEvent[C <: Classifier] <: E
  private type Cls = Class[_ <: Classifier]

  protected def classifierSuperclass: Class[Classifier]
  protected def classify(event: E): Cls

  private val register = new ConcurrentHashMap[Cls, Vector[EventSubscription]]
  private val superclassCache = new SuperclassCache(classifierSuperclass)

  private[eventbus] def isEmpty = register.isEmpty

  def close() = removeAllSubscriptions()

  final def publish(event: E): Unit =
    for (cls <- superclassCache.assignableClasses(classify(event))) {   // Optimizable in addSubscription ???
      register.get(cls) match {
        case null =>
        case subscriptions =>
          for (subscription <- subscriptions) {
            try subscription.call(event)
            catch { case NonFatal(t) =>
              scribe.error(s"Error in event handler ignored: $t", t)
            }
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
    private[ClassEventBus] val classes: Set[Cls],
    private[ClassEventBus] val call: E => Unit)
  extends AutoCloseable
  {
    //val assignableClasses: Set[Cls] = classes.asInstanceOf[Set[Cls]]
    //  .flatMap(c => superclassCache.assignableClasses(c).asInstanceOf[Set[Cls]])

    def close() = removeSubscription(this)
  }
}
