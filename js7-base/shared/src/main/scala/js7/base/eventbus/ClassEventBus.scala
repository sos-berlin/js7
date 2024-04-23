package js7.base.eventbus

import cats.effect.IO
import java.util.concurrent.ConcurrentHashMap
import js7.base.eventbus.ClassEventBus.*
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.{Atomic, SuperclassCache}
import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait ClassEventBus[E] extends EventPublisher[E], AutoCloseable:

  protected type Classifier
  protected type ClassifierToEvent[C <: Classifier] <: E
  private type Cls = Class[? <: Classifier]

  protected def classifierSuperclass: Class[Classifier]
  protected def classify(event: E): Cls

  private val register = new ConcurrentHashMap[Cls, Vector[EventSubscription]]
  private val superclassCache = new SuperclassCache(classifierSuperclass)

  private[eventbus] def isEmpty = register.isEmpty

  def close(): Unit =
    removeAllSubscriptions()

  final def publish(event: E): Unit =
    for cls <- superclassCache.assignableClasses(classify(event)) do   // Optimizable in addSubscription ???
      register.get(cls) match
        case null =>
        case subscriptions =>
          for subscription <- subscriptions do
            try subscription.call(event)
            catch { case NonFatal(t) =>
              logger.error(s"Error in event handler ignored: $t", t)
            }

  final def addSubscription(subscription: EventSubscription): Unit =
    register.synchronized:
      for cls <- subscription.classes do
        register.put(
          cls,
          register.getOrDefault(cls, Vector.empty) :+ subscription)

  final def removeSubscription(subscription: EventSubscription): Unit =
    register.synchronized:
      for cls <- subscription.classes do
        register.get(cls) match
          case null =>
          case list =>
            val removed = list.filterNot(_ eq subscription)
            if removed.nonEmpty then
              register.put(cls.asInstanceOf[Class[Classifier]], removed)
            else
              register.remove(cls)

  final def removeAllSubscriptions(): Unit =
    synchronized:
      register.clear()

  // TODO Replace IO-returning call with Future-returning calls.
  // Denn da beginnt die Überwachung sofort und nicht irgendwann.
  final def when[C <: Classifier : ClassTag]: Future[ClassifierToEvent[C]] =
    when_[C](_ => true)

  final def when_[C <: Classifier: ClassTag](predicate: ClassifierToEvent[C] => Boolean)
  : Future[ClassifierToEvent[C]] =
      val promise = Promise[ClassifierToEvent[C]]()
      oneShot[C](predicate)(promise.success)
      promise.future

  final def whenPF[C <: Classifier: ClassTag, D](pf: PartialFunction[ClassifierToEvent[C], D])
  : IO[D] =
    whenFilterMap(pf.lift)

  final def whenFilterMap[C <: Classifier: ClassTag, D](f: ClassifierToEvent[C] => Option[D]): IO[D] =
    IO.fromFuture(IO:
      val promise = Promise[D]()
      oneShotFilterMap(f)(promise.success)
      promise.future)

  @deprecated("Use when")
  final def whenFuture[C <: Classifier : ClassTag]: Future[ClassifierToEvent[C]] =
    whenFuture_[C](_ => true)

  @deprecated("Use when_")
  final def whenFuture_[C <: Classifier : ClassTag](predicate: ClassifierToEvent[C] => Boolean)
  : Future[ClassifierToEvent[C]] =
    when_(predicate)

  final def whenPFFuture[C <: Classifier: ClassTag, D](pf: PartialFunction[ClassifierToEvent[C], D])
  : Future[D] =
    whenFilterMapFuture(pf.lift)

  final def whenFilterMapFuture[C <: Classifier: ClassTag, D](f: ClassifierToEvent[C] => Option[D])
  : Future[D] =
    val promise = Promise[D]()
    oneShotFilterMap(f)(promise.success)
    promise.future

  final def oneShot[C <: Classifier: ClassTag](
    predicate: ClassifierToEvent[C] => Boolean = (_: ClassifierToEvent[C]) => true)
    (handle: ClassifierToEvent[C] => Unit)
  : Unit =
    val used = Atomic(false)
    lazy val subscription: EventSubscription =
      toSubscription { event_ =>
        val event = event_.asInstanceOf[ClassifierToEvent[C]]
        if predicate(event) && !used.getAndSet(true) then
          subscription.close()
          handle(event)
      }
    addSubscription(subscription)

  private final def oneShotPF[C <: Classifier : ClassTag, A](
    pf: PartialFunction[ClassifierToEvent[C], A])
    (handle: A => Unit)
  : Unit =
    oneShotFilterMap(pf.lift)(handle)

  final def oneShotFilterMap[C <: Classifier : ClassTag, A](
    f: ClassifierToEvent[C] => Option[A])
    (handle: A => Unit)
  : Unit =
    val used = Atomic(false)
    lazy val subscription: EventSubscription =
      toSubscription { event_ =>
        val event = event_.asInstanceOf[ClassifierToEvent[C]]
        for a <- f(event) do
          if !used.getAndSet(true) then
            subscription.close()
            handle(a)
      }
    addSubscription(subscription)

  final def subscribe[C <: Classifier: ClassTag](handle: ClassifierToEvent[C] => Unit): EventSubscription =
    subscribeToClass(implicitClass[C])(handle)

  final def subscribeToClass[C <: Classifier](eventClass: Class[C])(handle: ClassifierToEvent[C] => Unit): EventSubscription =
    val subscription = classToSubscription(eventClass)(handle)
    addSubscription(subscription)
    subscription

  final def toSubscription[C <: Classifier: ClassTag](handle: E => Unit): EventSubscription =
    classToSubscription(implicitClass[C])(handle)

  private def classToSubscription[C <: Classifier](eventClass: Class[C])(handle: ClassifierToEvent[C] => Unit): EventSubscription =
    new EventSubscription(Set(eventClass), handle.asInstanceOf[E => Unit])

  final class EventSubscription(
    private[ClassEventBus] val classes: Set[Cls],
    private[ClassEventBus] val call: E => Unit)
  extends AutoCloseable:
    //val assignableClasses: Set[Cls] = classes.asInstanceOf[Set[Cls]]
    //  .flatMap(c => superclassCache.assignableClasses(c).asInstanceOf[Set[Cls]])

    def close(): Unit =
      removeSubscription(this)


object ClassEventBus:
  private val logger = Logger[this.type]
