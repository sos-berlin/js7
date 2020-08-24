package js7.proxy.javaapi.eventbus

import js7.base.annotation.javaApi
import js7.base.eventbus.StandardEventBus
import js7.base.utils.Assertions.assertThat
import js7.proxy.javaapi.data.common.JavaWrapper
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

@javaApi
final class JStandardEventBus[E](val asScala: StandardEventBus[E])
extends JavaWrapper with AutoCloseable
{
  protected type AsScala = StandardEventBus[E]

  def this()(implicit e: ClassTag[E]) =
    this(new StandardEventBus[E])

  @javaApi
  def this(eventClass: Class[E]) =
    this(new StandardEventBus(eventClass))

  /** Close all subscriptions. */
  def close() = asScala.close()

  @javaApi
  def subscribe[E1 <: E](
    eventClasses: java.lang.Iterable[Class[_ <: E1]],
    callback: java.util.function.Consumer[E1])
  : EventSubscription = {
    val subscription = newSubscription(eventClasses, callback)
    addSubscription(subscription)
    subscription
  }

  @javaApi
  def newSubscription[E1 <: E](
    eventClasses: java.lang.Iterable[Class[_ <: E1]],
    callback: java.util.function.Consumer[E1])
  : EventSubscription =
    EventSubscription(new asScala.EventSubscription(
      eventClasses.asScala.toSet,
      e => callback.accept(e.asInstanceOf[E1])))

  @javaApi
  def addSubscription(subscription: EventSubscription): Unit = {
    assertThat(subscription.eventBus eq asScala)
    subscription.internalAddToEventBus()
  }

  @javaApi
  def removeAllSubscriptions(): Unit =
    asScala.removeAllSubscriptions()

  sealed/*instead of final in Scala 2: https://github.com/scala/bug/issues/4440*/
  case class EventSubscription private(asScala: JStandardEventBus.this.asScala.EventSubscription)
  extends js7.proxy.javaapi.eventbus.EventSubscription
  with JavaWrapper
  with AutoCloseable
  {
    protected type AsScala = JStandardEventBus.this.asScala.EventSubscription

    /** For internal use only. */
    private[JStandardEventBus] def eventBus = JStandardEventBus.this.asScala

    def close() = asScala.close()

    /** For internal use only. */
    private[JStandardEventBus] def internalAddToEventBus(): Unit =
      JStandardEventBus.this.asScala.addSubscription(asScala)
  }
}
