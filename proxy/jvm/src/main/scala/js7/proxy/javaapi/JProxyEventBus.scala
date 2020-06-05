package js7.proxy.javaapi

import js7.base.annotation.javaApi
import js7.base.utils.Assertions.assertThat
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.master.data.MasterState
import js7.proxy.ProxyEventBus
import js7.proxy.javaapi.data.{JMasterState, JavaWrapper}
import scala.jdk.CollectionConverters._

@javaApi
final class JProxyEventBus(val underlying: ProxyEventBus[MasterState])
{
  def this() = this(new ProxyEventBus[MasterState])

  def subscribe[E <: Event](
    eventClasses: java.lang.Iterable[Class[_ <: Event]],
    callback: java.util.function.BiConsumer[Stamped[KeyedEvent[E]], JMasterState])
  : AutoCloseable = {
    val subscription = newSubscription(eventClasses, callback)
    addSubscription(subscription)
    subscription
  }

  def newSubscription[E <: Event](
    eventClasses: java.lang.Iterable[Class[_ <: Event]],
    callback: java.util.function.BiConsumer[Stamped[KeyedEvent[E]], JMasterState])
  : EventSubscription[E] =
    EventSubscription[E](
      new underlying.EventSubscription(
        eventClasses.asScala.toSet,
        o => callback.accept(
          o.stampedEvent.asInstanceOf[Stamped[KeyedEvent[E]]],
          new JMasterState(o.state))))

  def addSubscription[E <: Event](subscription: EventSubscription[E]): Unit = {
    assertThat(subscription.eventBus eq underlying)
    subscription.internalAddToEventBus()
  }

  def removeSubscription[E <: Event](subscription: EventSubscription[E]): Unit = {
    assertThat(subscription.eventBus eq underlying)
    underlying.removeSubscription(subscription.underlying)
  }

  def removeAllSubscriptions(): Unit =
    underlying.removeAllSubscriptions()

  sealed/*instead of final in Scala 2: https://github.com/scala/bug/issues/4440*/
  case class EventSubscription[E <: Event] private(underlying: JProxyEventBus.this.underlying.EventSubscription)
  extends JavaWrapper
  with AutoCloseable
  {
    protected type Underlying = JProxyEventBus.this.underlying.EventSubscription

    /** For nternal use only. */
    private[JProxyEventBus] def eventBus = JProxyEventBus.this.underlying

    def close() = underlying.close()

    /** For internal use only. */
    private[JProxyEventBus] def internalAddToEventBus(): Unit =
      JProxyEventBus.this.underlying.addSubscription(underlying)
  }
}
