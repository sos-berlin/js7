package js7.proxy.javaapi

import js7.base.annotation.javaApi
import js7.base.utils.Assertions.assertThat
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.proxy.JournaledStateEventBus
import js7.proxy.javaapi.data.{JJournaledState, JavaWrapper}
import scala.jdk.CollectionConverters._

class JJournaledStateEventBus[JS <: JJournaledState[JS, S], S <: JournaledState[S]](val underlying: JournaledStateEventBus[S])
  (implicit JS: JJournaledState.Companion[JS, S])
{
  def this()(implicit S: JJournaledState.Companion[JS, S]) =
    this(new JournaledStateEventBus[S])

  @javaApi
  final def subscribe[E <: Event](
    eventClasses: java.lang.Iterable[Class[_ <: Event]],
    callback: java.util.function.BiConsumer[Stamped[KeyedEvent[E]], JS])
  : AutoCloseable = {
    val subscription = newSubscription(eventClasses, callback)
    addSubscription(subscription)
    subscription
  }

  @javaApi
  final def newSubscription[E <: Event](
    eventClasses: java.lang.Iterable[Class[_ <: Event]],
    callback: java.util.function.BiConsumer[Stamped[KeyedEvent[E]], JS])
  : EventSubscription[E] =
    EventSubscription[E](
      new underlying.EventSubscription(
        eventClasses.asScala.toSet,
        o => callback.accept(
          o.stampedEvent.asInstanceOf[Stamped[KeyedEvent[E]]],
          JS(o.state))))

  @javaApi
  final def addSubscription[E <: Event](subscription: EventSubscription[E]): Unit = {
    assertThat(subscription.eventBus eq underlying)
    subscription.internalAddToEventBus()
  }

  @javaApi
  final def removeSubscription[E <: Event](subscription: EventSubscription[E]): Unit = {
    assertThat(subscription.eventBus eq underlying)
    underlying.removeSubscription(subscription.underlying)
  }

  @javaApi
  final def removeAllSubscriptions(): Unit =
    underlying.removeAllSubscriptions()

  @javaApi
  sealed/*instead of final in Scala 2: https://github.com/scala/bug/issues/4440*/
  case class EventSubscription[E <: Event] private(underlying: JJournaledStateEventBus.this.underlying.EventSubscription)
  extends JavaWrapper
  with AutoCloseable
  {
    type Underlying = JJournaledStateEventBus.this.underlying.EventSubscription

    /** For internal use only. */
    private[JJournaledStateEventBus] def eventBus = JJournaledStateEventBus.this.underlying

    @javaApi
    def close() = underlying.close()

    /** For internal use only. */
    private[JJournaledStateEventBus] def internalAddToEventBus(): Unit =
      JJournaledStateEventBus.this.underlying.addSubscription(underlying)
  }
}
