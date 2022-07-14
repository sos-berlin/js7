package js7.proxy.javaapi.eventbus

import java.util.Objects.requireNonNull
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.utils.Assertions.assertThat
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.data_for_java.common.{JJournaledState, JavaWrapper}
import js7.proxy.JournaledStateEventBus
import scala.jdk.CollectionConverters._

class JJournaledStateEventBus[JS <: JJournaledState[JS, S], S <: JournaledState[S]]
  (val asScala: JournaledStateEventBus[S])
  (implicit JS: JJournaledState.Companion[JS, S])
extends AutoCloseable
{
  def this()(implicit S: JJournaledState.Companion[JS, S]) =
    this(new JournaledStateEventBus[S])

  /** Close all subscriptions. */
  def close() = asScala.close()

  @javaApi @Nonnull
  final def subscribe[E <: Event](
    @Nonnull eventClasses: java.lang.Iterable[Class[_ <: Event]],
    @Nonnull callback: java.util.function.BiConsumer[Stamped[KeyedEvent[E]], JS])
  : EventSubscription = {
    val subscription = newSubscription(eventClasses, callback)
    addSubscription(subscription)
    subscription
  }

  @javaApi @Nonnull
  final def newSubscription[E <: Event](
    @Nonnull eventClasses: java.lang.Iterable[Class[_ <: Event]],
    @Nonnull callback: java.util.function.BiConsumer[Stamped[KeyedEvent[E]], JS])
  : EventSubscription = {
    requireNonNull(callback)
    EventSubscription(
      new asScala.EventSubscription(
        eventClasses.asScala.toSet,
        o => callback.accept(
          o.stampedEvent.asInstanceOf[Stamped[KeyedEvent[E]]],
          JS(o.state))))
  }

  @javaApi
  final def addSubscription[E <: Event](@Nonnull subscription: EventSubscription): Unit = {
    assertThat(subscription.eventBus eq asScala)
    subscription.internalAddToEventBus()
  }

  //@javaApi
  //final def removeSubscription[E <: Event](subscription: EventSubscription[E]): Unit = {
  //  assertThat(subscription.eventBus eq asScala)
  //  asScala.removeSubscription(subscription.asScala)
  //}

  @javaApi
  final def removeAllSubscriptions(): Unit =
    asScala.removeAllSubscriptions()

  @javaApi
  sealed/*instead of final in Scala 2: https://github.com/scala/bug/issues/4440*/
  case class EventSubscription(asScala: JJournaledStateEventBus.this.asScala.EventSubscription)
  extends js7.proxy.javaapi.eventbus.EventSubscription
  with JavaWrapper
  with AutoCloseable
  {
    protected type AsScala = JJournaledStateEventBus.this.asScala.EventSubscription

    /** For internal use only. */
    private[JJournaledStateEventBus] def eventBus = JJournaledStateEventBus.this.asScala

    @javaApi
    def close() = asScala.close()

    /** For internal use only. */
    private[JJournaledStateEventBus] def internalAddToEventBus(): Unit =
      JJournaledStateEventBus.this.asScala.addSubscription(asScala)
  }
}
