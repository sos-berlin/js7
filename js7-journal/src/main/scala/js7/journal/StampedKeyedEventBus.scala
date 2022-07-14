package js7.journal

import akka.actor.ActorRef
import akka.event.{EventBus, SubchannelClassification}
import akka.util.Subclassification
import javax.inject.Singleton
import js7.data.event.{AnyKeyedEvent, Stamped, Event as EngineEvent}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class StampedKeyedEventBus extends EventBus with SubchannelClassification {

  type Event = Stamped[AnyKeyedEvent]
  type Classifier = Class[? <: EngineEvent]
  type Subscriber = ActorRef

  protected val subclassification = new Subclassification[Classifier] {
    def isEqual(x: Classifier, y: Classifier) =
      x == y

    def isSubclass(x: Classifier, y: Classifier) =
      y isAssignableFrom x
  }

  protected def classify(stamped: Stamped[AnyKeyedEvent]) =
    stamped.value.event.getClass

  protected def publish(eventStamped: Stamped[AnyKeyedEvent], subscriber: ActorRef) =
    subscriber ! eventStamped
}
