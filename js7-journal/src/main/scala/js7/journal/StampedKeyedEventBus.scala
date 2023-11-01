package js7.journal

import javax.inject.Singleton
import js7.data.event.{AnyKeyedEvent, Stamped, Event as EngineEvent}
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.event.{EventBus, SubchannelClassification}
import org.apache.pekko.util.Subclassification

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
