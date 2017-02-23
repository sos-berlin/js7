package com.sos.jobscheduler.shared.event

import akka.actor.ActorRef
import akka.event.{EventBus, SubchannelClassification}
import akka.util.Subclassification
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Snapshot, Event ⇒ EngineEvent}
import javax.inject.Singleton

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class SnapshotKeyedEventBus extends EventBus with SubchannelClassification {

  type Event = Snapshot[AnyKeyedEvent]
  type Classifier = Class[_ <: EngineEvent]
  type Subscriber = ActorRef

  protected val subclassification = new Subclassification[Classifier] {
    def isEqual(x: Classifier, y: Classifier) =
      x == y

    def isSubclass(x: Classifier, y: Classifier) =
      y isAssignableFrom x
  }

  protected def classify(snapshot: Snapshot[AnyKeyedEvent]) =
    snapshot.value.event.getClass

  protected def publish(eventSnapshot: Snapshot[AnyKeyedEvent], subscriber: ActorRef) =
    subscriber ! eventSnapshot
}
