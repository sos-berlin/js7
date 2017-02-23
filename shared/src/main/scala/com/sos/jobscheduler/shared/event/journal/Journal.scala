package com.sos.scheduler.engine.shared.event.journal

import akka.actor.ActorRef
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Snapshot}
import com.sos.scheduler.engine.shared.event.journal.JsonJournalRecoverer.RecoveredJournalingActors
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object Journal {

  object Input {
    final case class Start(recoveredJournalingActors: RecoveredJournalingActors)
    final case class RegisterMe(key: Option[Any])
    final case class Store(eventSnapshots: Seq[AnyKeyedEvent], journalingActor: ActorRef)
    final case object TakeSnapshot
  }

  trait Output
  object Output {
    final case object Ready
    final case class Stored(snapshots: Seq[Snapshot[AnyKeyedEvent]]) extends Output
    final case class SerializationFailure(throwable: Throwable) extends Output
    final case class StoreFailure(throwable: Throwable) extends Output
    final case object SnapshotTaken
  }
}
