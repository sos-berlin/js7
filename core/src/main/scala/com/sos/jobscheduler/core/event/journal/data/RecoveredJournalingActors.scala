package com.sos.jobscheduler.core.event.journal.data

import akka.actor.ActorRef

/**
  * @author Joacim Zschimmer
  */
final case class RecoveredJournalingActors(keyToJournalingActor: Map[Any, ActorRef])

object RecoveredJournalingActors {
  final val Empty = RecoveredJournalingActors(Map.empty)
}
