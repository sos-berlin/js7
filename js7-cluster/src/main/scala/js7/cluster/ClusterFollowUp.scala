package js7.cluster

import js7.data.event.SnapshotableState
import js7.journal.recover.Recovered

sealed trait ClusterFollowUp[S <: SnapshotableState[S]]

object ClusterFollowUp
{
  final case class BecomeActive[S <: SnapshotableState[S]](
    recovered: Recovered[S])
  extends ClusterFollowUp[S]

  //final case class Terminate[S <: SnapshotableState[S, E], E <: Event]() extends ClusterFollowUp[S, E]
}
