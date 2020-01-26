package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.data.event.{Event, JournalPosition, JournaledState}

sealed trait ClusterFollowUp[S <: JournaledState[S, E], E <: Event]

object ClusterFollowUp
{
  final case class BecomeActive[S <: JournaledState[S, E], E <: Event](
    recovered: Recovered[S, E],
    failedAt: Option[JournalPosition])
  extends ClusterFollowUp[S, E]

  final case class Terminate[S <: JournaledState[S, E], E <: Event]() extends ClusterFollowUp[S, E]
}
