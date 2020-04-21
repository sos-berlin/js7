package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.data.event.JournaledState

sealed trait ClusterFollowUp[S <: JournaledState[S]]

object ClusterFollowUp
{
  final case class BecomeActive[S <: JournaledState[S]](
    recovered: Recovered[S])
  extends ClusterFollowUp[S]

  //final case class Terminate[S <: JournaledState[S, E], E <: Event]() extends ClusterFollowUp[S, E]
}
