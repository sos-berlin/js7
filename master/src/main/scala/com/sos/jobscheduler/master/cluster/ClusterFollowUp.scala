package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.data.event.{Event, JournaledState}

sealed trait ClusterFollowUp

object ClusterFollowUp
{
  final case class BecomeActive[S <: JournaledState[S, E], E <: Event](recovered: Recovered[S, E])
  extends ClusterFollowUp

  case object Terminate extends ClusterFollowUp
}
