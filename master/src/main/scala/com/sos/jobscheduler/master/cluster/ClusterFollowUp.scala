package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.core.event.state.Recovered
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.MasterState

sealed trait ClusterFollowUp

object ClusterFollowUp
{
  final case class BecomeActive(recovered: Recovered[MasterState, Event])
  extends ClusterFollowUp

  case object Terminate extends ClusterFollowUp
}
