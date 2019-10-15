package com.sos.jobscheduler.master.cluster

sealed trait ClusterFollowUp

object ClusterFollowUp
{
  case object BecomeActive extends ClusterFollowUp

  case object Terminate extends ClusterFollowUp
}
