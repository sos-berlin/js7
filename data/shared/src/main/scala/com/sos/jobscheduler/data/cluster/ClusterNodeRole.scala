package com.sos.jobscheduler.data.cluster

sealed trait ClusterNodeRole

object ClusterNodeRole
{
  case object Primary
  extends ClusterNodeRole

  case object Backup
  extends ClusterNodeRole
}
