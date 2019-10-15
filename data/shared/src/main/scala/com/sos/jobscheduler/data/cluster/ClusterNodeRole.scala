package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.data.common.Uri

sealed trait ClusterNodeRole

object ClusterNodeRole
{
  case object Primary extends ClusterNodeRole
  final case class Backup(initialActiveUri: Uri) extends ClusterNodeRole
}
