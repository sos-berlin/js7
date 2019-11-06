package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.data.common.Uri

sealed trait ClusterNodeRole

object ClusterNodeRole
{
  final case class Primary(
    backupNodeId: Option[ClusterNodeId] = None,
    backupUri: Option[Uri] = None)
  extends ClusterNodeRole

  final case class Backup(primaryUri: Uri)
  extends ClusterNodeRole
}
