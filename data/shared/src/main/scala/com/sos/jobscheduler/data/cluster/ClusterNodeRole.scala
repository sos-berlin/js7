package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.data.common.Uri

sealed trait ClusterNodeRole
{
  def isPrimary: Boolean

  final def isBackup = !isPrimary
}

object ClusterNodeRole
{
  final case class Primary(backupUri: Option[Uri] = None)
  extends ClusterNodeRole
  {
    def isPrimary = true
  }

  final case class Backup(primaryUri: Uri)
  extends ClusterNodeRole
  {
    def isPrimary = false
  }
}
