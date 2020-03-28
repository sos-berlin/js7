package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.generic.GenericString

final case class ClusterNodeId private(string: String)
extends GenericString

object ClusterNodeId extends GenericString.NonEmpty[ClusterNodeId]
{
  protected def unchecked(string: String) = new ClusterNodeId(string)
}
