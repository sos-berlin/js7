package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.data.cluster.ClusterState
import io.circe.Json

trait ClusterStateBuilder
{
  def put(json: Json): Unit

  def isAcceptingEvents: Boolean

  def clusterState: ClusterState
}
