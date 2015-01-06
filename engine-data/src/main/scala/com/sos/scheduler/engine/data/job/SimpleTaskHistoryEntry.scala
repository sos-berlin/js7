package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.scheduler.{SchedulerId, ClusterMemberId}
import org.joda.time.DateTime

final case class SimpleTaskHistoryEntry(
  id: Int,
  schedulerId: SchedulerId,
  clusterMemberId: ClusterMemberId,
  jobPath: JobPath,
  startTime: DateTime,
  endTimeOption: Option[DateTime],
  cause: String,
  stepsOption: Option[Int],
  errorCode: String,
  errorText: String,
  parameterXml: String,
  processIdOption: Option[Int])
extends TaskHistoryEntry