package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.scheduler.{SchedulerId, ClusterMemberId}
import java.time.Instant

final case class SimpleTaskHistoryEntry(
  id: Int,
  schedulerId: SchedulerId,
  clusterMemberId: ClusterMemberId,
  jobPath: JobPath,
  startTime: Instant,
  endTimeOption: Option[Instant],
  cause: String,
  stepsOption: Option[Int],
  errorCode: String,
  errorText: String,
  parameterXml: String,
  processIdOption: Option[Int])
extends TaskHistoryEntry
