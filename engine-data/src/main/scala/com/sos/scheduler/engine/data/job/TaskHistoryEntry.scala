package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.scheduler.{SchedulerId, ClusterMemberId}
import java.time.Instant

trait TaskHistoryEntry {
  val id: Int
  val schedulerId: SchedulerId
  val clusterMemberId: ClusterMemberId
  val jobPath: JobPath
  val startTime: Instant
  val endTimeOption: Option[Instant]
  val cause: String
  val stepsOption: Option[Int]
  val errorCode: String
  val errorText: String
  val parameterXml: String
  val processIdOption: Option[Int]
}
