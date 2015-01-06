package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.scheduler.{ClusterMemberId, SchedulerId}
import org.joda.time.ReadableInstant

trait SchedulerHistoryEntry {
  val id: Int
  val schedulerId: SchedulerId
  val clusterMemberId: ClusterMemberId
  val startTime: ReadableInstant
  val endTimeOption: Option[ReadableInstant]
  val errorCode: String
  val errorText: String
  val parameterXml: String
  val processId: Int
}
