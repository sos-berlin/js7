package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import org.joda.time.ReadableInstant
import scala.annotation.meta.getter

@ForCpp
final case class TaskPersistentState(
                      taskId: TaskId,
                      jobPath: JobPath,
    @(ForCpp @getter) enqueueTime: ReadableInstant,
                      startTimeOption: Option[ReadableInstant],
    @(ForCpp @getter) parametersXml: String,
    @(ForCpp @getter) xml: String)
extends HasKey {

  type Key = TaskId

  def key = taskId

  @ForCpp
  def taskIdNumber: Int = taskId.number

  @ForCpp
  def startTimeMillis: Long = startTimeOption map { _.getMillis } getOrElse 0

  @ForCpp
  def enqueueTimeMillis: Long = enqueueTime.getMillis
}
