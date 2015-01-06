package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import com.sos.scheduler.engine.data.base.{HasKey, HasIsDefault}
import org.joda.time.ReadableInstant
import scala.annotation.meta.getter

@ForCpp
final case class JobPersistentState(
    jobPath: JobPath,
    @(ForCpp @getter) isPermanentlyStopped: Boolean,
    nextStartTimeOption: Option[ReadableInstant])
extends HasKey[JobPath]
with HasIsDefault {

  def key =
    jobPath

  def isDefault =
    nextStartTimeOption.isEmpty && !isPermanentlyStopped
}
