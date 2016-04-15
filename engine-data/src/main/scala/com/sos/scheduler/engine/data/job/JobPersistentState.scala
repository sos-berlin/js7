package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.generic.HasIsDefault
import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import java.time.Instant
import scala.annotation.meta.getter

@ForCpp
final case class JobPersistentState(
    jobPath: JobPath,
    @(ForCpp @getter) isPermanentlyStopped: Boolean,
    nextStartTimeOption: Option[Instant])
extends HasKey
with HasIsDefault {

  type Key = JobPath

  def key = jobPath

  def isDefault = nextStartTimeOption.isEmpty && !isPermanentlyStopped
}
