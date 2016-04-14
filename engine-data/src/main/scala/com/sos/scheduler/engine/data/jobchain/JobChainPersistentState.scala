package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.generic.HasIsDefault
import com.sos.scheduler.engine.base.utils.HasKey

final case class JobChainPersistentState(jobChainPath: JobChainPath, isStopped: Boolean)
extends HasKey with HasIsDefault {

  type Key = JobChainPath

  def key = jobChainPath

  def isDefault = !isStopped
}
