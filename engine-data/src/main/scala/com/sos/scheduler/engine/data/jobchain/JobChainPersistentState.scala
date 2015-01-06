package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.base.{HasKey, HasIsDefault}

final case class JobChainPersistentState(jobChainPath: JobChainPath, isStopped: Boolean)
extends HasKey[JobChainPath] with HasIsDefault {

  def key = jobChainPath

  def isDefault = !isStopped
}
