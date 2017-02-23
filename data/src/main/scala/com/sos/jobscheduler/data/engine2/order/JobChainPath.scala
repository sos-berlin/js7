package com.sos.scheduler.engine.data.engine2.order

import com.sos.scheduler.engine.data.filebased.TypedPath

final case class JobChainPath(string: String)
extends TypedPath {

  validate()

  def companion = JobChainPath
}


object JobChainPath extends TypedPath.Companion[JobChainPath] {

  override protected[engine] def isCommaAllowed = false  // Legacy of JobScheduler 1
}
