package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.data.filebased.TypedPath

final case class JobChainPath(string: String)
extends TypedPath {

  validate()

  def companion = JobChainPath
}


object JobChainPath extends TypedPath.Companion[JobChainPath] {

  override def isCommaAllowed = false  // Legacy of JobScheduler 1
}
