package com.sos.scheduler.engine.data.engine2.order

import com.sos.scheduler.engine.data.filebased.TypedPath

final case class JobPath(string: String)
extends TypedPath {

  validate()

  def companion = JobPath
}

object JobPath extends TypedPath.Companion[JobPath]
