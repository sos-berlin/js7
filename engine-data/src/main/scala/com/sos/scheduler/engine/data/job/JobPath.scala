package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class JobPath(string: String)
extends TypedPath {

  validate()

  def companion = JobPath
}

object JobPath extends TypedPath.Companion[JobPath] {

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.Job
}
