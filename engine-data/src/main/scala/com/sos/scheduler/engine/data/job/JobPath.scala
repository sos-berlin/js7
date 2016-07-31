package com.sos.scheduler.engine.data.job

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class JobPath(string: String)
extends TypedPath {

  validate()

  def companion = JobPath
}

object JobPath extends TypedPath.Companion[JobPath] {
  @JsonCreator def valueOf(absolutePath: String) = new JobPath(absolutePath)

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.job
}
