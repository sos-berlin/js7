package com.sos.scheduler.engine.data.job

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class JobPath(string: String)
extends TypedPath {

  requireIsAbsolute()

  def fileBasedType = FileBasedType.job
}

object JobPath extends TypedPath.Companion[JobPath] {
  @JsonCreator def valueOf(absolutePath: String) = new JobPath(absolutePath)
}
