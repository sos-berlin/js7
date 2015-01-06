package com.sos.scheduler.engine.data.job

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.data.base.IsString
import com.sos.scheduler.engine.data.filebased.{AbsolutePath, FileBasedType, TypedPath}

final case class JobPath(string: String)
extends TypedPath {

  requireIsAbsolute()

  def fileBasedType = FileBasedType.job
}


object JobPath {
  implicit val MyJsonFormat = new IsString.MyJsonFormat(apply)

  @JsonCreator def valueOf(absolutePath: String) = new JobPath(absolutePath)

  def makeAbsolute(path: String) =
    new JobPath(AbsolutePath.makeAbsolute(path))
}
