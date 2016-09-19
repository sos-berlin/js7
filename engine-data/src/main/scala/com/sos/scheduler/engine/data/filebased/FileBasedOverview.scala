package com.sos.scheduler.engine.data.filebased

import spray.json.DefaultJsonProtocol._

final case class FileBasedOverview(
  path: TypedPath,
  fileBasedState: FileBasedState)
extends HasPath {

  def asTyped[P <: TypedPath: TypedPath.Companion] = copy(path = path.asTyped[P])
}

object FileBasedOverview {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat2(apply)
}
