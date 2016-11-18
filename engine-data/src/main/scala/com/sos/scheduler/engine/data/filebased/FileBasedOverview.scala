package com.sos.scheduler.engine.data.filebased

import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

final case class FileBasedOverview(
  path: TypedPath,
  fileBasedState: FileBasedState)
extends FileBasedView
with HasPath

object FileBasedOverview extends FileBasedView.Companion[FileBasedOverview] {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val jsonFormat: RootJsonFormat[FileBasedOverview] = jsonFormat2(apply)
}
