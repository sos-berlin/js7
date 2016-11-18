package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import java.nio.file.Path
import java.time.Instant
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
 * @author Joacim Zschimmer
 */
final case class FileBasedDetailed(
  overview: FileBasedOverview,
  file: Option[Path],
  fileModifiedAt: Option[Instant],
  sourceXml: Option[String])
extends FileBasedView
{
  def path = overview.path
}

object FileBasedDetailed extends FileBasedView.Companion[FileBasedDetailed] {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val jsonFormat: RootJsonFormat[FileBasedDetailed] = jsonFormat4(apply)
}
