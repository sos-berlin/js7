package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import java.nio.file.Path
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class FileBasedDetailed(
  overview: FileBasedOverview,
  file: Option[Path],
  fileModifiedAt: Option[Instant],
  sourceXml: Option[String])
{
  def path = overview.path

  def asTyped[P <: TypedPath: TypedPath.Companion] = copy(overview = overview.asTyped[P])
}

object FileBasedDetailed {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat4(apply)
}
