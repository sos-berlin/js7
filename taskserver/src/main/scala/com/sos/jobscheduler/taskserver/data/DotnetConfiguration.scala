package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import java.nio.file.Path
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class DotnetConfiguration(
  adapterDllDirectory: Option[Path] = None,
  classDllDirectory: Option[Path] = None)

object DotnetConfiguration {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
