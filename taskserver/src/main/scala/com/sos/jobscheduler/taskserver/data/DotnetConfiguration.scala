package com.sos.jobscheduler.taskserver.data

import com.sos.jobscheduler.base.sprayjson.SprayJson.JsonFormats._
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
