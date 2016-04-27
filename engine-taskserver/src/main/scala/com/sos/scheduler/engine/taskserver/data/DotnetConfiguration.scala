package com.sos.scheduler.engine.taskserver.data

import java.nio.file.Path
import spray.json.DefaultJsonProtocol._
import com.sos.scheduler.engine.common.sprayutils.SprayJson.implicits._

/**
  * @author Joacim Zschimmer
  */
final case class DotnetConfiguration(
  adapterDllDirectory: Option[Path] = None,
  classDllDirectory: Option[Path] = None)

object DotnetConfiguration {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
