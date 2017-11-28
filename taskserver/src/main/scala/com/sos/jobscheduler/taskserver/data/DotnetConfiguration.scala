package com.sos.jobscheduler.taskserver.data

import io.circe.generic.JsonCodec
import java.nio.file.Path
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.PathJsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class DotnetConfiguration(
  adapterDllDirectory: Option[Path] = None,
  classDllDirectory: Option[Path] = None)

object DotnetConfiguration {
  PathJsonCodec  // For IntelliJ import
}
