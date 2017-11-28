package com.sos.jobscheduler.taskserver.data

import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.PathJsonCodec
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import io.circe.generic.JsonCodec
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class DotnetConfiguration(
  adapterDllDirectory: Option[Path] = None,
  classDllDirectory: Option[Path] = None)

object DotnetConfiguration {
  intelliJuseImport(PathJsonCodec)
}
