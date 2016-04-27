package com.sos.scheduler.engine.taskserver.module

import java.nio.file.Path

/**
  * Arguments for a [[Module]] as received from the C++ engine.
  *
  * @author Joacim Zschimmer
  */
final case class RawModuleArguments(
  language: ModuleLanguage,
  javaClassNameOption: Option[String] = None,
  script: Script = Script.Empty,
  dllOption: Option[Path],
  dotnetClassNameOption: Option[String]) {
}
