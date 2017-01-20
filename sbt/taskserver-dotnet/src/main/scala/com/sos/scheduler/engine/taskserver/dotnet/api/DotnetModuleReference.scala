package com.sos.scheduler.engine.taskserver.dotnet.api

import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
sealed trait DotnetModuleReference

object DotnetModuleReference {
  final case class DotnetClass(dll: Path, className: String)
  extends DotnetModuleReference

  final case class Powershell(script: String)
  extends DotnetModuleReference

  final case class ScriptControl(language: String, script: String)
  extends DotnetModuleReference
}
