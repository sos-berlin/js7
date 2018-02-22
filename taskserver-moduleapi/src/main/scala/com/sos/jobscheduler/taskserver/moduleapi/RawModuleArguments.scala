package com.sos.jobscheduler.taskserver.moduleapi

import com.sos.jobscheduler.base.utils.Strings.RichString
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

  override def toString = (
      Some(s"language=$language") ++
      (javaClassNameOption map { o ⇒ s"java_class=$o" }) ++
      (dllOption map { o ⇒ s"dll=$o" }) ++
      (dotnetClassNameOption map { o ⇒ s"dotnet_class=$o" }) ++
      Some(s"script=${script.string truncateWithEllipsis 50}")
    ).mkString("RawModuleArguments(", " ", ")")
}
