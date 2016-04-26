package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversable
import com.sos.scheduler.engine.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
sealed trait ModuleLanguage extends IsString

object ModuleLanguage {
  private val JavaScriptingEnginePrefixes = Set("java:", "javax.script:")
  private val SimplyNamedLanguages = List(
    ShellModuleLanguage,
    JavaModuleLanguage)

  private val SimpleNameToModuleLanguage: Map[String, ModuleLanguage] = SimplyNamedLanguages toKeyedMap { _.string.toLowerCase }

  def apply(language: String): ModuleLanguage = {
    val normalized = language.toLowerCase
    SimpleNameToModuleLanguage.getOrElse(normalized, complexlyNamedToModuleLanguage(normalized))
  }

  private def complexlyNamedToModuleLanguage(language: String) =
    language match {
      case _ if JavaScriptingEnginePrefixes exists language.startsWith ⇒ new JavaScriptModuleLanguage(language)
      case _ ⇒ throw new IllegalArgumentException(s"Unknown language='$language'")
    }
}

case object ShellModuleLanguage extends ModuleLanguage {
  val string = "shell"
}

case object JavaModuleLanguage extends ModuleLanguage {
  val string = "java"
}

final case class JavaScriptModuleLanguage(languageName: String) extends ModuleLanguage {
  val string = languageName
}
