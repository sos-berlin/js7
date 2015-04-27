package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.data.base.IsString

/**
 * @author Joacim Zschimmer
 */
sealed trait ModuleLanguage extends IsString

object ModuleLanguage {
  private val ScriptPrefixes = Set("java:", "javax.script:")

  def apply(language: String): ModuleLanguage =
    language.toLowerCase match {
      case ShellModuleLanguage.string ⇒ ShellModuleLanguage
      case JavaModuleLanguage.string ⇒ JavaModuleLanguage
      case _ if ScriptPrefixes exists language.startsWith ⇒ new JavaScriptModuleLanguage(language)
      case _ ⇒ OtherModuleLanguage(language)
    }
}

case object ShellModuleLanguage extends ModuleLanguage {
  val string = "shell"
}

case object JavaModuleLanguage extends ModuleLanguage {
  val string = "java"
}

final case class JavaScriptModuleLanguage(languageName: String) extends ModuleLanguage {
  def string = languageName
}

final case class OtherModuleLanguage(string: String) extends ModuleLanguage
