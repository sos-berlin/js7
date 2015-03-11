package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.data.base.IsString

/**
 * @author Joacim Zschimmer
 */
trait ModuleLanguage extends IsString

object ModuleLanguage {
  def apply(language: String): ModuleLanguage =
    language.toLowerCase match {
      case ShellModuleLanguage.string ⇒ ShellModuleLanguage
      case JavaModuleLanguage.string ⇒ JavaModuleLanguage
      case _ ⇒ OtherModuleLanguage(language)
    }
}

case object ShellModuleLanguage extends ModuleLanguage {
  val string = "shell"
}

case object JavaModuleLanguage extends ModuleLanguage {
  val string = "java"
}

//final case class JavaScriptModuleLanguage(javaName: String) extends ScriptLanguage {
//  def string = s"java:$javaName"
//}

final case class OtherModuleLanguage(string: String) extends ModuleLanguage
