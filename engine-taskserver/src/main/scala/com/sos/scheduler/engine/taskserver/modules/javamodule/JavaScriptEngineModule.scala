package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleArguments, ModuleFactory, ModuleLanguage, RawModuleArguments, Script}
import sos.spooler.jobs.{ScriptAdapterJob, ScriptAdapterMonitor}

/**
 * @author Joacim Zschimmer
 */
final class JavaScriptEngineModule(arguments: JavaScriptEngineModule.Arguments) extends JavaModule {

  import arguments.{script, scriptLanguage}

  protected def newJobInstance() = new ScriptAdapterJob(scriptLanguage, script.string)

  protected def newMonitorInstance() = new ScriptAdapterMonitor(scriptLanguage, script.string)
}

object JavaScriptEngineModule extends ModuleFactory {
  private val Prefixes = Set("java:", "javax.script:")

  private object JavaScriptEngineLanguage {
    def unapply(lang: ModuleLanguage) = if (Prefixes exists lang.string.startsWith) Some(lang.string) else None
  }

  def toModuleArguments = {
    case RawModuleArguments(JavaScriptEngineLanguage(language), None, script, None, None) ⇒ Arguments(language, script)
  }

  def newModule(arguments: ModuleArguments) = new JavaScriptEngineModule(arguments.asInstanceOf[Arguments])

  final case class Arguments(scriptLanguage: String, script: Script) extends ModuleArguments {
    val moduleFactory = JavaScriptEngineModule
  }
}
