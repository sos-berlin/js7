package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module.{JavaScriptModuleLanguage, ModuleArguments, ModuleFactory, RawModuleArguments, Script}
import sos.spooler.jobs.{ScriptAdapterJob, ScriptAdapterMonitor}

/**
 * @author Joacim Zschimmer
 */
final case class JavaScriptModule(val arguments: JavaScriptModule.Arguments) extends JavaModule {

  import arguments.{script, scriptLanguage}

  protected def newJobInstance() = new ScriptAdapterJob(scriptLanguage, script.string)

  protected def newMonitorInstance() = new ScriptAdapterMonitor(scriptLanguage, script.string)
}

object JavaScriptModule extends ModuleFactory {
  def toModuleArguments = {
    case args @ RawModuleArguments(JavaScriptModuleLanguage(scriptLanguage), javaClassNameOption, script, None, None) ⇒
      args.requireUnused("java_class", javaClassNameOption)
      Arguments(scriptLanguage, script)
  }

  def newModule(arguments: ModuleArguments) = new JavaScriptModule(arguments.asInstanceOf[Arguments])

  final case class Arguments(scriptLanguage: String, script: Script) extends ModuleArguments {
    val moduleFactory = JavaScriptModule
  }
}
