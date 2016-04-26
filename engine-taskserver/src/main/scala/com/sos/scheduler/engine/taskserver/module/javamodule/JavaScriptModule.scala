package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module.{JavaScriptModuleLanguage, ModuleArguments, ModuleType, RawModuleArguments, Script}
import sos.spooler.jobs.{ScriptAdapterJob, ScriptAdapterMonitor}

/**
 * @author Andreas Liebert
 */
final case class JavaScriptModule(val arguments: JavaScriptModule.Arguments) extends JavaModule {

  import arguments.{script, scriptLanguage}

  protected def newJobInstance() = new ScriptAdapterJob(scriptLanguage, script.string)

  protected def newMonitorInstance() = new ScriptAdapterMonitor(scriptLanguage, script.string)
}

object JavaScriptModule extends ModuleType {
  def toModuleArguments = {
    case args @ RawModuleArguments(JavaScriptModuleLanguage(scriptLanguage), javaClassNameOption, script) ⇒
      args.requireUnused("java_class", javaClassNameOption)
      Arguments(scriptLanguage, script)
  }

  def newModule(arguments: ModuleArguments) = new JavaScriptModule(arguments.asInstanceOf[Arguments])

  final case class Arguments(scriptLanguage: String, script: Script) extends ModuleArguments {
    val moduleType = JavaScriptModule
  }
}
