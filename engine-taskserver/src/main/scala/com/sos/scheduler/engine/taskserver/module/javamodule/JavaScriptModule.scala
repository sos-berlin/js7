package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module.ModuleArguments.JavaScriptModuleArguments
import sos.spooler.jobs.{ScriptAdapterJob, ScriptAdapterMonitor}

/**
 * @author Andreas Liebert
 */
final class JavaScriptModule(val arguments: JavaScriptModuleArguments) extends JavaModule {

  import arguments.{scriptLanguage, script}

  protected def newJobInstance() = new ScriptAdapterJob(scriptLanguage, script.string)

  protected def newMonitorInstance() = new ScriptAdapterMonitor(scriptLanguage, script.string)
}
