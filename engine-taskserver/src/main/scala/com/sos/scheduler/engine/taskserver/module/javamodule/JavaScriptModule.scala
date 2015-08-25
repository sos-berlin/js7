package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module._
import sos.spooler.jobs.{ScriptAdapterJob, ScriptAdapterMonitor}

/**
 * @author Andreas Liebert
 */
final case class JavaScriptModule(language: String, script: Script) extends JavaModule {

  def moduleLanguage = JavaModuleLanguage

  def newJobInstance() = new ScriptAdapterJob(language, script.string)

  def newMonitorInstance() = new ScriptAdapterMonitor(language, script.string)
}
