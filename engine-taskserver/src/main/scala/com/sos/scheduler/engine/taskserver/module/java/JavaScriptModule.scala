package com.sos.scheduler.engine.taskserver.module.java

import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import com.sos.scheduler.engine.taskserver.module._
import sos.spooler.jobs.ScriptAdapterJob

/**
 * Created by Andreas Liebert on 30.04.2015.
 */
final case class JavaScriptModule(language: String, script: Script) extends Module {
  def moduleLanguage = JavaModuleLanguage

  def newMonitorInstance(namedInvocables: NamedInvocables): sos.spooler.Monitor_impl = {
    val adapter = new ScriptAdapterJob(language, script.string)
    adapter.spooler_log = new sos.spooler.Log(JavaInvoker(namedInvocables.spoolerLog))
    adapter.spooler_task = new sos.spooler.Task(JavaInvoker(namedInvocables.spoolerTask))
    adapter.spooler_job = new sos.spooler.Job(JavaInvoker(namedInvocables.spoolerJob))
    adapter.spooler = new sos.spooler.Spooler(JavaInvoker(namedInvocables.spooler))

    val monitorImpl = new sos.spooler.Monitor_impl(){
      override def spooler_task_before(): Boolean = adapter.spooler_task_before()
      override def spooler_task_after(): Unit = adapter.spooler_task_after()
      override def spooler_process_before(): Boolean = adapter.spooler_process_before()
      override def spooler_process_after(spooler_process_result: Boolean): Boolean = adapter.spooler_process_after(spooler_process_result)
    }
    monitorImpl
  }
}
