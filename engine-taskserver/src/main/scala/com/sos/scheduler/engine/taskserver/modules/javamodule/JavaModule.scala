package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.module.{ModuleArguments, NamedInvocables}
import com.sos.scheduler.engine.taskserver.modules.javamodule.JavaModule._

/**
  * @author Joacim Zschimmer
  */
trait JavaModule extends ApiModule {

  protected def newJobInstance(): sos.spooler.Job_impl
  protected def newMonitorInstance(): sos.spooler.Monitor_impl

  final def newJobInstance(namedInvocables: NamedInvocables) = {
    val r = newJobInstance()
    r.spooler_log = spooler_log(namedInvocables)
    r.spooler_task = spooler_task(namedInvocables)
    r.spooler_job = spooler_job(namedInvocables)
    r.spooler = spooler(namedInvocables)
    r
  }

  final def newMonitorInstance(namedInvocables: NamedInvocables) = {
    val r = newMonitorInstance()
    r.spooler_log = spooler_log(namedInvocables)
    r.spooler_task = spooler_task(namedInvocables)
    r.spooler_job = spooler_job(namedInvocables)
    r.spooler = spooler(namedInvocables)
    r
  }
}

object JavaModule {
  def spooler_log(o: NamedInvocables) = new sos.spooler.Log(JavaInvoker(o.spoolerLog))
  def spooler_task(o: NamedInvocables) = new sos.spooler.Task(JavaInvoker(o.spoolerTask))
  def spooler_job(o: NamedInvocables) = new sos.spooler.Job(JavaInvoker(o.spoolerJob))
  def spooler(o: NamedInvocables) = new sos.spooler.Spooler(JavaInvoker(o.spooler))

  trait Arguments extends ModuleArguments
}
