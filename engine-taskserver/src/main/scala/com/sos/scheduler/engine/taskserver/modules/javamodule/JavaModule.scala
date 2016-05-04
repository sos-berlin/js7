package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleArguments, NamedIDispatches}
import com.sos.scheduler.engine.taskserver.modules.javamodule.JavaModule._

/**
  * @author Joacim Zschimmer
  */
trait JavaModule extends ApiModule {

  protected def newJobInstance(): sos.spooler.Job_impl
  protected def newMonitorInstance(): sos.spooler.Monitor_impl

  final def newJobInstance(namedIDispatches: NamedIDispatches) = {
    val r = newJobInstance()
    r.spooler_log = spooler_log(namedIDispatches)
    r.spooler_task = spooler_task(namedIDispatches)
    r.spooler_job = spooler_job(namedIDispatches)
    r.spooler = spooler(namedIDispatches)
    r
  }

  final def newMonitorInstance(namedIDispatches: NamedIDispatches) = {
    val r = newMonitorInstance()
    r.spooler_log = spooler_log(namedIDispatches)
    r.spooler_task = spooler_task(namedIDispatches)
    r.spooler_job = spooler_job(namedIDispatches)
    r.spooler = spooler(namedIDispatches)
    r
  }
}

object JavaModule {
  def spooler_log(o: NamedIDispatches) = new sos.spooler.Log(JavaInvoker(o.spoolerLog))
  def spooler_task(o: NamedIDispatches) = new sos.spooler.Task(JavaInvoker(o.spoolerTask))
  def spooler_job(o: NamedIDispatches) = new sos.spooler.Job(JavaInvoker(o.spoolerJob))
  def spooler(o: NamedIDispatches) = new sos.spooler.Spooler(JavaInvoker(o.spooler))

  trait Arguments extends ModuleArguments
}
