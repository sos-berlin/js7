package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.ModuleArguments
import com.sos.scheduler.engine.taskserver.modules.javamodule.JavaModule._
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches

/**
  * @author Joacim Zschimmer
  */
trait JavaModule extends ApiModule {

  protected def newJobInstance(): sos.spooler.Job_impl
  protected def newMonitorInstance(): sos.spooler.Monitor_impl

  final def newJobInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.Job_impl = {
    val r = newJobInstance()
    r.spooler_log = spooler_log(namedIDispatches)
    r.spooler_task = spooler_task(namedIDispatches)
    r.spooler_job = spooler_job(namedIDispatches)
    r.spooler = spooler(namedIDispatches)
    r
  }

  final def newMonitorInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.Monitor_impl = {
    val r = newMonitorInstance()
    r.spooler_log = spooler_log(namedIDispatches)
    r.spooler_task = spooler_task(namedIDispatches)
    r.spooler_job = spooler_job(namedIDispatches)
    r.spooler = spooler(namedIDispatches)
    r
  }
}

object JavaModule {
  def spooler_log(o: TypedNamedIDispatches) = new sos.spooler.Log(JavaInvoker(o.spoolerLog))
  def spooler_task(o: TypedNamedIDispatches) = new sos.spooler.Task(JavaInvoker(o.spoolerTask))
  def spooler_job(o: TypedNamedIDispatches) = new sos.spooler.Job(JavaInvoker(o.spoolerJob))
  def spooler(o: TypedNamedIDispatches) = new sos.spooler.Spooler(JavaInvoker(o.spooler))

  trait Arguments extends ModuleArguments
}
