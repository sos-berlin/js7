package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.ModuleArguments
import com.sos.scheduler.engine.taskserver.modules.javamodule.JavaModule._
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches

/**
  * @author Joacim Zschimmer
  */
trait JavaModule extends ApiModule {

  protected def newJobInstance(): sos.spooler.IJob_impl
  protected def newMonitorInstance(): sos.spooler.IMonitor_impl

  final def newJobInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.IJob_impl = {
    val r = newJobInstance()
    initializeSpoolerVariables(r, namedIDispatches)
    r
  }

  final def newMonitorInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.IMonitor_impl = {
    val r = newMonitorInstance()
    initializeSpoolerVariables(r, namedIDispatches)
    r
  }

  private def initializeSpoolerVariables(o: sos.spooler.HasInitializeSpoolerVariables, namedIDispatches: TypedNamedIDispatches): Unit = {
    o.initializeSpoolerVariables(
      spooler_log(namedIDispatches),
      spooler(namedIDispatches),
      spooler_job(namedIDispatches),
      spooler_task(namedIDispatches))
  }
}

object JavaModule {
  def spooler_log(o: TypedNamedIDispatches) = new sos.spooler.Log(JavaInvoker(o.spoolerLog))
  def spooler_task(o: TypedNamedIDispatches) = new sos.spooler.Task(JavaInvoker(o.spoolerTask))
  def spooler_job(o: TypedNamedIDispatches) = new sos.spooler.Job(JavaInvoker(o.spoolerJob))
  def spooler(o: TypedNamedIDispatches) = new sos.spooler.Spooler(JavaInvoker(o.spooler))

  trait Arguments extends ModuleArguments
}
