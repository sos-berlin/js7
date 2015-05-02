package com.sos.scheduler.engine.taskserver.module.java

import com.sos.scheduler.engine.taskserver.module.java.JavaModule._
import com.sos.scheduler.engine.taskserver.module.{Module, NamedInvocables}

/**
 * @author Joacim Zschimmer
 */
trait JavaModule extends Module {

  def newJobInstance(): sos.spooler.Job_impl
  def newMonitorInstance(): sos.spooler.Monitor_impl

  final def newJobInstance(namedInvocables: NamedInvocables): sos.spooler.Job_impl = {
    val r = newJobInstance()
    r.spooler_log = spooler_log(namedInvocables)
    r.spooler_task = spooler_task(namedInvocables)
    r.spooler_job = spooler_job(namedInvocables)
    r.spooler = spooler(namedInvocables)
    r
  }

  final def newMonitorInstance(namedInvocables: NamedInvocables): sos.spooler.Monitor_impl = {
    val r = newMonitorInstance()
    r.spooler_log = spooler_log(namedInvocables)
    r.spooler_task = spooler_task(namedInvocables)
    r.spooler_job = spooler_job(namedInvocables)
    r.spooler = spooler(namedInvocables)
    r
  }
}

object JavaModule {
//val SpoolerInitSignature = "spooler_open()Z"  // Z: returns Boolean
  val SpoolerExitSignature = "spooler_exit()V"  // V: returns Unit
  val SpoolerOpenSignature = "spooler_open()Z"
//val SpoolerCloseSignature = "spooler_open()V"
//val SpoolerProcessSignature = "spooler_open()Z"
  val SpoolerOnSuccessSignature = "spooler_on_success()V"
  val SpoolerOnErrorSignature = "spooler_on_error()V"

  private def spooler_log(o: NamedInvocables) = new sos.spooler.Log(JavaInvoker(o.spoolerLog))
  private def spooler_task(o: NamedInvocables) = new sos.spooler.Task(JavaInvoker(o.spoolerTask))
  private def spooler_job(o: NamedInvocables) = new sos.spooler.Job(JavaInvoker(o.spoolerJob))
  private def spooler(o: NamedInvocables) = new sos.spooler.Spooler(JavaInvoker(o.spooler))
}
