package com.sos.scheduler.engine.taskserver.module.java

import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.taskserver.module.{JavaModuleLanguage, Module, NamedInvocables}

/**
 * @author Joacim Zschimmer
 */
final case class JavaModule(newClassInstance: () ⇒ Any) extends Module {

  def moduleLanguage = JavaModuleLanguage

  def newJobInstance(namedInvocables: NamedInvocables): sos.spooler.Job_impl = {
    val r = cast[sos.spooler.Job_impl](newClassInstance())
    r.spooler_log = new sos.spooler.Log(JavaInvoker(namedInvocables.spoolerLog))
    r.spooler_task = new sos.spooler.Task(JavaInvoker(namedInvocables.spoolerTask))
    r.spooler_job = new sos.spooler.Job(JavaInvoker(namedInvocables.spoolerJob))
    r.spooler = new sos.spooler.Spooler(JavaInvoker(namedInvocables.spooler))
    r
  }

  def newMonitorInstance(namedInvocables: NamedInvocables): sos.spooler.Monitor_impl = {
    val r = cast[sos.spooler.Monitor_impl](newClassInstance())
    r.spooler_log = new sos.spooler.Log(JavaInvoker(namedInvocables.spoolerLog))
    r.spooler_task = new sos.spooler.Task(JavaInvoker(namedInvocables.spoolerTask))
    r.spooler_job = new sos.spooler.Job(JavaInvoker(namedInvocables.spoolerJob))
    r.spooler = new sos.spooler.Spooler(JavaInvoker(namedInvocables.spooler))
    r
  }
}
