package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.taskserver.module.javamodule.ApiModule
import com.sos.scheduler.engine.taskserver.module.{ModuleFactory, NamedInvocables}
import com.sos.scheduler.engine.taskserver.spoolerapi.SpoolerLog
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class MonitorProcessor private(instances: Vector[sos.spooler.Monitor_impl], spoolerLog: SpoolerLog)
extends HasCloser {

  def preTask(): Boolean = instances forall { _.spooler_task_before() }
  
  def postTask(): Unit =
    for (i ← instances.reverseIterator) {
      try i.spooler_task_after()
      catch { case NonFatal(t) ⇒ spoolerLog.error(s"$i: $t") }
    }

  def preStep(): Boolean = instances forall { _.spooler_process_before() }

  def postStep(returnCode: Boolean): Boolean = {
    var r = returnCode
    for (i ← instances.reverseIterator) r = i.spooler_process_after(r)
    r
  }

  def isEmpty = instances.isEmpty
}

object MonitorProcessor {
  def create(moduleFactory: ModuleFactory, monitors: Seq[Monitor], namedInvocables: NamedInvocables) = {
    def newMonitorInstance(monitor: Monitor): sos.spooler.Monitor_impl =
      moduleFactory(monitor.moduleArguments) match {
        case module: ApiModule ⇒ module.newMonitorInstance(namedInvocables)
        case module ⇒ throw new IllegalArgumentException(s"Unsupported language '${module.language}' for a monitor ($module)")
      }
    new MonitorProcessor(monitors.toVector map newMonitorInstance, namedInvocables.spoolerLog)
  }
}
