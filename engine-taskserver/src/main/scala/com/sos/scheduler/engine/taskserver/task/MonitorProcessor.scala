package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleArguments
import com.sos.scheduler.engine.taskserver.modules.javamodule.ApiModule
import com.sos.scheduler.engine.taskserver.spoolerapi.{SpoolerLog, TypedNamedIDispatches}
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
  def create(monitors: Seq[Monitor], namedIDispatches: TypedNamedIDispatches) = {
    def newMonitorInstance(args: ModuleArguments): sos.spooler.Monitor_impl =
      args.newModule() match {
        case module: ApiModule ⇒ module.newMonitorInstance(namedIDispatches)
        case module ⇒ throw new IllegalArgumentException(s"Unsupported module class '${module.getClass.getSimpleName}' for a monitor")
      }
    new MonitorProcessor(monitors.toVector map { o ⇒ newMonitorInstance(o.moduleArguments) }, namedIDispatches.spoolerLog)
  }
}
