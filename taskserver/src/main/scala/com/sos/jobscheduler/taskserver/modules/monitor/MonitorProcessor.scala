package com.sos.scheduler.engine.taskserver.modules.monitor

import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.taskserver.modules.monitors.Monitors
import com.sos.scheduler.engine.taskserver.spoolerapi.{SpoolerLog, TypedNamedIDispatches}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
private[modules] final class MonitorProcessor private(instances: Vector[sos.spooler.IMonitor_impl], spoolerLog: SpoolerLog)
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

private[taskserver] object MonitorProcessor {
  def create(monitors: Seq[Monitor], namedIDispatches: TypedNamedIDispatches): MonitorProcessor =
    new MonitorProcessor(
      monitors.toVector map { o ⇒ Monitors.newMonitorInstance(o.moduleArguments, namedIDispatches) },
      namedIDispatches.spoolerLog)
}
