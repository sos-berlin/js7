package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.javamodule.JavaModule
import scala.collection.immutable
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class MonitorProcessor(monitors: immutable.Seq[Monitor], namedInvocables: NamedInvocables, jobName: String)
extends HasCloser {

  private val classInstances = (for (monitor ← monitors) yield monitor.module match {
    case module: JavaModule ⇒ module.newMonitorInstance(namedInvocables)
    case module ⇒ throw new IllegalArgumentException(s"Unsupported language '${module.moduleLanguage}' for a monitor ($module)")
  }).toVector

  def preTask(): Boolean = classInstances forall { _.spooler_task_before() }
  
  def postTask(): Unit =
    for (i ← classInstances.reverseIterator) {
      try i.spooler_task_after()
      catch { case NonFatal(t) ⇒ namedInvocables.spoolerLog.error(s"$i: $t") }
    }

  def preStep(): Boolean = classInstances forall { _.spooler_process_before() }

  def postStep(returnCode: Boolean): Boolean = {
    var r = returnCode
    for (i ← classInstances.reverseIterator) r = i.spooler_process_after(r)
    r
  }

  def isEmpty = monitors.isEmpty
}
