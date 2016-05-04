package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.minicom.idispatch.Invocable

/**
  * @author Joacim Zschimmer
  */
trait NamedInvocables {
  def spoolerLog: Invocable
  def spoolerTask: Invocable
  def spoolerJob: Invocable
  def spooler: Invocable

  /**
    * Invocable by name.
    *
    * @param name "spooler_log", "spooler_task", "spooler_job" or "spooler"
    * @return `spoolerLog`, `spoolerTask`, `spoolerJob` or `spooler`
    */
  def apply(name: String): Invocable
}

object NamedInvocables {
  val SpoolerLogName = "spooler_log"
  val SpoolerTaskName = "spooler_task"
  val SpoolerJobName = "spooler_job"
  val SpoolerName = "spooler"
  val AllNames = Set(SpoolerLogName, SpoolerTaskName, SpoolerJobName, SpoolerName)
}
