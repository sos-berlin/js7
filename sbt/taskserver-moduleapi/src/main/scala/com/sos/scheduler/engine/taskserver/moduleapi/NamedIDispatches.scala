package com.sos.scheduler.engine.taskserver.moduleapi

import com.sos.scheduler.engine.minicom.idispatch.IDispatch

/**
  * @author Joacim Zschimmer
  */
trait NamedIDispatches {
  def spoolerLog: IDispatch
  def spoolerTask: IDispatch
  def spoolerJob: IDispatch
  def spooler: IDispatch

  /**
    * IDispatch by name.
    *
    * @param name "spooler_log", "spooler_task", "spooler_job" or "spooler"
    * @return `spoolerLog`, `spoolerTask`, `spoolerJob` or `spooler`
    */
  def apply(name: String): IDispatch
}

object NamedIDispatches {
  val SpoolerLogName = "spooler_log"
  val SpoolerTaskName = "spooler_task"
  val SpoolerJobName = "spooler_job"
  val SpoolerName = "spooler"
  val AllNames = Set(SpoolerLogName, SpoolerTaskName, SpoolerJobName, SpoolerName)
}
