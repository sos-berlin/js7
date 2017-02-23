package com.sos.jobscheduler.taskserver.modules.javamodule

import com.sos.jobscheduler.taskserver.moduleapi.Module
import com.sos.jobscheduler.taskserver.modules.common.CommonArguments
import com.sos.jobscheduler.taskserver.spoolerapi.TypedNamedIDispatches

/**
  * @author Joacim Zschimmer
  */
trait ApiModule extends Module {

  def newJobInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.IJob_impl

  def newMonitorInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.IMonitor_impl

  final def newTask(commonArguments: CommonArguments): ApiProcessTask =
    new ApiProcessTask(this, commonArguments)
}
