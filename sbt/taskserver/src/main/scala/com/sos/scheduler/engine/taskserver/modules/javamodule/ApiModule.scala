package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.Module
import com.sos.scheduler.engine.taskserver.modules.common.CommonArguments
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches

/**
  * @author Joacim Zschimmer
  */
trait ApiModule extends Module {

  def newJobInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.IJob_impl

  def newMonitorInstance(namedIDispatches: TypedNamedIDispatches): sos.spooler.IMonitor_impl

  final def newTask(commonArguments: CommonArguments): ApiProcessTask =
    new ApiProcessTask(this, commonArguments)
}
