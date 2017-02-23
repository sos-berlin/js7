package com.sos.jobscheduler.taskserver.modules.monitors

import com.sos.jobscheduler.taskserver.moduleapi.ModuleArguments
import com.sos.jobscheduler.taskserver.modules.javamodule.ApiModule
import com.sos.jobscheduler.taskserver.spoolerapi.TypedNamedIDispatches

/**
  * @author Joacim Zschimmer
  */
object Monitors {
  def newMonitorInstance(args: ModuleArguments, namedIDispatches: TypedNamedIDispatches): sos.spooler.IMonitor_impl =
    args.newModule() match {
      case module: ApiModule ⇒ module.newMonitorInstance(namedIDispatches)
      case module ⇒ throw new IllegalArgumentException(s"Unsupported module class '${module.getClass.getSimpleName}' for a monitor")
    }
}
