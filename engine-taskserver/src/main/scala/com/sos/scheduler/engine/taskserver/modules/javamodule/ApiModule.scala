package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.Module
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches
import sos.spooler.{Job_impl, Monitor_impl}

/**
  * @author Joacim Zschimmer
  */
trait ApiModule extends Module {

  def newJobInstance(namedIDispatches: TypedNamedIDispatches): Job_impl

  def newMonitorInstance(namedIDispatches: TypedNamedIDispatches): Monitor_impl
}
