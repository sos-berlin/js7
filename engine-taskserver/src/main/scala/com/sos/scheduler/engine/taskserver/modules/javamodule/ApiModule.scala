package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.{Module, NamedIDispatches}
import sos.spooler.{Job_impl, Monitor_impl}

/**
  * @author Joacim Zschimmer
  */
trait ApiModule extends Module {

  def newJobInstance(namedIDispatches: NamedIDispatches): Job_impl

  def newMonitorInstance(namedIDispatches: NamedIDispatches): Monitor_impl
}
