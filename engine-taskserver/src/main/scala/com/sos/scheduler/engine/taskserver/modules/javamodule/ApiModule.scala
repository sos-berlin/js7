package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.module.{Module, NamedInvocables}
import sos.spooler.{Job_impl, Monitor_impl}

/**
  * @author Joacim Zschimmer
  */
trait ApiModule extends Module {

  def newJobInstance(namedInvocables: NamedInvocables): Job_impl

  def newMonitorInstance(namedInvocables: NamedInvocables): Monitor_impl
}
