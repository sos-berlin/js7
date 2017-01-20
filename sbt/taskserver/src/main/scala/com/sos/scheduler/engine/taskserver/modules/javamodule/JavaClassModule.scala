package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.base.utils.ScalaUtils.cast

/**
  * For a Java class which may be a [[sos.spooler.IJob_impl]] or [[sos.spooler.IMonitor_impl]].
  *
  * @author Joacim Zschimmer
  */
trait JavaClassModule extends JavaModule {

  protected def newInstance(): Any

  protected final def newJobInstance() = cast[sos.spooler.IJob_impl](newInstance())

  protected final def newMonitorInstance() = cast[sos.spooler.IMonitor_impl](newInstance())
}
