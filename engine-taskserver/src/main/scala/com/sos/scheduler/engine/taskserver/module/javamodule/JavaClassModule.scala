package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast

/**
 * @author Joacim Zschimmer
 */
trait JavaClassModule extends JavaModule {

  protected def newInstance(): Any

  protected final def newJobInstance() = cast[sos.spooler.Job_impl](newInstance())

  protected final def newMonitorInstance() = cast[sos.spooler.Monitor_impl](newInstance())
}
