package com.sos.scheduler.engine.taskserver.module.java

import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.taskserver.module.JavaModuleLanguage

/**
 * @author Joacim Zschimmer
 */
final case class StandardJavaModule(className: String) extends JavaModule {

  def moduleLanguage = JavaModuleLanguage

  def newJobInstance() = cast[sos.spooler.Job_impl](newInstance())

  def newMonitorInstance() = cast[sos.spooler.Monitor_impl](newInstance())

  private def newInstance() = Class.forName(className).newInstance()
}
