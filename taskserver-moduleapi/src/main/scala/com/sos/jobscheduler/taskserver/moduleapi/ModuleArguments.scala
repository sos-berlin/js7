package com.sos.jobscheduler.taskserver.moduleapi

/**
  * @author Joacim Zschimmer
  */
trait ModuleArguments {
  def moduleFactory: ModuleFactory

  final def newModule(): Module = moduleFactory.newModule(this)
}
