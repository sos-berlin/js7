package com.sos.scheduler.engine.taskserver.moduleapi

/**
  * @author Joacim Zschimmer
  */
trait ModuleArguments {
  def moduleFactory: ModuleFactory

  final def newModule(): Module = moduleFactory.newModule(this)
}
