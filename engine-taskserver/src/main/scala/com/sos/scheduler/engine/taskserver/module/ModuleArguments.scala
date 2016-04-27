package com.sos.scheduler.engine.taskserver.module

/**
  * @author Joacim Zschimmer
  */
trait ModuleArguments {
  def moduleFactory: ModuleFactory

  final def newModule(): Module = moduleFactory.newModule(this)
}
