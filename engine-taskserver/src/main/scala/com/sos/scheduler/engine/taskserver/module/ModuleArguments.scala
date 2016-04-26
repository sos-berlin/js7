package com.sos.scheduler.engine.taskserver.module

/**
  * @author Joacim Zschimmer
  */
trait ModuleArguments {
  def moduleType: ModuleType

  final def newModule(): Module = moduleType.newModule(this)
}
