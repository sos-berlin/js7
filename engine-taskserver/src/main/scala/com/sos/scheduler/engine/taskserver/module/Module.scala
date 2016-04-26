package com.sos.scheduler.engine.taskserver.module

/**
 * @author Joacim Zschimmer
 */
trait Module {
  def arguments: ModuleArguments

  final def language = arguments.language
}
