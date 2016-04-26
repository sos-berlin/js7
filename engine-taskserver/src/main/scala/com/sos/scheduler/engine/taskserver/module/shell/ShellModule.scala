package com.sos.scheduler.engine.taskserver.module.shell

import com.sos.scheduler.engine.taskserver.module.Module
import com.sos.scheduler.engine.taskserver.module.ModuleArguments.ShellModuleArguments

/**
 * @author Joacim Zschimmer
 */
final class ShellModule(val arguments: ShellModuleArguments) extends Module {
  def script = arguments.script
}
