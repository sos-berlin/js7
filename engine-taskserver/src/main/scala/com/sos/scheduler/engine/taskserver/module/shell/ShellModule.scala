package com.sos.scheduler.engine.taskserver.module.shell

import com.sos.scheduler.engine.taskserver.module.{Module, Script, ShellModuleLanguage}

/**
 * @author Joacim Zschimmer
 */
final case class ShellModule(script: Script) extends Module {
  def moduleLanguage = ShellModuleLanguage
}
