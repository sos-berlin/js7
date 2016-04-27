package com.sos.scheduler.engine.taskserver.modules.shell

import com.sos.scheduler.engine.taskserver.module.{Module, ModuleArguments, ModuleFactory, ModuleLanguage, RawModuleArguments, Script}

/**
 * @author Joacim Zschimmer
 */
final case class ShellModule(arguments: ShellModule.Arguments) extends Module {
  def script = arguments.script
}

object ShellModule extends ModuleFactory {
  def toModuleArguments = {
    case RawModuleArguments(ModuleLanguage("shell"), None, script, None, None) ⇒ Arguments(script)
  }

  def newModule(arguments: ModuleArguments) = new ShellModule(arguments.asInstanceOf[Arguments])

  final case class Arguments(script: Script) extends ModuleArguments {
    val moduleFactory = ShellModule
  }
}
