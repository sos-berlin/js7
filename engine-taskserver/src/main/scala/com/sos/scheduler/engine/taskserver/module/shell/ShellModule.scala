package com.sos.scheduler.engine.taskserver.module.shell

import com.sos.scheduler.engine.taskserver.module.{Module, ModuleArguments, ModuleType, RawModuleArguments, Script, ShellModuleLanguage}

/**
 * @author Joacim Zschimmer
 */
final case class ShellModule(arguments: ShellModule.Arguments) extends Module {
  def script = arguments.script
}

object ShellModule extends ModuleType {
  def toModuleArguments = {
    case args @ RawModuleArguments(ShellModuleLanguage, javaClassNameOption, script, None, None) ⇒
      args.requireUnused("java_class", javaClassNameOption)
      Arguments(script)
  }

  def newModule(arguments: ModuleArguments) = new ShellModule(arguments.asInstanceOf[Arguments])

  final case class Arguments(script: Script) extends ModuleArguments {
    val moduleType = ShellModule
  }
}
